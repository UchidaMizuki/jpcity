# graph_area --------------------------------------------------------------

# e-Stat LOD issues a brand-new period-tagged resource at every standard-area-
# code re-issue (roughly every 5 years), even when nothing changed for a given
# municipality. `succeedingCode` / `previousCode` link those same-area versions,
# while `succeedingMunicipality` / `previousMunicipality` record real mergers and
# splits. We therefore collapse each `code_chain` connected component into one
# logical entity (matching the old `(code, interval)` granularity of
# `graph_city$nodes_city`) and treat municipality succession as the graph edges.
#
# Per-version names (renames without a code change) are preserved on the `dated`
# objects via the node table; the entity (`undated`) keeps the most recent name.

sac_uri_code <- function(uri) {
  str_extract(uri, "(?<=/sac/C)\\d{5}")
}

get_area_data <- function(standard_area_code) {
  nodes <- standard_area_code$nodes
  code_chain <- standard_area_code$code_chain
  succession <- standard_area_code$succession
  part_of <- standard_area_code$part_of

  # Connected components over the same-area code chain -> logical entities.
  graph_chain <- igraph::graph_from_data_frame(
    code_chain[c("from", "to")],
    directed = FALSE,
    vertices = nodes["id"]
  )
  nodes <- nodes |>
    left_join(
      tibble(
        id = igraph::V(graph_chain)$name,
        entity = igraph::components(graph_chain)$membership
      ),
      by = "id"
    )

  # Entity (undated) attributes: one code, contiguous interval, latest name.
  #
  # `valid` is the *next* code's issue date, i.e. an exclusive end, whereas
  # lubridate intervals include their end instant (`%within%`). Step back a day
  # so the last day the area existed is the interval end.
  area_master <- nodes |>
    arrange(entity, issued) |>
    summarise(
      code = vec_unique(code) |> first(),
      admin_class = last(admin_class[!is.na(admin_class)]),
      pref_name = last(pref_name[!is.na(pref_name)]),
      name = last(name[!is.na(name)]),
      name_kana = last(name_kana[!is.na(name_kana)]),
      name_en = last(name_en[!is.na(name_en)]),
      check_digit = last(check_digit[!is.na(check_digit)]),
      date_start = min(issued),
      date_end = if (any(is.na(valid))) NA_Date_ else max(valid) - days(1L),
      .by = entity
    ) |>
    filter(!is.na(code)) |>
    arrange(code, date_start)

  # Resolve the (unique) parent entity code.
  parent <- part_of |>
    transmute(id, parent_code = sac_uri_code(part_of)) |>
    left_join(nodes[c("id", "entity")], by = "id") |>
    distinct(entity, parent_code) |>
    filter(!is.na(parent_code)) |>
    summarise(parent_code = first(parent_code), .by = entity)

  area_master <- area_master |>
    left_join(
      nodes |>
        distinct(entity) |>
        left_join(parent, by = "entity"),
      by = "entity"
    ) |>
    mutate(
      interval = date_start %--%
        coalesce(date_end, as.POSIXct(Inf, tz = tz_jst_legacy)),
      node = row_number()
    )

  # Entity-level succession edges, dated by the successor version's issue date.
  entity_of <- nodes |>
    select(id, entity)
  issued_of <- nodes |>
    distinct(id, issued)

  edges <- succession |>
    left_join(entity_of, by = c("from" = "id")) |>
    rename(from_entity = entity) |>
    left_join(entity_of, by = c("to" = "id")) |>
    rename(to_entity = entity) |>
    left_join(issued_of, by = c("to" = "id")) |>
    filter(!is.na(from_entity), !is.na(to_entity), from_entity != to_entity) |>
    summarise(date = min(issued), .by = c(from_entity, to_entity)) |>
    left_join(
      area_master |> select(from_entity = entity, from = node),
      by = "from_entity"
    ) |>
    left_join(
      area_master |> select(to_entity = entity, to = node),
      by = "to_entity"
    ) |>
    # tbl_graph() reads the first two edge columns as from/to.
    select(from, to, date)

  # Drop the `node` helper column from the graph: otherwise tidygraph data-masks
  # the `node =` argument of `to_local_neighborhood()` to this column vector
  # instead of the scalar index passed in below.
  graph_area <- tbl_graph(nodes = area_master |> select(!node), edges = edges)

  size_nodes <- vec_size(area_master)

  area_master <- area_master |>
    mutate(
      ancestors = node |>
        map(
          \(node) local_relatives(graph_area, node, size_nodes, "in"),
          .progress = TRUE
        ),
      descendants = node |>
        map(
          \(node) local_relatives(graph_area, node, size_nodes, "out"),
          .progress = TRUE
        )
    )

  ancestors_area <- area_master |>
    select(node, ancestors) |>
    unnest(ancestors)
  descendants_area <- area_master |>
    select(node, descendants) |>
    unnest(descendants)

  interval_code <- area_master |>
    summarise(
      interval = min(int_start(interval)) %--% max(int_end(interval)),
      .by = code
    )

  nodes_area <- area_master |>
    select(
      node,
      entity,
      code,
      admin_class,
      pref_name,
      parent_code,
      name,
      name_kana,
      name_en,
      check_digit,
      interval
    )

  list(
    nodes = nodes_area,
    dated = nodes,
    ancestors = ancestors_area,
    descendants = descendants_area,
    interval_code = interval_code
  )
}

# Neighborhood with intervals truncated at the succession edge dates
# (ported from data-raw/use-data/graph_city.R).
local_relatives <- function(graph_area, node, order, mode) {
  sub <- graph_area |>
    convert(to_local_neighborhood, node = node, order = order, mode = mode)
  nodes_sub <- sub |>
    activate(nodes) |>
    as_tibble()
  edges_sub <- sub |>
    activate(edges) |>
    as_tibble()

  if (mode == "in") {
    if (vec_duplicate_any(edges_sub$from)) {
      edges_sub <- edges_sub |>
        summarise(date = max(date), .by = from)
    }
    int_end(vec_slice(nodes_sub$interval, edges_sub$from)) <- edges_sub$date -
      days(1L)
  } else {
    if (vec_duplicate_any(edges_sub$to)) {
      edges_sub <- edges_sub |>
        summarise(date = min(date), .by = to)
    }
    int_start(vec_slice(nodes_sub$interval, edges_sub$to)) <- edges_sub$date
  }

  nodes_sub |>
    rename(node_relatives = .tidygraph_node_index) |>
    select(interval, node_relatives)
}
