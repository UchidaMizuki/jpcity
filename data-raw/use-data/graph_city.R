
# graph_city --------------------------------------------------------------

col_names_city <- c("city_code", "pref_name",
                    "city_desig_name", "city_desig_name_kana",
                    "city_name", "city_name_kana")

get_graph_city <- function(interval_city, areacode_start, areacode_end, absorption_separation) {
  # Anamizu Town (17421) and Monzen Town (17422) in Hosi-gun were changed to Anamizu Town (17461) and Monzen Town (17462) in Hozu-gun.
  absorption_separation <- absorption_separation |>
    filter(date != "2005-03-01" | is.na(from$city_code) | from$city_code != "17421" | to$city_code == "17461",
           date != "2005-03-01" | is.na(from$city_code) | from$city_code != "17422" | to$city_code == "17462")

  areacode_start <- tidy_city(areacode_start) |>
    vec_unique() |>
    filter(is.na(pref_name) | !is.na(city_desig_name) | !is.na(city_name))
  areacode_end <- tidy_city(areacode_end) |>
    vec_unique() |>
    filter(is.na(pref_name) | !is.na(city_desig_name) | !is.na(city_name))
  absorption_separation <- absorption_separation |>
    mutate(across(c(from, to),
                  tidy_city)) |>
    vec_unique() |>
    filter(is.na(from$pref_name) | !is.na(from$city_desig_name) | !is.na(from$city_name),
           is.na(to$pref_name) | !is.na(to$city_desig_name) | !is.na(to$city_name))

  nodes_city <- bind_rows(vec_init(areacode_start),
                          areacode_start,
                          absorption_separation$from,
                          absorption_separation$to) |>
    distinct(city_code, pref_name, city_desig_name, city_desig_name_kana, city_name, city_name_kana) |>
    rowid_to_column("node")

  edges_city <- bind_rows(areacode_start |>
                            left_join(nodes_city |>
                                        rename(to = node),
                                      by = col_names_city) |>
                            add_column(date = int_start(interval_city),
                                       type = "start",
                                       from = 1L),
                          absorption_separation |>
                            mutate(from = from |>
                                     left_join(nodes_city,
                                               by = col_names_city) |>
                                     pull(node),
                                   to = to |>
                                     left_join(nodes_city,
                                               by = col_names_city) |>
                                     pull(node)),
                          areacode_end |>
                            left_join(nodes_city |>
                                        rename(from = node),
                                      by = col_names_city) |>
                            add_column(date = as.POSIXct(Inf), # int_end(interval_city) + days(1L),
                                       to = 1L)) |>
    select(date, type, from, to) |>

    # Because links between the same node create bugs in ancestor/ancestor calculations
    filter(from != to)

  graph_city <- tbl_graph(nodes = nodes_city |>
                            select(!node),
                          edges = edges_city)

  stopifnot(
    graph_city |>
      filter(node_is_sink()) |>
      activate(nodes) |>
      as_tibble() |>
      vec_is_empty(),
    graph_city |>
      filter(node_is_source()) |>
      activate(nodes) |>
      as_tibble() |>
      vec_is_empty()
  )

  edges_city <- graph_city |>
    activate(edges) |>
    as_tibble()
  date_start <- edges_city |>
    summarise(date_start = min(date),
              .by = to)
  date_end <- edges_city |>
    summarise(date_end = max(date) - days(1L),
              .by = from)

  graph_city <- graph_city |>
    mutate(node = row_number()) |>
    left_join(date_start,
              by = join_by(node == to)) |>
    left_join(date_end,
              by = join_by(node == from)) |>
    select(!node) |>
    mutate(interval = if_else(is.na(city_code),
                              interval(NA_Date_, NA_Date_,
                                       tzone = tz_jst),
                              date_start %--% date_end)) |>
    select(!c(date_start, date_end)) |>
    filter(!is.na(city_code))

  stopifnot(
    !graph_city |>
      activate(nodes) |>
      as_tibble() |>
      select(city_code, interval) |>
      vec_duplicate_any()
  )

  nodes_city <- graph_city |>
    activate("nodes") |>
    as_tibble() |>
    rowid_to_column("node")

  size_nodes_city <- vec_size(nodes_city)

  nodes_city <- nodes_city |>
    mutate(ancestors = node |>
             map(\(node) {
               ancestors <- graph_city |>
                 convert(to_local_neighborhood,
                         node = node,
                         order = size_nodes_city,
                         mode = "in")
               nodes_ancestors <- ancestors |>
                 activate(nodes) |>
                 as_tibble()
               edges_ancestors <- ancestors |>
                 activate(edges) |>
                 as_tibble()
               if (vec_duplicate_any(edges_ancestors$from)) {
                 edges_ancestors <- edges_ancestors |>
                   summarise(date = max(date),
                             .by = from)
               }
               int_end(vec_slice(nodes_ancestors$interval, edges_ancestors$from)) <- edges_ancestors$date - days(1L)

               nodes_ancestors |>
                 rename(node_relatives = .tidygraph_node_index) |>
                 select(interval, node_relatives)
             },
             .progress = TRUE),
           descendants = node |>
             map(\(node) {
               descendants <- graph_city |>
                 convert(to_local_neighborhood,
                         node = node,
                         order = size_nodes_city,
                         mode = "out")
               nodes_descendants <- descendants |>
                 activate(nodes) |>
                 as_tibble()
               edges_descendants <- descendants |>
                 activate(edges) |>
                 as_tibble()
               if (vec_duplicate_any(edges_descendants$to)) {
                 edges_descendants <- edges_descendants |>
                   summarise(date = min(date),
                             .by = to)
               }
               int_start(vec_slice(nodes_descendants$interval, edges_descendants$to)) <- edges_descendants$date

               nodes_descendants |>
                 rename(node_relatives = .tidygraph_node_index) |>
                 select(interval, node_relatives)
             },
             .progress = TRUE))

  ancestors_city <- nodes_city |>
    select(node, ancestors) |>
    unnest(ancestors)

  descendants_city <- nodes_city |>
    select(node, descendants) |>
    unnest(descendants)

  interval_city_code <- nodes_city |>
    summarise(interval = min(int_start(interval)) %--% max(int_end(interval)),
              .by = city_code)

  nodes_city <- nodes_city |>
    select(!c(ancestors, descendants)) |>
    mutate(across(c(city_desig_name, city_desig_name_kana, city_name, city_name_kana),
                  \(x) replace_na(x, "")))

  list(graph_city = graph_city,
       interval_city = interval_city,
       nodes_city = nodes_city,
       ancestors_city = ancestors_city,
       descendants_city = descendants_city,
       interval_city_code = interval_city_code)
}

tidy_city <- function(data) {
  data |>
    mutate(city_desig_name = if_else(str_ends(city_desig_name, "市"),
                                     city_desig_name,
                                     NA_character_),
           city_desig_name_kana = if_else(str_ends(city_desig_name, "市"),
                                          city_desig_name_kana,
                                          NA_character_),
           city_name_kana = case_match(city_code,
                                       "02445" ~ "なんぶちょう",
                                       "41401" ~ "ありたちょう",
                                       .default = city_name_kana)) |>
    select(!!!col_names_city)
}
