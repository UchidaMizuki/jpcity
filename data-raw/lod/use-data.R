# Build step of the LOD pipeline (see data-raw/README.md).
#
# Reads the cached SPARQL response, normalizes it, and builds the internal
# master + succession graph. Re-runnable offline.

source("data-raw/setup.R")

# use-data-lod ------------------------------------------------------------

# source("data-raw/lod/update-data.R")

StandardAreaCode <- read_rds("data-raw/lod/data/StandardAreaCode.rds")

# Parse the SPARQL JSON bindings ------------------------------------------

# Each binding (row) is a named list of cells shaped like
# `list(type = ..., value = ..., "xml:lang" = ..., datatype = ...)`. A single
# period-tagged code (the `CODE` URI, e.g. `.../sac/C01101-19720401`) appears in
# several bindings: one per `rdfs:label` language and per multi-valued relation
# (hasPart, succeedingMunicipality, ...). We therefore flatten every binding to
# a long table first, then split it into normalized node / relation tables.

cell_value <- function(cell) {
  if (is.null(cell)) NA_character_ else cell[["value"]]
}
cell_lang <- function(cell) {
  if (is.null(cell)) NA_character_ else cell[["xml:lang"]] %||% NA_character_
}

bindings_long <- StandardAreaCode$results$bindings |>
  map(\(row) {
    tibble(
      id = cell_value(row$CODE),
      code = cell_value(row$IDENTIFIER),
      issued = cell_value(row$ISSUED),
      valid = cell_value(row$VALID),
      admin_class = cell_value(row$ADMINISTRATIVECLASS),
      pref_name = cell_value(row$PREFECTURELABEL),
      check_digit = cell_value(row$CHECKDIGIT),
      part_of = cell_value(row$ISPARTOF),
      has_part = cell_value(row$HASPART),
      previous_municipality = cell_value(row$PREVIOUSMUNICIPALITY),
      succeeding_municipality = cell_value(row$SUCCEEDINGMUNICIPALITY),
      previous_code = cell_value(row$PREVIOUSCODE),
      succeeding_code = cell_value(row$SUCCEEDINGCODE),
      label = cell_value(row$LABEL),
      label_lang = cell_lang(row$LABEL)
    )
  }) |>
  list_rbind()

# Labels by language: ja = kanji, ja-hrkt = kana, en = English -------------

labels <- bindings_long |>
  filter(!is.na(label)) |>
  distinct(id, label_lang, label) |>
  mutate(
    label_lang = recode(
      label_lang,
      "ja" = "name",
      "ja-hrkt" = "name_kana",
      "en" = "name_en"
    )
  ) |>
  filter(label_lang %in% c("name", "name_kana", "name_en")) |>
  # Guard against the rare duplicate (id, lang) pair.
  summarise(label = first(label), .by = c(id, label_lang)) |>
  pivot_wider(names_from = label_lang, values_from = label)

# Nodes: one row per period-tagged code (single-valued attributes) ----------

# `isPartOf` is multi-valued (a resource links to every period version of its
# parent), so it is kept only in the relation table `sac_part_of` and resolved
# by interval overlap later -- not collapsed into a single node column.
sac_nodes <- bindings_long |>
  summarise(
    across(c(code, issued, valid, admin_class, pref_name, check_digit), \(x) {
      first(x[!is.na(x)], default = NA_character_)
    }),
    .by = id
  ) |>
  mutate(
    admin_class = str_remove(admin_class, "^.*#"),
    issued = ymd(issued, tz = tz_jst_legacy),
    valid = ymd(valid, tz = tz_jst_legacy)
  ) |>
  left_join(labels, by = "id") |>
  arrange(code, issued)

# Relation tables (long, keyed by node id) ---------------------------------

# Municipality succession (merge / split) edges.
sac_succession <- bindings_long |>
  distinct(from = id, to = succeeding_municipality) |>
  filter(!is.na(to))

# Parent / child structure (e.g. ward -> designated city, town -> district).
sac_part_of <- bindings_long |>
  distinct(id, part_of) |>
  filter(!is.na(part_of))

# Same-area code-change chain.
sac_code_chain <- bindings_long |>
  distinct(from = id, to = succeeding_code) |>
  filter(!is.na(to))

standard_area_code <- list(
  nodes = sac_nodes,
  succession = sac_succession,
  part_of = sac_part_of,
  code_chain = sac_code_chain
)

write_rds(standard_area_code, "data-raw/lod/data/standard_area_code.rds")

# area data (master + succession graph + ancestors/descendants) -----------

source("data-raw/lod/use-data/graph_area.R")

area_data <- get_area_data(standard_area_code)
write_rds(area_data, "data-raw/lod/data/area_data.rds")

# NOTE: this script does not call usethis::use_data(internal = TRUE) itself.
# data-raw/internal.R sources this file, reads the cached legacy pipeline
# result, and makes the single use_data(internal = TRUE) call that writes
# R/sysdata.rda (currently `graph_city_legacy` / `city_desig_code_legacy` /
# `string_pref_name_legacy`, read by the `_legacy`-suffixed exported
# functions). `area_data` itself is not yet wired into sysdata.rda; that
# happens once the R layer is migrated to these id-based objects.

area_data
