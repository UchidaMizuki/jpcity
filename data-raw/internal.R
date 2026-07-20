source("data-raw/lod/use-data.R")

# The legacy pipeline is frozen and expensive to re-run (see
# data-raw/legacy/use-data.R), so its result is read from a cache instead of
# sourcing that script here. Re-run data-raw/legacy/use-data.R by hand to
# refresh the cache if the legacy raw data ever changes.
legacy <- readr::read_rds("data-raw/legacy/data/legacy.rds")
graph_city_legacy <- legacy$graph_city_legacy
city_desig_code_legacy <- legacy$city_desig_code_legacy
string_pref_name_legacy <- legacy$string_pref_name_legacy

usethis::use_data(
  # lod

  # legacy
  graph_city_legacy,
  city_desig_code_legacy,
  string_pref_name_legacy,
  internal = TRUE,
  overwrite = TRUE
)
