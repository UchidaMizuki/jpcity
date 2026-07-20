# [DEPRECATED] Legacy pipeline -- superseded by data-raw/lod/.
#
# The exported (now `_legacy`-suffixed) functions read `graph_city_legacy` /
# `city_desig_code_legacy` / `string_pref_name_legacy`. This pipeline is frozen and
# the ancestor/descendant graph computation below is expensive, so this
# script is run by hand and its result cached to data/legacy.rds;
# data-raw/internal.R reads that cache instead of re-running this script on
# every build. Once the R layer is migrated to the id-based `area_data`
# objects, this file (and the cache) is removed.

source("data-raw/setup.R")

# use-data ----------------------------------------------------------------

# source("data-raw/legacy/update-data.R")
interval_city <- read_rds("data-raw/legacy/data/interval_city.rds")
areacode_start <- read_rds("data-raw/legacy/data/areacode_start.rds")
areacode_end <- read_rds("data-raw/legacy/data/areacode_end.rds")
absorption_separation <- read_rds(
  "data-raw/legacy/data/absorption_separation.rds"
)

# graph_city_legacy ---------------------------------------------------------

source("data-raw/legacy/use-data/graph_city.R")

graph_city_legacy <- get_graph_city(
  interval_city = interval_city,
  areacode_start = areacode_start,
  areacode_end = areacode_end,
  absorption_separation = absorption_separation
)

# city_desig_code_legacy ------------------------------------------------------

source("data-raw/legacy/use-data/city_desig_code.R")

city_desig_code_legacy <- get_city_desig_code(graph_city = graph_city_legacy)

# string_pref_name_legacy --------------------------------------------------

source("data-raw/legacy/use-data/string_pref_name.R")

string_pref_name_legacy <- get_string_pref_name(graph_city = graph_city_legacy)

# cache ---------------------------------------------------------------------

write_rds(
  list(
    graph_city_legacy = graph_city_legacy,
    city_desig_code_legacy = city_desig_code_legacy,
    string_pref_name_legacy = string_pref_name_legacy
  ),
  "data-raw/legacy/data/legacy.rds"
)
