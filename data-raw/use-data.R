source("data-raw/setup.R")

# use-data ----------------------------------------------------------------

# source("data-raw/update-data.R")
interval_city <- read_rds("data-raw/interval_city.rds")
areacode_start <- read_rds("data-raw/areacode_start.rds")
areacode_end <- read_rds("data-raw/areacode_end.rds")
absorption_separation <- read_rds("data-raw/absorption_separation.rds")

# graph_city --------------------------------------------------------------

source("data-raw/use-data/graph_city.R")

graph_city <- get_graph_city(interval_city = interval_city,
                             areacode_start = areacode_start,
                             areacode_end = areacode_end,
                             absorption_separation = absorption_separation)

# city_desig_code ---------------------------------------------------------

source("data-raw/use-data/city_desig_code.R")

city_desig_code <- get_city_desig_code(graph_city = graph_city)

# string_pref_name --------------------------------------------------------

source("data-raw/use-data/string_pref_name.R")

string_pref_name <- get_string_pref_name(graph_city = graph_city)

# internal ----------------------------------------------------------------

usethis::use_data(graph_city,
                  city_desig_code,
                  string_pref_name,
                  internal = TRUE,
                  overwrite = TRUE)
