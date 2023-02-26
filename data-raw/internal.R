source("data-raw/setup.R")

# Use to update data
# source("data-raw/areacode.R")
# source("data-raw/absorption_separation.R")

source("data-raw/graph_city.R")
source("data-raw/city_desig_code.R")

# internal ----------------------------------------------------------------

usethis::use_data(interval_graph_city, graph_city, nodes_city,
                  city_desig_code,
                  internal = TRUE,
                  overwrite = TRUE)
