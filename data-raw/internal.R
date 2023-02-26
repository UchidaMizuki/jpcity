source("data-raw/setup.R")
source("data-raw/graph_city.R")
source("data-raw/city_desig_code.R")

# internal ----------------------------------------------------------------

usethis::use_data(graph_city, interval_graph_city,
                  city_desig_code,
                  internal = TRUE,
                  overwrite = TRUE)
