source("data-raw/setup.R")

# internal ----------------------------------------------------------------

graph_city <- read_rds("data-raw/graph_city.rds")
interval_graph_city <- graph_city$interval
graph_city <- graph_city$graph

city_desig_code <- read_rds("data-raw/city_desig_code.rds")

usethis::use_data(graph_city, interval_graph_city,
                  city_desig_code,
                  internal = TRUE,
                  overwrite = TRUE)
