source("data-raw/setup.R")

# graph_city --------------------------------------------------------------

col_names_city <- c("city_code", "pref_name",
                    "city_desig_name", "city_desig_name_kana",
                    "city_name", "city_name_kana")
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


areacode_start <- read_rds("data-raw/areacode/areacode_start.rds")
areacode_end <- read_rds("data-raw/areacode/areacode_end.rds")
absorption_separation <- read_rds("data-raw/absorption_separation.rds")

interval_graph_city <- absorption_separation$interval

stopifnot(areacode_start$date %--% areacode_end$date == interval_graph_city)

areacode_start <- tidy_city(areacode_start$areacode) |>
  vec_unique() |>
  filter(is.na(pref_name) | !is.na(city_desig_name) | !is.na(city_name))
areacode_end <- tidy_city(areacode_end$areacode) |>
  vec_unique() |>
  filter(is.na(pref_name) | !is.na(city_desig_name) | !is.na(city_name))
absorption_separation <- absorption_separation$absorption_separation |>
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
                          add_column(date = int_start(interval_graph_city),
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
                          add_column(date = int_end(interval_graph_city),
                                     to = 1L)) |>
  select(date, type, from, to)

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
  summarise(date_end = max(date),
            .by = from)

graph_city <- graph_city |>
  mutate(node = row_number()) |>
  left_join(date_start,
            by = join_by(node == to)) |>
  left_join(date_end,
            by = join_by(node == from)) |>
  mutate(interval = if_else(is.na(city_code),
                            NA_Date_ %--% NA_Date_,
                            date_start %--% date_end)) |>
  select(!c(date_start, date_end))

stopifnot(
  !graph_city |>
    activate(nodes) |>
    as_tibble() |>
    select(city_code, interval) |>
    vec_duplicate_any()
)
