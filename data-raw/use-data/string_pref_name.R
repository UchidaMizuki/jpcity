
# string_pref_name --------------------------------------------------------

get_string_pref_name <- function(graph_city) {
  graph_city$nodes_city |>
    mutate(pref_code = city_code |>
             str_extract("^\\d{2}") |>
             as.integer()) |>
    distinct(pref_code, pref_name) |>
    mutate(string_pref_name = extract_pref_name(pref_name))
}
