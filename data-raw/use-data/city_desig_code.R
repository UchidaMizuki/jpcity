
# city_desig_code ---------------------------------------------------------

get_city_desig_code <- function(graph_city) {
  city_code <- graph_city$nodes_city |>
    select(!node) |>
    mutate(city_desig_name = if_else(str_starts(city_code, "131"),
                                     "特別区部",
                                     city_desig_name),
           city_desig_name_kana = if_else(str_starts(city_code, "131"),
                                          "とくべつくぶ",
                                          city_desig_name_kana))

  city_desig_code <- city_code |>
    filter(city_code == "13100" | city_desig_name != "" & city_name == "") |>
    select(!c(city_name, city_name_kana))

  city_code |>
    inner_join(city_desig_code |>
                 rename(city_desig_code = city_code,
                        interval_desig = interval),
               by = join_by(pref_name, city_desig_name, city_desig_name_kana)) |>
    select(city_code, city_desig_code, interval, interval_desig) |>
    arrange(city_code)
}
