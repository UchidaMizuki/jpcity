source("data-raw/setup.R")

# string_pref_name --------------------------------------------------------

string_pref_name <- nodes_city |>
  mutate(pref_code = city_code |>
           str_extract("^\\d{2}") |>
           as.integer()) |>
  distinct(pref_code, pref_name) |>
  mutate(string_pref_name = extract_pref_name(pref_name))
