# [DEPRECATED] Legacy pipeline -- superseded by data-raw/lod/.
#
# Scrapes the e-Stat area-code CSVs at two points in time and reconstructs the
# municipality history by joining them with a separately scraped table of
# mergers and splits. Kept only so the current R/sysdata.rda (graph_city_legacy /
# city_desig_code_legacy / string_pref_name_legacy) remains reproducible until the
# R layer has migrated to the LOD-based objects. Do not extend this; see
# data-raw/README.md.

source("data-raw/setup.R")

# update-data -------------------------------------------------------------

date_start <- ymd("1970-04-01", tz = tz_jst_legacy)
date_end <- now(tzone = tz_jst_legacy) |>
  floor_date("day")

interval_city <- date_start %--% date_end

# areacode ----------------------------------------------------------------

source("data-raw/legacy/update-data/areacode.R")

areacode_start <- get_areacode(date = date_start)
areacode_end <- get_areacode(date = date_end)

# absorption_separation ---------------------------------------------------

source("data-raw/legacy/update-data/absorption_separation.R")

absorption_separation <- get_absorption_separation(
  date_start = date_start,
  date_end = date_end
)

# okinawa_reversion

# https://ja.wikipedia.org/wiki/%E6%B2%96%E7%B8%84%E8%BF%94%E9%82%84
date_okinawa_reversion <- ymd("1972-06-07", tz = tz_jst_legacy)

absorption_separation_okinawa_reversion <- get_areacode(
  date = date_okinawa_reversion
) |>
  filter(pref_name == "沖縄県") |>
  rename(date = date_absorption_separation) |>
  select(!absorption_separation)
absorption_separation_okinawa_reversion <- tibble(
  date = absorption_separation_okinawa_reversion$date,
  type = "沖縄返還",
  from = vec_init(absorption_separation_okinawa_reversion),
  to = absorption_separation_okinawa_reversion
)

# northern_territories

date_northern_territories <- ymd("1983-04-01", tz = tz_jst_legacy)

absorption_separation_northern_territories <- get_areacode(
  date = date_northern_territories
) |>
  filter(pref_name == "北海道", as.integer(city_code) %in% 1695:1700) |>
  rename(date = date_absorption_separation) |>
  select(!absorption_separation)
absorption_separation_northern_territories <- tibble(
  date = absorption_separation_northern_territories$date,
  type = "北方領土",
  from = vec_init(absorption_separation_northern_territories),
  to = absorption_separation_northern_territories
)

absorption_separation <- bind_rows(
  absorption_separation,
  absorption_separation_okinawa_reversion,
  absorption_separation_northern_territories
) |>
  arrange(date, type)

# write_rds ---------------------------------------------------------------

# interval_city
write_rds(interval_city, "data-raw/legacy/data/interval_city.rds")

# area_code
write_rds(areacode_start, "data-raw/legacy/data/areacode_start.rds")
write_rds(areacode_end, "data-raw/legacy/data/areacode_end.rds")

# absorption_separation
write_rds(
  absorption_separation,
  "data-raw/legacy/data/absorption_separation.rds"
)
