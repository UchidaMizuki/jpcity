
# absorption_separation ---------------------------------------------------

get_absorption_separation <- function(date_start, date_end) {
  url <- "https://www.e-stat.go.jp/municipalities/cities/absorption-separation-of-municipalities?prefecture_all=on&pf%5B1%5D=1&pf%5B2%5D=2&pf%5B3%5D=3&pf%5B4%5D=4&pf%5B5%5D=5&pf%5B6%5D=6&pf%5B7%5D=7&pf%5B8%5D=8&pf%5B9%5D=9&pf%5B10%5D=10&pf%5B11%5D=11&pf%5B12%5D=12&pf%5B13%5D=13&pf%5B14%5D=14&pf%5B15%5D=15&pf%5B16%5D=16&pf%5B17%5D=17&pf%5B18%5D=18&pf%5B19%5D=19&pf%5B20%5D=20&pf%5B21%5D=21&pf%5B22%5D=22&pf%5B23%5D=23&pf%5B24%5D=24&pf%5B25%5D=25&pf%5B26%5D=26&pf%5B27%5D=27&pf%5B28%5D=28&pf%5B29%5D=29&pf%5B30%5D=30&pf%5B31%5D=31&pf%5B32%5D=32&pf%5B33%5D=33&pf%5B34%5D=34&pf%5B35%5D=35&pf%5B36%5D=36&pf%5B37%5D=37&pf%5B38%5D=38&pf%5B39%5D=39&pf%5B40%5D=40&pf%5B41%5D=41&pf%5B42%5D=42&pf%5B43%5D=43&pf%5B44%5D=44&pf%5B45%5D=45&pf%5B46%5D=46&pf%5B47%5D=47&city_nm=&city_kd%5B4%5D=4&city_kd%5B5%5D=5&city_kd%5B2%5D=2&city_kd%5B3%5D=3&city_kd%5B6%5D=6&city_kd%5B7%5D=7&reason_kind_all=on&reason_kd%5B1%5D=1&reason_kd%5B2%5D=2&reason_kd%5B6%5D=6&reason_kd%5B7%5D=7&reason_kd%5B16%5D=16&reason_kd%5B4%5D=4&reason_kd%5B5%5D=5&reason_kd%5B8%5D=8&reason_kd%5B9%5D=9&reason_kd%5B10%5D=10&reason_kd%5B11%5D=11&reason_kd%5B12%5D=12&reason_kd%5B13%5D=13&reason_kd%5B15%5D=15&border_kd=0&keyword_kd=code&item%5B%5D=htCode&item%5B%5D=todoNm&item%5B%5D=parentCityNm&item%5B%5D=parentCityKana&item%5B%5D=selfCityNm&item%5B%5D=selfCityKana&item%5B%5D=htCodeSDate&item%5B%5D=kaiseiJiyu&sort%5B%5D=htCode-asc&sort%5B%5D=htCodeSDate-asc&choices_to_show%5B%5D=jiyuKind&choices_to_show%5B%5D=sityoNaiGunNm&choices_to_show%5B%5D=sityoNaiGunKana&choices_to_show%5B%5D=haitibungoKokujiDate&choices_to_show%5B%5D=haitibungoKokujiNo&choices_to_sort%5B%5D=jiyuKind&choices_to_sort_value%5B%5D=jiyuKind-asc&choices_to_sort_value%5B%5D=jiyuKind-desc&choices_to_sort_value%5B%5D=htCode-desc&choices_to_sort_value%5B%5D=htCodeSDate-desc&form_id=city_merger_form&source=setup&file_format=csv&charset=UTF-8&bom=0&op=download" |>
    param_set("year_from", year(date_start)) |>
    param_set("month_from", month(date_start)) |>
    param_set("day_from", day(date_start)) |>
    param_set("year_to", year(date_end)) |>
    param_set("month_to", month(date_end)) |>
    param_set("day_to", day(date_end))
  file <- file_temp()
  curl::curl_download(url, file)
  read_absorption_separation(file)
}

read_absorption_separation <- function(file) {
  col_types <- cols(`標準地域コード` = "c",
                    `都道府県` = "c",
                    `政令市･郡･支庁･振興局等` = "c",
                    `政令市･郡･支庁･振興局等（ふりがな）` = "c",
                    `市区町村` = "c",
                    `市区町村（ふりがな）` = "c",
                    `廃置分合等施行年月日` = "D",
                    `改正事由` = "c")

  col_names <- c(`標準地域コード` = "city_code",
                 `都道府県` = "pref_name",
                 `政令市･郡･支庁･振興局等` = "city_desig_name",
                 `政令市･郡･支庁･振興局等（ふりがな）` = "city_desig_name_kana",
                 `市区町村` = "city_name",
                 `市区町村（ふりがな）` = "city_name_kana",
                 `廃置分合等施行年月日` = "date",
                 `改正事由` = "event")

  absorption_separation <- read_csv(file,
                                    col_types = col_types) |>
    rename_with(\(.) col_names,
                names(col_names)) |>
    relocate(date) |>
    arrange(date) |>
    mutate(across(c(pref_name, city_desig_name, city_desig_name_kana, city_name, city_name_kana),
                  \(x) x |>
                    str_remove_all("\\s") |>
                    stringi::stri_trans_nfkc()),
           city_name = city_name |>
             coalesce(city_desig_name),
           city_name_kana = city_name_kana |>
             coalesce(city_desig_name_kana),
           event = event |>
             str_split("\\n|　|(?<=編入)し、")) |>
    unnest(event)  |>
    mutate(event = event |>
             str_remove_all("\\s") |>
             stringi::stri_trans_nfkc()) |>
    add_row(date = ymd("2006-03-01"),
            city_code = "19201",
            pref_name = "山梨県",
            city_name = "甲府市",
            city_name_kana = "こうふし",
            event = "上九一色村(19341)大字梯及び古関が甲府市(19201)に編入") |>
    add_row(date = ymd("2006-03-01"),
            city_code = "19341",
            pref_name = "山梨県",
            city_desig_name = "西八代郡",
            city_desig_name_kana = "にしやつしろぐん",
            city_name = "上九一色村",
            city_name_kana = "かみくいしきむら",
            event = "中道町(19326)と上九一色村(19341)大字梯及び古関が甲府市(19201)に編入") |>
    vec_unique() |>
    summarise(across(c(city_code, pref_name, city_desig_name, city_desig_name_kana, city_name_kana),
                     \(x) x |>
                       replace_na("") |>
                       str_c(collapse = "|")),
              .by = c(date, city_name, event)) |>
    relocate(city_code) |>
    mutate(pattern_city = str_glue("(\\((({city_name_kana})、)?({city_code})\\))"),
           pattern_city = case_when(city_name == "上九一色村" ~
                                      str_glue("{city_name}{pattern_city}大字梯及び古関|大字精進、本栖及び富士ヶ嶺"),

                                    # 2024-01-01: Kita-ku, Hamamatsu City
                                    city_desig_name == "浜松市" & city_name == "北区" ~
                                      str_glue("{city_name}{pattern_city}(\\((三方原地区/初生町、三方原町、東三方町、豊岡町、三幸町、大原町、根洗町|三方原地区以外)\\))?"),

                                    .default = str_glue("({pref_name})?({city_desig_name})?{city_name}{pattern_city}?")),
           pattern_city = str_glue("^{pattern_city}$")) |>
    nest(.by = c(date, event),
         .key = "city") |>
    mutate(pattern_city = city |>
             map_chr(\(city) {
               pattern_city <- city |>
                 pull(pattern_city) |>
                 str_extract("(?<=\\^).+(?=\\$)") |>
                 vec_unique() |>
                 str_c(collapse = "|")

               str_glue("({pattern_city})")
             }))

  zero_or_more <- str_c("{{", str_glue("0,{max(str_length(absorption_separation$event))}"), "}}")
  pattern_city <- "{pattern_city}"
  pattern_cities <- str_glue("({pattern_city}[、と]){zero_or_more}{pattern_city}")
  pattern_absorption_separation <- tribble(
    ~ name, ~ city_from, ~ verb_city_from, ~ city_to, ~ verb_city_to,
    "編入合併", pattern_cities, "が", pattern_city, "に編入",
    "新設合併", pattern_cities, "が(合併|統合)し、", pattern_city, "を新設",
    "政令指定都市施行", pattern_city, "の", pattern_city, "への政令指定都市[施移]行",
    "区の新設|郡の新設", "", "", pattern_cities, "の新設",
    "区の再編", pattern_cities, "を", pattern_city, "に再編(\\(区名、区域の変更なし\\))?",
    "分割", pattern_city, "を分割し、", pattern_cities, "を新設",
    "分離", pattern_cities, "から分離し、", pattern_cities, "を新設",
    "名称変更", pattern_city, str_glue("が(.{zero_or_more}に[市町]制施行し、)?"), pattern_city, "に名称変更",
    "町制施行", pattern_city, str_glue("が(.{zero_or_more}に名称変更し、)?"), pattern_city, "に町制施行",
    "市制施行", pattern_city, str_glue("が(.{zero_or_more}に名称変更し、)?"), pattern_city, "に市制施行",
    "郡の区域変更", pattern_cities, "が", pattern_cities, "に(郡の)?区域変更",
    "郡の廃止", pattern_city, "の廃止", "", ""
  )

  pattern_type <- pattern_absorption_separation |>
    str_glue_data("^{city_from}{verb_city_from}{city_to}{verb_city_to}$") |>
    set_names(pattern_absorption_separation$name)

  pattern_city_from <- pattern_absorption_separation |>
    str_glue_data("^{city_from}(?={verb_city_from}{city_to}{verb_city_to}$)") |>
    set_names(pattern_absorption_separation$name)

  pattern_city_to <- pattern_absorption_separation |>
    str_glue_data("(?<={city_from}{verb_city_from}){city_to}(?={verb_city_to}$)") |>
    set_names(pattern_absorption_separation$name)

  get_city_from_to <- function(city, city_from_to) {
    if (is.na(city_from_to)) {
      city |>
        select(!pattern_city) |>
        vec_init()
    } else {
      out <- city |>
        filter(str_detect(city_from_to, pattern_city)) |>
        select(!pattern_city)
      stopifnot(vec_size(out) == 1L)
      if (str_detect(out$city_code, "\\|")) {
        city_code <- city_from_to |>
          str_extract("\\d{5}")
        city_name_kana <- city_from_to |>
          str_extract("(?<=\\()\\p{Hiragana}+(?=、\\d{5}\\))")

        out <- out |>
          mutate(across(everything(),
                        \(x) x |>
                          str_split("\\|"))) |>
          unnest(everything()) |>
          filter(city_code == .env$city_code,
                 is.na(.env$city_name_kana) | city_name_kana == .env$city_name_kana)
        stopifnot(vec_size(out) == 1L)
      }
      out |>
        mutate(across(everything(),
                      \(x) x |>
                        na_if("")),
               city_name = city_name |>
                 na_if(city_desig_name),
               city_name_kana = city_name_kana |>
                 na_if(city_desig_name_kana)) |>
        relocate(city_code, pref_name, city_desig_name, city_desig_name_kana, city_name, city_name_kana)
    }
  }

  absorption_separation |>
    filter(!str_detect(event, "(特例市に|中核市に)移行$|の区の再編")) |>
    mutate(type = case_when(str_detect(event, str_glue(pattern_type[["編入合併"]])) ~ "編入合併",
                            str_detect(event, str_glue(pattern_type[["新設合併"]])) ~ "新設合併",
                            str_detect(event, str_glue(pattern_type[["政令指定都市施行"]])) ~ "政令指定都市施行",
                            str_detect(event, str_glue(pattern_type[["区の新設|郡の新設"]])) ~ "区の新設|郡の新設",
                            str_detect(event, str_glue(pattern_type[["区の再編"]])) ~ "区の再編",
                            str_detect(event, str_glue(pattern_type[["分割"]])) ~ "分割",
                            str_detect(event, str_glue(pattern_type[["分離"]])) ~ "分離",
                            str_detect(event, str_glue(pattern_type[["名称変更"]])) ~ "名称変更",
                            str_detect(event, str_glue(pattern_type[["町制施行"]])) ~ "町制施行",
                            str_detect(event, str_glue(pattern_type[["市制施行"]])) ~ "市制施行",
                            str_detect(event, str_glue(pattern_type[["郡の区域変更"]])) ~ "郡の区域変更",
                            str_detect(event, str_glue(pattern_type[["郡の廃止"]])) ~ "郡の廃止")) |>
    nest(.by = type,
         .key = "absorption_separation") |>
    left_join(pattern_city_from |>
                enframe("type", "pattern_city_from"),
              by = join_by(type)) |>
    left_join(pattern_city_to |>
                enframe("type", "pattern_city_to"),
              by = join_by(type)) |>
    mutate(absorption_separation = list(absorption_separation, pattern_city_from, pattern_city_to) |>
             pmap(\(absorption_separation, pattern_city_from, pattern_city_to) {
               absorption_separation |>
                 mutate(pattern_city_from = str_glue(pattern_city_from),
                        pattern_city_to = str_glue(pattern_city_to))
             }),
           .keep = "unused") |>
    unnest(absorption_separation) |>
    relocate(date, type) |>
    arrange(date, type) |>
    mutate(city_from = event |>
             str_extract(pattern_city_from) |>
             str_extract_all(pattern_city),
           city_to = event |>
             str_extract(pattern_city_to) |>
             str_extract_all(pattern_city),
           event,
           .keep = "unused") |>
    unnest(city_from,
           keep_empty = TRUE) |>
    unnest(city_to,
           keep_empty = TRUE) |>
    mutate(from = list(city, city_from) |>
             pmap(get_city_from_to) |>
             list_c(),
           to = list(city, city_to) |>
             pmap(get_city_from_to) |>
             list_c(),
           .keep = "unused")
}
