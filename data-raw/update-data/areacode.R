
# areacode ----------------------------------------------------------------

get_areacode <- function(date) {
  url <- "https://www.e-stat.go.jp/municipalities/cities/areacode?prefecture_all=on&pf%5B1%5D=1&pf%5B2%5D=2&pf%5B3%5D=3&pf%5B4%5D=4&pf%5B5%5D=5&pf%5B6%5D=6&pf%5B7%5D=7&pf%5B8%5D=8&pf%5B9%5D=9&pf%5B10%5D=10&pf%5B11%5D=11&pf%5B12%5D=12&pf%5B13%5D=13&pf%5B14%5D=14&pf%5B15%5D=15&pf%5B16%5D=16&pf%5B17%5D=17&pf%5B18%5D=18&pf%5B19%5D=19&pf%5B20%5D=20&pf%5B21%5D=21&pf%5B22%5D=22&pf%5B23%5D=23&pf%5B24%5D=24&pf%5B25%5D=25&pf%5B26%5D=26&pf%5B27%5D=27&pf%5B28%5D=28&pf%5B29%5D=29&pf%5B30%5D=30&pf%5B31%5D=31&pf%5B32%5D=32&pf%5B33%5D=33&pf%5B34%5D=34&pf%5B35%5D=35&pf%5B36%5D=36&pf%5B37%5D=37&pf%5B38%5D=38&pf%5B39%5D=39&pf%5B40%5D=40&pf%5B41%5D=41&pf%5B42%5D=42&pf%5B43%5D=43&pf%5B44%5D=44&pf%5B45%5D=45&pf%5B46%5D=46&pf%5B47%5D=47&ht=&city_nm=&city_kd%5B4%5D=4&city_kd%5B5%5D=5&city_kd%5B2%5D=2&city_kd%5B3%5D=3&city_kd%5B6%5D=6&city_kd%5B7%5D=7&keyword_kd=code&item%5B%5D=htCode&item%5B%5D=todoNm&item%5B%5D=parentCityNm&item%5B%5D=parentCityKana&item%5B%5D=selfCityNm&item%5B%5D=selfCityKana&item%5B%5D=htCodeSDate&item%5B%5D=jiyuId&sort%5B%5D=htCode-asc&choices_to_show%5B%5D=cityType&choices_to_show%5B%5D=kasoFlg&choices_to_show%5B%5D=htCodeKokujiDate&choices_to_show%5B%5D=htCodeKokujiNo&choices_to_show%5B%5D=htCodeEDate&choices_to_sort%5B%5D=kasoFlg&choices_to_sort%5B%5D=htCodeSDate&choices_to_sort%5B%5D=htCodeEDate&choices_to_sort%5B%5D=htCodeKokujiDate&choices_to_sort%5B%5D=htCodeKokujiNo&choices_to_sort_value%5B%5D=htCode-desc&choices_to_sort_value%5B%5D=kasoFlg-asc&choices_to_sort_value%5B%5D=kasoFlg-desc&choices_to_sort_value%5B%5D=htCodeSDate-asc&choices_to_sort_value%5B%5D=htCodeSDate-desc&choices_to_sort_value%5B%5D=htCodeEDate-asc&choices_to_sort_value%5B%5D=htCodeEDate-desc&choices_to_sort_value%5B%5D=htCodeKokujiDate-asc&choices_to_sort_value%5B%5D=htCodeKokujiDate-desc&choices_to_sort_value%5B%5D=htCodeKokujiNo-asc&choices_to_sort_value%5B%5D=htCodeKokujiNo-desc&form_id=city_areacode_form&source=setup&page=&file_format=csv&charset=UTF-8&bom=0&op=download" |>
    param_set("date_year", year(date)) |>
    param_set("date_month", month(date)) |>
    param_set("date_day", day(date))
  file <- file_temp()
  curl::curl_download(url, file)
  read_areacode(file)
}

read_areacode <- function(file) {
  col_types <- cols(`標準地域コード` = "c",
                    `都道府県` = "c",
                    `政令市･郡･支庁･振興局等` = "c",
                    `政令市･郡･支庁･振興局等（ふりがな）` = "c",
                    `市区町村` = "c",
                    `市区町村（ふりがな）` = "c",
                    `廃置分合等施行年月日` = "D",
                    `廃置分合等情報有無` = "c")

  col_names <- c(`標準地域コード` = "city_code",
                 `都道府県` = "pref_name",
                 `政令市･郡･支庁･振興局等` = "city_desig_name",
                 `政令市･郡･支庁･振興局等（ふりがな）` = "city_desig_name_kana",
                 `市区町村` = "city_name",
                 `市区町村（ふりがな）` = "city_name_kana",
                 `廃置分合等施行年月日` = "date_absorption_separation",
                 `廃置分合等情報有無` = "absorption_separation")

  read_csv(file,
           col_types = col_types) |>
    rename_with(\(.) col_names,
                names(col_names)) |>
    mutate(across(c(pref_name, city_desig_name, city_desig_name_kana, city_name, city_name_kana),
                  \(x) x |>
                    str_remove_all("\\s") |>
                    stringi::stri_trans_nfkc())) |>
    replace_na(list(absorption_separation = "")) |>
    mutate(absorption_separation = absorption_separation == "有")
}
