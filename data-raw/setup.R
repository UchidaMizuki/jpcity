library(fs)
library(reticulate)
library(tidygraph)
library(tidyverse)
library(vctrs)

# setup -------------------------------------------------------------------

read_areacode <- function(exdir) {
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

  dir_ls(exdir,
         regexp = "csv$") |>
    read_csv(locale = locale(encoding = "shift-jis"),
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
