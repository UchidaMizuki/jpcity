library(readr)

# city --------------------------------------------------------------------

# Last update date: November 27, 2021

cols_city <- cols(`標準地域コード` = "c",
                  `都道府県` = "c",
                  `政令市･郡･支庁･振興局等` = "c",
                  `政令市･郡･支庁･振興局等（ふりがな）` = "c",
                  `市区町村` = "c",
                  `市区町村（ふりがな）` = "c",
                  `廃置分合等施行年月日` = "D",
                  `廃置分合等情報有無` = "c")

col_names_city <- c(`標準地域コード` = "city_code",
                    `都道府県` = "prefecture_name",
                    `政令市･郡･支庁･振興局等` = "subprefecture_name",
                    `政令市･郡･支庁･振興局等（ふりがな）` = "subprefecture_name_kana",
                    `市区町村` = "city_name",
                    `市区町村（ふりがな）` = "city_name_kana",
                    `廃置分合等施行年月日` = "merger_date",
                    `廃置分合等情報有無` = "merged")

city_1970_04_01 <- read_csv("data-raw/city/city_1970-04-01.csv",
                            locale = locale(encoding = "shift-jis"),
                            col_types = cols_city)

# URL: https://www.e-stat.go.jp/municipalities/cities/areacode
city_latest <- read_csv("data-raw/city/city_latest.csv",
                        locale = locale(encoding = "shift-jis"),
                        col_types = cols_city)

# usethis::use_data(DATASET, overwrite = TRUE)
