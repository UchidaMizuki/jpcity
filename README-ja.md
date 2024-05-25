
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpcity <a href="https://uchidamizuki.github.io/jpcity/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/UchidaMizuki/jpcity/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UchidaMizuki/jpcity/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/jpcity)](https://CRAN.R-project.org/package=jpcity)
[![Codecov test
coverage](https://codecov.io/gh/UchidaMizuki/jpcity/branch/main/graph/badge.svg)](https://app.codecov.io/gh/UchidaMizuki/jpcity?branch=main)
<!-- badges: end -->

jpcityは，日本の市区町村コードの読み取り・変換を行うためのRパッケージです．
このパッケージは，以下のような機能を提供しています．

- 市区町村コードの読み取り：`parse_city()`
  - `city_name()`や`pref_name()`を組み合わせることで市区町村名や都道府県名を取得可能
- 異なる時点の市区町村コードへの変換（廃置分合処理）：`city_convert()`
- 政令指定都市の区の集約・区への分割：`city_desig_merge()`，`city_desig_split()`
- 特定時点の市区町村コードの取得：`get_city()`
- 都道府県・市区町村名を用いた市区町村コードの検索：`find_city()`

## Installation

``` r
install.packages("jpcity")
```

jpcityの開発版は，[GitHub](https://github.com/)からインストールすることができます．

``` r
# install.packages("devtools")
devtools::install_github("UchidaMizuki/jpcity")
```

## Examples

``` r
library(jpcity)
library(tidyverse)
```

### 市区町村コードの読み取り

``` r
city <- parse_city(c("13101", "27101", "23101"))
#> Guessing the interval to be 1970-04-01 JST--1989-02-12 JST.
#> ℹ You can override using `when` argument.
```

``` r

# Override the interval using `when` argument
city <- parse_city(c("13101", "27101", "23101"),
                   when = "1980-01-01")
city
#> <city[3]> Interval: 1970-04-01--1989-02-12
#> [1] 13101 27101 23101
#> 
#> Cities:
#>   city_code pref_name city_desig_name city_desig_name_kana city_name
#> 1     13101    東京都            <NA>                 <NA>  千代田区
#> 2     27101    大阪府          大阪市           おおさかし      北区
#> 3     23101    愛知県        名古屋市             なごやし    千種区
#>   city_name_kana
#> 1       ちよだく
#> 2         きたく
#> 3       ちくさく
```

``` r

tibble(city = city,
       pref_name = pref_name(city),
       city_name = city_name(city),
       city_name_kana = city_name(city,
                                  kana = TRUE))
#> # A tibble: 3 × 4
#>   city                         pref_name city_name      city_name_kana  
#>   <city>                       <chr>     <chr>          <chr>           
#> 1 13101 [東京都千代田区]       東京都    千代田区       ちよだく        
#> 2 27101 [大阪府大阪市北区]     大阪府    大阪市北区     おおさかしきたく
#> 3 23101 [愛知県名古屋市千種区] 愛知県    名古屋市千種区 なごやしちくさく
```

### 異なる時点の市区町村コードへの変換（廃置分合処理）

``` r
city <- parse_city(c("13101", "27101", "23101"),
                   when = "1980-01-01")
tibble(city_from = city,
       city_to = city_convert(city,
                              from = "1980-01-01",
                              to = "2020-01-01")) |> 
  unnest(city_to)
#> # A tibble: 3 × 2
#>   city_from                    city_to                     
#>   <city>                       <city>                      
#> 1 13101 [東京都千代田区]       13101 [東京都千代田区]      
#> 2 27101 [大阪府大阪市北区]     27127 [大阪府大阪市北区]    
#> 3 23101 [愛知県名古屋市千種区] 23101 [愛知県名古屋市千種区]
```

``` r

city <- parse_city("15100",
                   when = "2020-01-01")
tibble(city_from = city,
       city_to = city_convert(city,
                              from = "2020-01-01",
                              to = "1970-04-01")) |> 
  unnest(city_to)
#> # A tibble: 15 × 2
#>    city_from            city_to               
#>    <city>               <city>                
#>  1 15100 [新潟県新潟市] 15201 [新潟県新潟市]  
#>  2 15100 [新潟県新潟市] 15207 [新潟県新津市]  
#>  3 15100 [新潟県新潟市] 15220 [新潟県白根市]  
#>  4 15100 [新潟県新潟市] 15305 [新潟県豊栄町]  
#>  5 15100 [新潟県新潟市] 15321 [新潟県小須戸町]
#>  6 15100 [新潟県新潟市] 15323 [新潟県横越村]  
#>  7 15100 [新潟県新潟市] 15324 [新潟県亀田町]  
#>  8 15100 [新潟県新潟市] 15341 [新潟県岩室村]  
#>  9 15100 [新潟県新潟市] 15345 [新潟県巻町]    
#> 10 15100 [新潟県新潟市] 15346 [新潟県西川町]  
#> 11 15100 [新潟県新潟市] 15347 [新潟県黒埼村]  
#> 12 15100 [新潟県新潟市] 15348 [新潟県味方村]  
#> 13 15100 [新潟県新潟市] 15349 [新潟県潟東村]  
#> 14 15100 [新潟県新潟市] 15350 [新潟県月潟村]  
#> 15 15100 [新潟県新潟市] 15351 [新潟県中之口村]
```

### 政令指定都市の区の集約・区への分割

``` r
city <- parse_city(c("13101", "27101", "23101"),
                   when = "1980-01-01")
tibble(city = city,
       city_desig = city_desig_merge(city),
       city_desig_merge_tokyo = city_desig_merge(city,
                                                 merge_tokyo = TRUE))
#> # A tibble: 3 × 3
#>   city                         city_desig             city_desig_merge_tokyo
#>   <city>                       <city>                 <city>                
#> 1 13101 [東京都千代田区]       13101 [東京都千代田区] 13100 [東京都特別区部]
#> 2 27101 [大阪府大阪市北区]     27100 [大阪府大阪市]   27100 [大阪府大阪市]  
#> 3 23101 [愛知県名古屋市千種区] 23100 [愛知県名古屋市] 23100 [愛知県名古屋市]
```

### 特定時点の市区町村コードの取得

``` r
tibble(city = get_city("2020-01-01"))
#> # A tibble: 1,923 × 1
#>    city                      
#>    <city>                    
#>  1 01100 [北海道札幌市]      
#>  2 01101 [北海道札幌市中央区]
#>  3 01102 [北海道札幌市北区]  
#>  4 01103 [北海道札幌市東区]  
#>  5 01104 [北海道札幌市白石区]
#>  6 01105 [北海道札幌市豊平区]
#>  7 01106 [北海道札幌市南区]  
#>  8 01107 [北海道札幌市西区]  
#>  9 01108 [北海道札幌市厚別区]
#> 10 01109 [北海道札幌市手稲区]
#> # ℹ 1,913 more rows
```

``` r

tibble(city = get_city("1970-04-01"))
#> # A tibble: 3,376 × 1
#>    city                  
#>    <city>                
#>  1 01201 [北海道札幌市]  
#>  2 01202 [北海道函館市]  
#>  3 01203 [北海道小樽市]  
#>  4 01204 [北海道旭川市]  
#>  5 01205 [北海道室蘭市]  
#>  6 01206 [北海道釧路市]  
#>  7 01207 [北海道帯広市]  
#>  8 01208 [北海道北見市]  
#>  9 01209 [北海道夕張市]  
#> 10 01210 [北海道岩見沢市]
#> # ℹ 3,366 more rows
```

### 都道府県・市区町村名を用いた市区町村コードの検索

``` r
find_city(c("東京都", "新宿区"))
#> <city[1]> Interval: 1970-04-01--Inf
#> [1] 13104
#> 
#> Cities:
#>   city_code pref_name city_desig_name city_desig_name_kana city_name
#> 1     13104    東京都            <NA>                 <NA>    新宿区
#>   city_name_kana
#> 1   しんじゅくく
```
