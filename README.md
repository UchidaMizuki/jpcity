
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpcity <a href="https://uchidamizuki.github.io/jpcity/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/UchidaMizuki/jpcity/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UchidaMizuki/jpcity/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/jpcity)](https://CRAN.R-project.org/package=jpcity)
[![Codecov test
coverage](https://codecov.io/gh/UchidaMizuki/jpcity/branch/main/graph/badge.svg)](https://app.codecov.io/gh/UchidaMizuki/jpcity?branch=main)
<!-- badges: end -->

README in Japanese is
[here](https://github.com/UchidaMizuki/jpcity/blob/main/README-ja.md).

jpcity is an R package for reading and converting Japanese municipality
codes. This package provides the following features,

- Read city codes: `parse_city()`.
  - city and prefecture names can be obtained by combining `city_name()`
    and `pref_name()`.
- Convert to city codes at a different point in time: `city_convert()`
- Combine wards of designated cities or divide them into wards:
  `city_desig_merge()`，`city_desig_split()`
- Get city codes at a specific point in time: `get_city()`
- Find city codes using the name of the prefecture or city:
  `find_city()`

## Installation

You can install the development version of jpcity from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UchidaMizuki/jpcity")
```

## Examples

``` r
library(jpcity)
#> Warning: パッケージ 'jpcity' はバージョン 4.3.1 の R の下で造られました
library(tidyverse)
```

### Read city codes

``` r
city <- parse_city(c("13101", "27101", "23101"))
#> Guessing the interval to be 1970-04-01 JST--1989-02-12 JST.
#> ℹ You can override using `when` argument.

# Override the interval using `when` argument
city <- parse_city(c("13101", "27101", "23101"),
                   when = "1989-02-12")
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

### Convert to city codes at a different point in time

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

### Combine wards of designated cities or divide them into wards

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

### Get city codes at a specific point in time

``` r
tibble(city = get_city("2020-01-01"))
#> # A tibble: 1,902 × 1
#>    city                      
#>    <city>                    
#>  1 01101 [北海道札幌市中央区]
#>  2 01102 [北海道札幌市北区]  
#>  3 01103 [北海道札幌市東区]  
#>  4 01104 [北海道札幌市白石区]
#>  5 01105 [北海道札幌市豊平区]
#>  6 01106 [北海道札幌市南区]  
#>  7 01107 [北海道札幌市西区]  
#>  8 01108 [北海道札幌市厚別区]
#>  9 01109 [北海道札幌市手稲区]
#> 10 01110 [北海道札幌市清田区]
#> # ℹ 1,892 more rows

tibble(city = get_city("1970-04-01"))
#> # A tibble: 3,369 × 1
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
#> # ℹ 3,359 more rows
```

### Find city codes using the name of the prefecture or city

``` r
find_city(c("東京都", "新宿区"))
#> <city[1]> Interval: 1970-04-01--2023-04-25
#> [1] 13104
#> 
#> Cities:
#>   city_code pref_name city_desig_name city_desig_name_kana city_name
#> 1     13104    東京都            <NA>                 <NA>    新宿区
#>   city_name_kana
#> 1   しんじゅくく
```
