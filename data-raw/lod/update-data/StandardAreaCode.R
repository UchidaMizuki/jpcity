source("data-raw/setup.R")

# update-data-lod-StandardAreaCode ----------------------------------------

download_lod_StandardAreaCode <- function(page_size = 10000) {
  url_estat_lod <- "https://data.e-stat.go.jp/lod/sparql/alldata/query"

  query_base <- '
  PREFIX sacs: <http://data.e-stat.go.jp/lod/terms/sacs#>
  PREFIX sac: <http://data.e-stat.go.jp/lod/sac/>
  PREFIX dcterms: <http://purl.org/dc/terms/>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX org: <http://www.w3.org/ns/org#>

  SELECT *
  WHERE {
    ?code a sacs:StandardAreaCode .
    OPTIONAL { ?code rdfs:label ?label }
    OPTIONAL { ?code dcterms:identifier ?identifier } # 標準地域コード
    OPTIONAL { ?code sacs:administrativeClass ?administrativeClass } # 行政区分
    OPTIONAL { ?code dcterms:issued ?issued } # 当該コードの施行年月日
    OPTIONAL { ?code dcterms:valid ?valid } # 当該コードの次の施行年月日又は廃止年月日
    OPTIONAL { ?code sacs:previousCode ?previousCode } # 当該コードの施行年月日前の期間つき標準地域コードのリソース
    OPTIONAL { ?code sacs:succeedingCode ?succeedingCode } # 当該コードの廃止年月日後の期間つき標準地域コードのリソース
    OPTIONAL { ?code dcterms:isPartOf ?isPartOf }
    OPTIONAL { ?code sacs:hasPart ?hasPart }
    OPTIONAL { ?code sacs:previousMunicipality ?previousMunicipality } # 当該コードの変更前の期間つき標準地域コードのリソース
    OPTIONAL { ?code sacs:succeedingMunicipality ?succeedingMunicipality } # 当該コードの変更後の期間つき標準地域コードのリソース
    OPTIONAL { ?code org:changedBy ?changedBy }
    OPTIONAL { ?code org:resultedFrom ?resultedFrom }
    OPTIONAL { ?code sacs:prefecturalCapitalCode ?prefecturalCapitalCode } # 都道府県の県庁所在地を指す期間つき標準地域コードのリソース（都道府県のみ）
    OPTIONAL { ?code sacs:districtOfSubPrefecture ?districtOfSubPrefecture } # 支庁・振興局等内郡（北海道）の名称
    OPTIONAL { ?code sacs:checkDigit ?checkDigit } # 標準地域コードのチェックデジットコード（検査数字）
    OPTIONAL { ?code sacs:prefectureLabel ?prefectureLabel } # 所属する都道府県
  }
  ORDER BY ?code
  '

  fetch_page <- function(offset) {
    request(url_estat_lod) |>
      req_url_query(
        query = paste0(query_base, "LIMIT ", page_size, " OFFSET ", offset)
      ) |>
      req_headers(Accept = "application/sparql-results+json") |>
      req_user_agent("jpcity (https://github.com/UchidaMizuki/jpcity)") |>
      req_retry(max_tries = 5) |>
      req_perform() |>
      resp_body_json()
  }

  # The e-Stat SPARQL endpoint caps the number of rows returned per request,
  # so page through the result set with LIMIT/OFFSET until a short page is
  # returned (signalling the last page).
  result <- NULL
  bindings <- list()
  offset <- 0
  repeat {
    page <- fetch_page(offset)
    page_bindings <- page$results$bindings

    bindings <- c(bindings, page_bindings)
    if (is.null(result)) {
      result <- page
    }
    if (length(page_bindings) < page_size) {
      break
    }
    offset <- offset + page_size
  }

  result$results$bindings <- bindings
  result
}
