#'
#'
#' @export
city_shikuchoson <- function(x, date,
                             shi = TRUE,
                             ku_tokyo = TRUE,
                             ku_seirei = TRUE,
                             cho = TRUE,
                             son = TRUE) {

}

#' @rdname city_shikuchoson
#' @export
city_shichoson <- function(x, date,
                           shi = TRUE,
                           cho = TRUE,
                           son = TRUE) {
  city_shikuchoson(x, date,
                   shi = shi,
                   ku_tokyo = FALSE,
                   ku_seirei = FALSE,
                   cho = cho,
                   son = son)
}
