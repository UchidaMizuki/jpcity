#' @importFrom labelled val_labels<-
#' @export
`val_labels<-.jpcity_city` <- function(x, v, value) {
  vec_restore(NextMethod(), x)
}
