#' Convert between different ID numbers
#'
#' Currently allows conversion from E-invoicing address and VAT number to
#' Business ID and vice versa.
#'
#' @param x The input string(s).
#'
#' @name conv
NULL

#' @export
#' @rdname conv
ovt_to_yt <- function(x) conv_to_yt(x, regex_ovt, 4)

#' @export
#' @rdname conv
vat_to_yt <- function(x) conv_to_yt(x, "FI\\d{8}", 2)

conv_to_yt <- function(x, regex, i) {
  x <- str_extract(x, regex)
  str_c(str_sub(x, i+1, i+7), "-", str_sub(x, i+8, i+8))
}

#' @export
#' @rdname conv
yt_to_vat <- function(x) conv_from_yt(x, "FI")

#' @export
#' @rdname conv
yt_to_ovt <- function(x) conv_from_yt(x, "0037")

conv_from_yt <- function(x, prefix) {
  x <- str_extract(x, regex_yt)
  str_c(prefix, str_sub(x, 1, 7), str_sub(x, 9, 9))
}
