#' Convert between different ID numbers
#'
#' Currently allows conversion from E-invoicing address and VAT number to
#' Business ID and vice versa.
#'
#' @param x The input string(s).
#' @param locate If \code{TRUE} (default) tries to locate the valid part of
#'  the string.
#'
#' @name conv
NULL

#' @export
#' @rdname conv
ovt_to_yt <- function(x, locate = TRUE) conv_to_yt(x, locate, regex_ovt, 4)

#' @export
#' @rdname conv
vat_to_yt <- function(x, locate = TRUE) conv_to_yt(x, locate, "FI\\d{8}", 2)

conv_to_yt <- function(x, locate, regex, i) {
  x <- invalid_to_na(x, regex, locate)
  str_c(str_sub(x, i+1, i+7), "-", str_sub(x, i+8, i+8))
}

#' @export
#' @rdname conv
yt_to_vat <- function(x, locate = TRUE) conv_from_yt(x, locate, "FI")

#' @export
#' @rdname conv
yt_to_ovt <- function(x, locate = TRUE) conv_from_yt(x, locate, "0037")

conv_from_yt <- function(x, locate, prefix) {
  x <- invalid_to_na(x, regex_yt, locate)
  str_c(prefix, str_sub(x, 1, 7), str_sub(x, 9, 9))
}
