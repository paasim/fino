#' Regexes for EU VAT Identification numbers
#'
#' A dataset containing regexes for VAT numbers (excluding whitespaces).
#'
#' @format A tibble with 28 rows and 3 columns
#' \describe{
#'   \item{code}{A country code.}
#'   \item{country}{The name of the country.}
#'   \item{format}{The regex for the VAT identification number}
#' }
#' @source \url{http://ec.europa.eu/taxation_customs/vies/faqvies.do#item_11}
"vat_regexes"
