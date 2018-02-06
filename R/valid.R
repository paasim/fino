#' Check if the string is a valid ID Number
#'
#' Functions for inferring if the input string is a valid Business ID, VAT
#' Number, E-Invoicing address or an ID Number. Rules obtained from
#' \url{http://tarkistusmerkit.teppovuori.fi/tarkmerk.htm}. Removes all
#' characters that can not occur in a valid string, such commas, dots
#' and white space.
#'
#'
#' @param x The input string(s).
#' @param locate If \code{TRUE} (default) tries to locate the valid part of
#'  the string.
#' @param require_checksum If \code{TRUE} (default), require that the checksum
#' digit matches in addition to the format of the string. Note, does not make
#' sense when used to validate non-Finnish VAT-numbers.
#'
#' @return A logical vector that can be interpreted as follows:
#'  \describe{
#'   \item{NA}{The string has a correct format but an incorrect checksum-digit.}
#'   \item{FALSE}{Ths string has an incorrect format.}
#'   \item{TRUE}{The string appears to be of correct format.}
#' }
#'
#' @name valid
NULL

#' @export
#' @rdname valid
valid_yt <- function(x, locate = TRUE, require_checksum = TRUE) {

  x <- invalid_to_na(x, regex_yt, locate, "\\-")

  if (!require_checksum) return(!is.na(x))

  check <- str_sub(x, 0, 7) %>%
    str_split("") %>%
    map(yt_cs_map) %>%
    map_if(~.x == 1L & !is.na(.x), ~NA) %>%
    map_int(~(11L-.x) %% 11L)

  # validate against the checksum
  res <- as.integer(str_sub(x, 9, 9)) == check
  ifelse(is.na(res), FALSE, res)

}
#' @export
#' @rdname valid
valid_ovt <- function(x, locate = TRUE, require_checksum = TRUE) {

  x <- invalid_to_na(x, regex_ovt, locate)

  if (!require_checksum) return(!is.na(x))

  ovt_to_yt(x) %>% valid_yt(FALSE, TRUE)
}

#' @export
#' @rdname valid
valid_vat <- function(x, locate = TRUE, require_checksum = TRUE) {

  if (!locate) stop("Only locate = TRUE implemented.")

  x <- invalid_to_na(x, regex_vat(), locate, "*+")

  if (!require_checksum) return(!is.na(x))

  vat_fi <- vat_regexes$format[vat_regexes$code == "FI"]
  yt_fi <- str_extract(x, str_c("^", vat_fi, "$"))
  valid_fi <- vat_to_yt(yt_fi) %>% valid_yt(FALSE, TRUE)
  # the parenthesized part includes the checksum only finnish VAT numbers
  !is.na(x) & (is.na(yt_fi) | valid_fi)
}

#' @export
#' @rdname valid
valid_id <- function(x, locate = TRUE, require_checksum = TRUE) {

  x <- invalid_to_na(x, regex_id, locate, "\\-")

  days <- str_c(str_map(c("+" = "18", "-" = "19", "A" = "20"), str_sub(x, 7,7)),
                str_sub(x, 5, 6), str_sub(x, 3, 4), str_sub(x, 1, 2)) %>%
    ymd(quiet = TRUE)
  days[days <= "1850-01-01"] <- NA

  if (!require_checksum) return(!is.na(days))

  check <- str_c(str_sub(x, 1, 6), str_sub(x, 8, 10)) %>%
    as.integer() %>%
    id_cs_map()

  !is.na(days) & !is.na(check) & (check == str_sub(x, 11, 11))
}
