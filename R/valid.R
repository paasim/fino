#' Check if the string is a valid ID Number
#'
#' Functions for inferring if the input string is a valid Business ID, VAT
#' Number, E-Invoicing address or a personal identification code (PIC).
#' Rules obtained from \url{http://tarkistusmerkit.teppovuori.fi/tarkmerk.htm}.
#' Removes all characters that can not occur in a valid string, such commas,
#' dots and white space.
#'
#'
#' @param x The input string(s).
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
valid_yt <- function(x, require_checksum = TRUE) {

  x <- str_extract(x, str_c("^", regex_yt, "$"))
  if (require_checksum) {
    valid_yt_cs(x) %>% coalesce(FALSE)
  } else {
    !is.na(x)
  }
}

#' @export
#' @rdname valid
valid_ovt <- function(x, require_checksum = TRUE) {

  x <- str_extract(x, str_c("^", regex_ovt, "$"))
  if (require_checksum) {
    ovt_to_yt(x) %>% valid_yt_cs() %>% coalesce(FALSE)
  } else {
    !is.na(x)
  }
}

#' @export
#' @rdname valid
valid_vat <- function(x, require_checksum = TRUE) {
  # spaces are ignored in VAT-numbers
  x <- str_remove_all(x, " ") %>% str_extract(str_c("^", regex_vat_all(), "$"))
  if (require_checksum) {
    vat_fi <- str_extract(x, str_c("^", regex_vat_fi, "$"))
    cs_fi <- vat_to_yt(vat_fi) %>%
      valid_yt_cs()
    !is.na(x) & (is.na(vat_fi) | cs_fi)
  } else {
    !is.na(x)
  }
}

#' @export
#' @rdname valid
valid_pic <- function(x, require_checksum = TRUE) {
  x <- str_extract(x, str_c("^", regex_pic, "$"))

  days <- str_c(c("+" = "18", "-" = "19", "A" = "20")[str_sub(x, 7,7)],
                str_sub(x, 5, 6), str_sub(x, 3, 4), str_sub(x, 1, 2)) %>%
    ymd(quiet = TRUE)
  days[days < "1850-01-01"] <- NA_character_

  if (!require_checksum) return(!is.na(days))

  check <- str_c(str_sub(x, 1, 6), str_sub(x, 8, 10)) %>%
    pic_cs_map()

  !is.na(days) & !is.na(check) & (check == str_sub(x, 11, 11))
}
