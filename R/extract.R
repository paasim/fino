#' Extract a valid ID Number from a string.
#'
#' Functions for extracting a valid Business ID, VAT Number, E-Invoicing
#' address or a personal identification code (PIC) if it is contained in the input string. Rules
#' obtained from \url{http://tarkistusmerkit.teppovuori.fi/tarkmerk.htm}.
#'
#' @param x The input string(s).
#' @param require_checksum If \code{TRUE} (default), require that the checksum
#' digit matches in addition to the format of the string. Note, does not make
#' sense when used to validate non-Finnish VAT-numbers.
#'
#' @return A character vector containing the only the ID-parts of the
#' string(s).
#'
#' @name extract
NULL

#' @export
#' @rdname extract
extract_yt <- function(x, require_checksum = TRUE) {
  x <- str_extract(x, regex_yt)
  if (require_checksum) {
    valid_yt_cs(x) %>% if_else(x, NA_character_)
  } else {
    x
  }
}

#' @export
#' @rdname extract
extract_ovt <- function(x, require_checksum = TRUE) {
  x <- str_extract(x, regex_ovt)
  if (require_checksum) {
    ovt_to_yt(x) %>% valid_yt_cs() %>% if_else(x, NA_character_)
  } else {
    x
  }
}

#' @export
#' @rdname extract
extract_vat <- function(x, require_checksum = TRUE) {
  x <- str_remove_all(x, " ") %>% str_extract(regex_vat_all())
  if (require_checksum) {
    vat_fi <- str_extract(x, str_c("^", regex_vat_fi, "$"))
    cs_fi <- vat_to_yt(vat_fi) %>%
      valid_yt_cs()
    (!is.na(x) & (is.na(vat_fi) | cs_fi)) %>% if_else(x, NA_character_)
  } else {
    x
  }
}

#' @export
#' @rdname extract
extract_pic <- function(x, require_checksum = TRUE) {
  x <- str_extract(x, regex_pic)

  days <- str_c(c("+" = "18", "-" = "19", "A" = "20")[str_sub(x, 7,7)],
                str_sub(x, 5, 6), str_sub(x, 3, 4), str_sub(x, 1, 2)) %>%
    ymd(quiet = TRUE)
  days[days < "1850-01-01"] <- NA_character_

  if (!require_checksum) return(!is.na(days))

  check <- str_c(str_sub(x, 1, 6), str_sub(x, 8, 10)) %>%
    pic_cs_map()

  (!is.na(days) & !is.na(check) & (check == str_sub(x, 11, 11))) %>%
    if_else(x, NA_character_)
}
