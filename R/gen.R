#' Generate valid examples of the strings
#'
#' Rules for generating the IDs from
#' \url{http://tarkistusmerkit.teppovuori.fi/tarkmerk.htm}
#' \describe{
#'  \item{\code{gen_yt}}{generates a Business ID (y-tunnus).}
#'  \item{\code{gen_vat}}{generates a (Finnish) VAT Number (alv-tunnus).}
#'  \item{\code{gen_ovt}}{generates a (Finnish) OVT-code (ovt-tunnus).}
#'  \item{\code{gen_id}}{generates a (Finnish) ID Number (ovt-tunnus).}
#' }
#'
#' @param n The number of examples to be generated. Defaults to 1.
#' @name gen
NULL

#' @export
#' @rdname gen
gen_yt <- function(n = 1) {
  digits <- map(1:n, ~sample(9, 7, TRUE))

  mod <- map_int(digits, yt_cs_map)
  while (any(mod == 1)) {
    digits <- map_if(digits, mod == 1, ~sample(9, 7, TRUE))
    mod <- map_int(digits, yt_cs_map)
  }

  ifelse(mod == 0, mod, 11 - mod) %>%
    map2_chr(digits, ~c(.y, "-", .x) %>% str_c(collapse = ""))
}

#' @export
#' @rdname gen
gen_vat <- function(n = 1) {
  x <- gen_yt(n)
  str_c("FI", str_sub(x, 1, 7), str_sub(x, 9, 9))
}

#' @export
#' @rdname gen
gen_ovt <- function(n = 1) {
  x <- gen_yt(n)
  suffix_len <- sample(0:5, n, TRUE)
  str_c("0037", str_sub(x, 1, 7), str_sub(x, 9, 9)) %>%
    map2(suffix_len, ~str_c(.x, str_c(sample(0:9, .y, TRUE), collapse = ""))) %>%
    unlist()
}

#' @export
#' @rdname gen
gen_id <- function(n = 1) {
  date_seq <- seq(ymd("1850-01-01"), ymd("2017-08-21"), by = "day") %>%
    sample(n, replace = TRUE) %>%
    as.character()

  p <- function(x, n) str_pad(x, n, pad = 0)
  ppkkvv <- str_c(p(day(date_seq), 2),
                  p(month(date_seq), 2),
                  p(year(date_seq) %% 100, 2))
  nnn <- sample(2:500, n, TRUE) %>% p(3)

  t <- str_c(ppkkvv, nnn) %>%
    as.integer() %>%
    id_cs_map()

  s <- c("18" = "+", "19" = "-" , "20" = "A") %>%
    str_map(str_sub(date_seq, 1, 2))
  str_c(ppkkvv, s, nnn, t)
}

