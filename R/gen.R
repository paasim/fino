#' Generate valid examples of the strings
#'
#' Rules for generating the IDs obtained from
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
  digits <- rerun(n, sample(0:9, 7, TRUE))

  cs <- yt_cs_map(digits)
  while (any(is.na(cs))) {
    digits <- map_if(digits, is.na(cs), ~sample(0:9, 7, TRUE))
    cs <- yt_cs_map(digits)
  }

  map(digits, ~str_c(.x, collapse = "")) %>%
    str_c("-", cs)
}

#' @export
#' @rdname gen
gen_vat <- function(n = 1) gen_yt(n) %>% yt_to_vat()

#' @export
#' @rdname gen
gen_ovt <- function(n = 1) {
  x <- gen_yt(n) %>% yt_to_ovt()
  suffix <- sample(0:5, n, TRUE) %>%
    map_chr(~str_c("", sample(0:9, .x, TRUE), collapse = ""))
  str_c(x, suffix)
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
    id_cs_map()

  s <- c("18" = "+", "19" = "-" , "20" = "A")
  str_c(ppkkvv, s[str_sub(date_seq, 1, 2)], nnn, t)
}
