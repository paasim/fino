# map for the checksum in the finnish id number
id_cs_map <- function(x) {
  check_vec <- c(0:9, LETTERS[c(1:6, 8, 10:14, 16, 18:25)])
  check_vec[(as.integer(x) %% 31)+1]
}

# map for the checksum in the Business ID
yt_cs_map <- function(x) {
  res <- map_int(x, ~sum(as.integer(.x) * c(7L, 9L, 10L, 5L, 8L, 4L, 2L))) %>%
    mod(11L) %>%
    na_if(1L)
  if_else(res == 0L, res, 11L - res)
}

valid_yt_cs <- function(x) {
  check_actual <- as.integer(str_sub(x, 9, 9))
  check <- str_sub(x, 0, 7) %>%
    str_split("") %>%
    yt_cs_map()
  coalesce(check_actual == check, FALSE)
}

regex_id <- "\\d{6}[A+-]\\d{3}[:alnum:]"
regex_ovt <- "0037\\d{8}\\d{0,5}"
regex_yt <- "\\d{7}-\\d"
regex_vat_fi <- "FI\\d{8}"
regex_vat_all <- function() str_c("(", str_c(vat_regexes$format, collapse = "|"), ")")


handle_locate <- function(reg, locate) if (locate) reg else str_c("^", reg, "$")

.onAttach <- function(...) {
  ver <- utils::packageVersion("fino")
  packageStartupMessage("This is fino version ", ver)
}
