# convert x into a vector with the matches and the other elements as NA
# ignore makes it possible to ignore some characters such as "-".
invalid_to_na <- function(x, regex, locate) {
  if (locate) x <- map_chr(x, ~str_extract(.x, regex))
  valid_format <- if_else(is.na(x), FALSE, str_detect(x, str_c("^", regex, "$")))
  if_else(valid_format, x, NA_character_)
}

# map for the checksum in the finnish id number
id_cs_map <- function(x) {
  check_vec <- c(0:9, LETTERS[c(1:6, 8, 10:14, 16, 18:25)])
  check_vec[(x %% 31)+1]
}

regex_id <- "\\d{6}[\\+\\-A]\\d{3}[:alnum:]"
regex_ovt <- "0037\\d{8}\\d{0,5}"
regex_yt <- "\\d{7}-\\d"
regex_vat <- function() str_c(vat_regexes$format, collapse = "|")

# map for the checksum in the Business ID
yt_cs_map <- function(x) {
  sum(as.integer(x) * c(7L, 9L, 10L, 5L, 8L, 4L, 2L)) %% 11L
}

.onAttach <- function(...) {
  ver <- utils::packageVersion("fino")
  packageStartupMessage("This is fino version ", ver)
}
