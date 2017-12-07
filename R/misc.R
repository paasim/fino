# convert x into a vector with the matches and the other elements as NA
# ignore makes it possible to ignore some characters such as "-".
invalid_to_na <- function(x, regex, locate, ignore = NULL) {
  x <- toupper(x) %>%
    str_replace_all(set_names("", str_c("[^A-Z0-9]", ignore, "]")))
  if (locate) {
    x <- map_if(x, ~is.na(.x) | str_detect(.x, regex),
                ~str_extract(.x, regex)) %>%
      unlist()
  }
  valid_format <- ifelse(is.na(x), FALSE, str_detect(x, str_c("^", regex, "$")))
  ifelse(valid_format, x, NA)
}
str_map <- function(str, x) unname(str[x])

# map for the checksum in the finnish id number
id_cs_map <- function(x) {
  set_names(c(0:9, LETTERS[c(1:6, 8, 10:14, 16, 18:25)]), 0:30) %>%
    str_map(as.character(x %% 31))
}

regex_id <- "[0-9]{6}[\\+\\-A][0-9]{3}[A-Z0-9]"
regex_ovt <- "0037[0-9]{8}[0-9]{0,5}"
regex_yt <- "[0-9]{7}-[0-9]"
regex_vat <- function() str_c(vat_regexes$format, collapse = "|")

# map for the checksum in the Business ID
yt_cs_map <- function(x) {
  coefs <- c(7L, 9L, 10L, 5L, 8L, 4L, 2L)
  sum(as.integer(x)*coefs) %% 11L
}

.onAttach <- function(...) {
  ver <- utils::packageVersion("fino")
  packageStartupMessage("This is fino version ", ver)
}
