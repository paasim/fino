% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract}
\alias{extract}
\alias{extract_yt}
\alias{extract_ovt}
\alias{extract_vat}
\alias{extract_pic}
\title{Extract a valid ID Number from a string.}
\usage{
extract_yt(x, require_checksum = TRUE)

extract_ovt(x, require_checksum = TRUE)

extract_vat(x, require_checksum = TRUE)

extract_pic(x, require_checksum = TRUE)
}
\arguments{
\item{x}{The input string(s).}

\item{require_checksum}{If \code{TRUE} (default), require that the checksum
digit matches in addition to the format of the string. Note, does not make
sense when used to validate non-Finnish VAT-numbers.}
}
\value{
A character vector containing the only the ID-parts of the
string(s).
}
\description{
Functions for extracting a valid Business ID, VAT Number, E-Invoicing
address or a personal identification code (PIC) if it is contained in the input string. Rules
obtained from \url{http://tarkistusmerkit.teppovuori.fi/tarkmerk.htm}.
}
