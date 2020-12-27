# fino

[![R build status](https://github.com/paasim/fino/workflows/R-CMD-check/badge.svg)](https://github.com/paasim/fino/actions)
[![codecov](https://codecov.io/gh/paasim/fino/branch/master/graphs/badge.svg)](https://codecov.io/gh/paasim/fino)

An R package for generating, validating and converting between Finnish ID numbers. Currently supported 'types' include personal identification code (PIC), Business ID, VAT Number and E-invoicing address.

Installation
------------

    devtools::install_github('paasim/fino')


Usage example
-------------
    
    library(fino)
    # generate three syntactically valid Finnish Business IDs
    ids <- gen_yt(3)
    all(valid_yt(ids))


