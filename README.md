# fino

[![Build Status](https://travis-ci.org/paasim/fino.svg?branch=master)](https://travis-ci.org/paasim/fino)
[![codecov](https://codecov.io/gh/paasim/fino/branch/master/graphs/badge.svg)](https://codecov.io/gh/paasim/fino)

An R package for generating, validating and converting between Finnish ID numbers. Currently supported 'types' include ID, Business ID, VAT Number and E-invoicing address.

Installation
------------

    devtools::install_github('paasim/fino')


Usage example
-------------
    
    library(fino)
    # generate three syntactically valid Finnish Business IDs
    ids <- gen_yt(3)
    all(valid_yt(ids))


