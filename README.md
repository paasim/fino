# fino

An R package for generating, validating and converting between Finnish ID numbers. Currently supported 'types' include ID, Business ID, VAT Number and E-invoicing address.

MVP, tests and documentation imcomplete.

Installation
------------

    devtools::install_github('paasim/fino')


Usage example
-------------
    
    library(fino)
    all(valid_id(gen_id(13)))


