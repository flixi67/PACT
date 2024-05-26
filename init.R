my_packages = c("tippy", "gtable", "bslib", "ggplot2", "htmlwidgets", "lattice",
                "tzdb", "vctrs", "tools", "crosstalk", "generics", "curl", "tibble",
                "proxy", "fansi", "pkgconfig", "methods", "KernSmooth", "tidyverse",
                "leaflet", "lifecycle", "farver", "compiler", "stringr", "munsell",
                "ggforce", "httpuv", "htmltools", "class", "sass", "pillar", "later",
                "jquerylib", "tidyr", "MASS", "openssl", "rsconnect", "classInt",
                "cachem", "mime", "tidyselect", "digest", "stringi", "sf", "dplyr",
                "purrr", "forcats", "polyclip", "fastmap", "grid", "colorspace",
                "cli", "magrittr", "grDevices", "utf8", "e1071", "readr", "withr",
                "utils", "scales", "promises", "lubridate", "timechange", "askpass",
                "datasets", "zoo", "stats", "hms", "graphics", "shiny", "rlang",
                "Rcpp", "xtable", "glue", "DBI", "tweenr", "renv", "rstudioapi",
                "base", "jsonlite", "R6", "units")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
