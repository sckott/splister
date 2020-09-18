splister
========



[![Build Status](https://api.travis-ci.org/sckott/splister.png?branch=master)](https://travis-ci.org/sckott/splister)

`splister` xxx

## Installation


```r
devtools::install_github("sckott/splister")
```


```r
library('splister')
```

## Examples


```r
x <- read.csv(system.file("examples", "iucn_dat.csv", package = "splister"), stringsAsFactors = FALSE)[,-1]
y <- system.file("examples", "worms_sample.csv", package = "splister")
spp <- x$sciname[1:20000L]
res <- matcher(spplist = spp, ref = y, against = "scientificName")
df <- data.frame(original = spp, taxon = unlist(res),
   match = sapply(res, attr, "match"))
df[df$match == "replace", ]

#>                      original              taxon   match
#> 2080           Alburnus mento Alburnus mentoides replace
#> 7080           Bahaba chaptis Alburnus mentoides replace
#> 12080           Caridina evae Alburnus mentoides replace
#> 17080 Copadichromis inornatus Alburnus mentoides replace
```

## Meta

* Please [report any issues or bugs](https://github.com/sckott/splister/issues).
* License: MIT
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
