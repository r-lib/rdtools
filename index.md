# rdtools

rdtools provides fast, cached lookup of help topics and aliases across
installed, source, and in-development packages, plus efficient retrieval
of parsed Rd objects. It’s low-level infrastructure designed to be
shared by documentation tools like roxygen2, pkgdown, pkgload, and
downlit, which currently each implement their own variant of this
machinery.

## Installation

You can install the development version of rdtools from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("r-lib/rdtools")
```

## Usage

[`pkg_topics()`](https://rdtools.r-lib.org/reference/pkg_topics.md) maps
every alias a package documents to the Rd file that documents it:

``` r

library(rdtools)

head(pkg_topics("stats"))
#>    stats-package  .checkMFClasses      .getXlevels          .lm.fit 
#>  "stats-package" "checkMFClasses" "checkMFClasses"          "lmfit" 
#>         .MFclass    .nknots.smspl 
#> "checkMFClasses"  "smooth.spline"
```

[`topic_find()`](https://rdtools.r-lib.org/reference/topic_find.md)
tells you which package documents a topic, searching the attached
packages by default:

``` r

topic_find("rnorm")
#> $package
#> [1] "stats"
#> 
#> $file
#> [1] "Normal"

topic_split("dplyr::across")
#> $package
#> [1] "dplyr"
#> 
#> $topic
#> [1] "across"
```

[`topic_rd()`](https://rdtools.r-lib.org/reference/topic_rd.md)
retrieves the parsed Rd for a topic, fetching it lazily from the help
database for installed packages, or parsing `man/*.Rd` for source
packages:

``` r

rd <- topic_rd("rnorm", "stats")
class(rd)
#> [1] "Rd"
```

All lookups are backed by per-package indexes that remain cached until
explicitly reset. Installed package indexes are reset when their
namespace is unloaded, while source package indexes can be reset with
[`pkg_cache_reset()`](https://rdtools.r-lib.org/reference/pkg_cache_reset.md).
This makes lookups cheap enough to call in a tight loop (e.g. once per
link while rendering documentation).
