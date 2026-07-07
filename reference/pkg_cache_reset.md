# Reset cached package indexes

Clears the cached topic index, parsed Rd objects, and dependency search
set for `package`, or for every package when `package` is `NULL`. This
will generally be called automatically by roxygen2 for source packages.
Installed package indexes are automatically reset when their namespace
is unloaded.

## Usage

``` r
pkg_cache_reset(package = NULL)
```

## Arguments

- package:

  A package name, or a path to the source directory of a package. Use
  `NULL`, the default, to reset every cached index.

## Value

`NULL`, invisibly.

## Examples

``` r
head(pkg_topics("stats"))
#>    stats-package  .checkMFClasses      .getXlevels          .lm.fit 
#>  "stats-package" "checkMFClasses" "checkMFClasses"          "lmfit" 
#>         .MFclass    .nknots.smspl 
#> "checkMFClasses"  "smooth.spline" 
pkg_cache_reset("stats")

# Reset every cached index
pkg_cache_reset()
```
