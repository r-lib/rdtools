# Packages searched for help by default

`pkg_search_attached()` returns the packages a bare `?topic` can see:
every attached package, in search path order, followed by the base
packages that are always available. This is the default search set for
[`topic_find()`](https://rdtools.r-lib.org/reference/topic_find.md).

`pkg_search_base()` returns the base packages that are always searched.

## Usage

``` r
pkg_search_attached()

pkg_search_base()
```

## Value

A character vector of package names.

## Examples

``` r
pkg_search_attached()
#>  [1] "rdtools"   "stats"     "graphics"  "grDevices" "utils"    
#>  [6] "datasets"  "methods"   "base"      "tools"     "grid"     
#> [11] "splines"   "stats4"    "tcltk"     "compiler"  "parallel" 
pkg_search_base()
#>  [1] "base"      "tools"     "utils"     "grDevices" "graphics" 
#>  [6] "stats"     "datasets"  "methods"   "grid"      "splines"  
#> [11] "stats4"    "tcltk"     "compiler"  "parallel" 
```
