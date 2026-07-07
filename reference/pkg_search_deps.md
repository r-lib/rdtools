# Packages to search for topics in a package's dependencies

`pkg_search_deps()` returns the packages whose topics `package`'s
documentation can link to: the packages declared in its `Depends`,
`Imports`, and `Suggests` fields, followed by the [base
packages](https://rdtools.r-lib.org/reference/pkg_search_attached.md),
which are always available. The result is cached alongside the package's
topic index, so it remains valid until
[`pkg_cache_reset()`](https://rdtools.r-lib.org/reference/pkg_cache_reset.md)
is called.

## Usage

``` r
pkg_search_deps(package)
```

## Arguments

- package:

  A package name, or a path to the source directory of a package.

## Value

A character vector of package names.

## Examples

``` r
pkg_search_deps("stats")
#>  [1] "utils"     "grDevices" "graphics"  "MASS"      "Matrix"   
#>  [6] "SuppDists" "methods"   "stats4"    "base"      "tools"    
#> [11] "stats"     "datasets"  "grid"      "splines"   "tcltk"    
#> [16] "compiler"  "parallel" 
```
