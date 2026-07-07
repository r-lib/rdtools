# List the topics documented by a package

`pkg_topics()` returns a named character vector mapping every alias
(i.e. everything you can type after `?`) to the name of the Rd file
(without extension) that documents it. It understands three kinds of
package:

- **Installed** packages, read from their `help/aliases.rds` index.

- **In-development** packages loaded with
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html),
  indexed from the `\alias{}` commands in their source `man/` directory.

- **Source** packages, when `package` is a path to a package directory
  rather than a name.

Indexes remain cached until
[`pkg_cache_reset()`](https://rdtools.r-lib.org/reference/pkg_cache_reset.md)
is called. Installed package indexes are also reset automatically when
their namespace is unloaded.

For source packages, `\alias{}` extraction is line-based: any number of
aliases may appear anywhere on a line, but an alias must open and close
on the same line, and aliases produced by Rd macros are not seen.

## Usage

``` r
pkg_topics(package)
```

## Arguments

- package:

  A package name, or a path to the source directory of a package.

## Value

A named character vector mapping alias to Rd file name.

## Examples

``` r
head(pkg_topics("stats"))
#>    stats-package  .checkMFClasses      .getXlevels          .lm.fit 
#>  "stats-package" "checkMFClasses" "checkMFClasses"          "lmfit" 
#>         .MFclass    .nknots.smspl 
#> "checkMFClasses"  "smooth.spline" 
```
