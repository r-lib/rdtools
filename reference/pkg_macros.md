# Load a package's Rd macros

Loads the macros declared by the package's `RdMacros` field and
`man/macros/` directory, layered over R's system macros. This reproduces
the macro environment used when installing a package.

## Usage

``` r
pkg_macros(package)
```

## Arguments

- package:

  A package name, or a path to the source directory of a package.

## Value

An environment containing the Rd macro definitions.

## Examples

``` r
macros <- pkg_macros("stats")
head(ls(macros))
#> [1] "\\CRANpkg"  "\\I"        "\\LaTeX"    "\\PR"       "\\bibcitep"
#> [6] "\\bibcitet"
```
