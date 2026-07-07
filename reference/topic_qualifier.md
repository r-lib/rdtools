# Find the package qualifier for a topic

Determines whether a topic needs a package qualifier when linking from
the documentation of another package. The `from` package is checked
first, followed by `packages`, and then the base packages. Re-exported
objects are attributed to their original package. Results are cached
alongside the `from` package's topic index, so repeated lookups are
cheap.

## Usage

``` r
topic_qualifier(topic, from, packages = character())
```

## Arguments

- topic:

  A single string naming an alias, matched exactly.

- from:

  The name or source directory of the package you are linking from. If
  it documents `topic`, no qualifier is needed.

- packages:

  A character vector of additional packages to search, typically the
  dependencies of `from` (as computed by
  [`pkg_search_deps()`](https://rdtools.r-lib.org/reference/pkg_search_deps.md)).
  [Base
  packages](https://rdtools.r-lib.org/reference/pkg_search_attached.md)
  are always included.

## Value

- `NULL` if the topic isn't documented by `from`, `packages`, or the
  base packages.

- `NA_character_` if the topic needs no qualifier because it's
  documented by `from` or a base package.

- Otherwise, the package name(s) that could qualify the topic: usually
  one, but more if the topic is ambiguous, in which case it's up to the
  caller to decide how to respond.

## Examples

``` r
topic_qualifier("rnorm", "stats")
#> [1] NA
```
