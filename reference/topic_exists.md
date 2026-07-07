# Is a topic documented?

Checks whether any of `packages` has an exact alias matching `topic`.

## Usage

``` r
topic_exists(topic, packages = pkg_search_attached())
```

## Arguments

- topic:

  A single string naming an alias, matched exactly. Use
  [`topic_split()`](https://rdtools.r-lib.org/reference/topic_split.md)
  first if you need to handle qualified topics like `"pkg::foo"`.

- packages:

  A character vector of package names (and/or source package paths) to
  search, in order. Defaults to
  [`pkg_search_attached()`](https://rdtools.r-lib.org/reference/pkg_search_attached.md).
  Unavailable packages are skipped.

## Value

A single `TRUE` or `FALSE`.

## Examples

``` r
topic_exists("rnorm", "stats")
#> [1] TRUE
topic_exists("rnorm", c("base", "stats"))
#> [1] TRUE
topic_exists("median")
#> [1] TRUE
topic_exists("not-a-topic", "stats")
#> [1] FALSE
```
