# Find packages that document a topic

`topic_find()` looks `topic` up in each of `packages` in order and
returns the first hit. `topic_find_all()` returns every hit. Lookups use
the cached per-package indexes built by
[`pkg_topics()`](https://rdtools.r-lib.org/reference/pkg_topics.md), so
scanning even a long search set is cheap.

## Usage

``` r
topic_find(topic, packages = pkg_search_attached())

topic_find_all(topic, packages = pkg_search_attached())
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

`topic_find()` returns `NULL` if the topic isn't found; otherwise a list
with elements:

- `package`: the package that documents the topic.

- `file`: the name of the Rd file (without extension).

`topic_find_all()` returns a data frame with columns `package` and
`file`, containing one row per hit (and no rows if the topic isn't
found).

## Examples

``` r
topic_find("rnorm")
#> $package
#> [1] "stats"
#> 
#> $file
#> [1] "Normal"
#> 
topic_find("mean", c("stats", "base"))
#> $package
#> [1] "base"
#> 
#> $file
#> [1] "mean"
#> 
topic_find_all("plot", c("graphics", "base"))
#>    package         file
#> 1 graphics plot.default
#> 2     base         plot
topic_find("no-such-topic")
#> NULL
```
