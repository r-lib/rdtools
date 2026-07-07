# Get the parsed Rd for a topic

Retrieves the parsed Rd object documenting `topic` in `package`. For
installed packages the topic is fetched lazily from the package's help
database (no parsing needed); for source and in-development packages the
Rd file is parsed with
[`tools::parse_Rd()`](https://rdrr.io/r/tools/parse_Rd.html), with the
package's Rd macros loaded. Results are cached per topic, so repeated
access (e.g. roxygen2 inheriting several fields from one topic) only
pays once.

## Usage

``` r
topic_rd(topic, package)
```

## Arguments

- topic:

  A single string.

- package:

  A package name, or a path to the source directory of a package.

## Value

An Rd object: a recursive structure of class `"Rd"`, as returned by
[`tools::parse_Rd()`](https://rdrr.io/r/tools/parse_Rd.html). Returns
`NULL` if the package or topic doesn't exist; use
[`topic_exists()`](https://rdtools.r-lib.org/reference/topic_exists.md)
first if you need to distinguish that from other problems.

## Examples

``` r
rd <- topic_rd("rnorm", "stats")
class(rd)
#> [1] "Rd"
```
