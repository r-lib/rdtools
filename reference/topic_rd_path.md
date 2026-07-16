# Get the path to the Rd file for a topic

Returns the path to the `.Rd` file that documents `topic`. Rd files only
exist on disk for source and in-development packages; installed packages
store their documentation in a binary database, so `topic_rd_path()`
returns `NULL` for them. Use
[`topic_rd()`](https://rdtools.r-lib.org/reference/topic_rd.md) if you
want the parsed contents regardless of where they are stored.

## Usage

``` r
topic_rd_path(topic, package)
```

## Arguments

- topic:

  A single string.

- package:

  A package name, or a path to the source directory of a package.

## Value

The path to a `.Rd` file, or `NULL` if the package or topic doesn't
exist, or if the package doesn't have Rd files on disk.

## Examples

``` r
# Rd files only exist on disk for source packages, so make a minimal one
pkg <- tempfile()
dir.create(file.path(pkg, "man"), recursive = TRUE)
writeLines("Package: demo", file.path(pkg, "DESCRIPTION"))
writeLines(
  c("\\name{foo}", "\\alias{foo}", "\\title{Foo}", "\\description{Foo.}"),
  file.path(pkg, "man", "foo.Rd")
)

topic_rd_path("foo", pkg)
#> [1] "/tmp/RtmpAFx903/file198946367d00/man/foo.Rd"
```
