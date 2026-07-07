# Find the package where a topic's object originates

Determines which package supplies the object associated with a
documented topic. This resolves re-exported functions and imported
objects to the package where they originate. Results are cached
alongside the package's topic index, so repeated lookups are cheap.

## Usage

``` r
topic_origin(topic, package)
```

## Arguments

- topic:

  A single string naming a topic.

- package:

  A package name.

## Value

A single package name.

## Examples

``` r
topic_origin("rnorm", "stats")
#> [1] "stats"
```
