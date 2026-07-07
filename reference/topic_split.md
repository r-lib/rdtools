# Split a qualified topic into package and topic

Splits `"pkg::topic"` (or `"pkg:::topic"`) into its package and topic
components. The prefix is only treated as a qualifier when it is a
syntactically valid package name, so aliases that merely contain `::`
(e.g. S7 method aliases like `"speak,foo::Dog-method"`) are left intact.

## Usage

``` r
topic_split(topic)
```

## Arguments

- topic:

  A single string.

## Value

A list with elements `package` (a string, or `NULL` if the topic is
unqualified) and `topic`.

## Examples

``` r
topic_split("stats::rnorm")
#> $package
#> [1] "stats"
#> 
#> $topic
#> [1] "rnorm"
#> 
topic_split("rnorm")
#> $package
#> NULL
#> 
#> $topic
#> [1] "rnorm"
#> 
topic_split("speak,foo::Dog-method")
#> $package
#> NULL
#> 
#> $topic
#> [1] "speak,foo::Dog-method"
#> 
```
