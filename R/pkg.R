#' List the topics documented by a package
#'
#' @description
#' `pkg_topics()` returns a named character vector mapping every alias
#' (i.e. everything you can type after `?`) to the name of the Rd file
#' (without extension) that documents it. It understands three kinds of
#' package:
#'
#' * **Installed** packages, read from their `help/aliases.rds` index.
#' * **In-development** packages loaded with `pkgload::load_all()`,
#'   indexed from the `\alias{}` commands in their source `man/` directory.
#' * **Source** packages, when `package` is a path to a package directory
#'   rather than a name.
#'
#' Indexes are cached, and revalidated against file modification times on
#' every access, so repeated lookups are cheap and edits to `man/` are
#' picked up automatically.
#'
#' For source packages, `\alias{}` extraction is line-based: any number of
#' aliases may appear anywhere on a line, but an alias must open and close
#' on the same line, and aliases produced by Rd macros are not seen.
#'
#' @param package A package name, or a path to the source directory of a
#'   package.
#' @returns A named character vector mapping alias to Rd file name.
#' @export
#' @examples
#' head(pkg_topics("stats"))
pkg_topics <- function(package) {
  index(package)$topics
}

#' Packages searched for help by default
#'
#' The packages a bare `?topic` can see: every attached package, in search
#' path order, followed by the base packages that are always available.
#' This is the default search set for [topic_find()].
#'
#' @returns A character vector of package names.
#' @export
#' @examples
#' pkg_search_path()
pkg_search_path <- function() {
  attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  always <- c("datasets", "utils", "grDevices", "graphics", "stats", "base")
  unique(c(attached, always))
}
