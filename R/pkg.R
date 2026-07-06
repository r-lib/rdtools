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
#' Indexes remain cached until [pkg_cache_reset()] is called. Installed package
#' indexes are also reset automatically when their namespace is unloaded.
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

#' Load a package's Rd macros
#'
#' Loads the macros declared by the package's `RdMacros` field and
#' `man/macros/` directory, layered over R's system macros. This reproduces the
#' macro environment used when installing a package.
#'
#' @param package A package name, or a path to the source directory of a
#'   package.
#' @returns An environment containing the Rd macro definitions.
#' @export
#' @examples
#' macros <- pkg_macros("stats")
#' head(ls(macros))
pkg_macros <- function(package) {
  check_string(package)
  path <- index_resolve(package)$path
  macros <- tools::loadPkgRdMacros(path)
  tools::loadRdMacros(
    file.path(R.home("share"), "Rd", "macros", "system.Rd"),
    macros = macros
  )
}

#' Packages searched for help by default
#'
#' @description
#' `pkg_search_attached()` returns the packages a bare `?topic` can see: every
#' attached package, in search path order, followed by the base packages that
#' are always available. This is the default search set for [topic_find()].
#'
#' `pkg_search_base()` returns the base packages that are always searched.
#'
#' @returns A character vector of package names.
#' @export
#' @examples
#' pkg_search_attached()
#' pkg_search_base()
pkg_search_attached <- function() {
  attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  unique(c(attached, pkg_search_base()))
}

#' @rdname pkg_search_attached
#' @export
pkg_search_base <- function() {
  if (getRversion() >= "4.4.0") {
    tools::standard_package_names()[["base"]]
  } else {
    c(
      "base",
      "compiler",
      "datasets",
      "graphics",
      "grDevices",
      "grid",
      "methods",
      "parallel",
      "splines",
      "stats",
      "stats4",
      "tcltk",
      "tools",
      "utils"
    )
  }
}
