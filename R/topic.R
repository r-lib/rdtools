#' Split a qualified topic into package and topic
#'
#' Splits `"pkg::topic"` (or `"pkg:::topic"`) into its package and topic
#' components. The prefix is only treated as a qualifier when it is a
#' syntactically valid package name, so aliases that merely contain `::`
#' (e.g. S7 method aliases like `"speak,foo::Dog-method"`) are left intact.
#'
#' @param topic A single string.
#' @returns A list with elements `package` (a string, or `NULL` if the
#'   topic is unqualified) and `topic`.
#' @export
#' @examples
#' topic_parse("stats::rnorm")
#' topic_parse("rnorm")
#' topic_parse("speak,foo::Dog-method")
topic_parse <- function(topic) {
  check_string(topic)

  match <- regmatches(
    topic,
    regexec("^([a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]):{2,3}(.+)$", topic)
  )[[1]]

  if (length(match) == 3) {
    list(package = match[[2]], topic = match[[3]])
  } else {
    list(package = NULL, topic = topic)
  }
}

#' Find packages that document a topic
#'
#' @description
#' `topic_find()` looks `topic` up in each of `packages` in order and returns
#' the first hit. `topic_find_all()` returns every hit. Lookups use the cached
#' per-package indexes built by [pkg_topics()], so scanning even a long search
#' set is cheap.
#'
#' @param topic A single string naming an alias, matched exactly.
#'   Use [topic_parse()] first if you need to handle qualified topics like
#'   `"pkg::foo"`.
#' @param packages A character vector of package names (and/or source
#'   package paths) to search, in order. Defaults to [pkg_search_attached()].
#'   Unavailable packages are skipped.
#' @returns `topic_find()` returns `NULL` if the topic isn't found; otherwise a
#'   list with elements:
#'   * `package`: the package that documents the topic.
#'   * `file`: the name of the Rd file (without extension).
#'
#'   `topic_find_all()` returns a list of these results, or an empty list if the
#'   topic isn't found.
#' @export
#' @examples
#' topic_find("rnorm")
#' topic_find("mean", c("stats", "base"))
#' topic_find_all("plot", c("graphics", "base"))
#' topic_find("no-such-topic")
topic_find <- function(topic, packages = pkg_search_attached()) {
  found <- topic_find_all(topic, packages)
  if (length(found) == 0) NULL else found[[1]]
}

#' @rdname topic_find
#' @export
topic_find_all <- function(topic, packages = pkg_search_attached()) {
  check_string(topic)
  check_character(packages)

  found <- list()
  for (package in packages) {
    if (!is_pkg_path(package) && !pkg_is_installed(package)) {
      next
    }
    entry <- index(package)
    file <- get0(topic, envir = entry$env, inherits = FALSE)
    if (!is.null(file)) {
      found[[length(found) + 1]] <- list(package = entry$name, file = file)
    }
  }
  found
}

#' Find the package qualifier for a topic
#'
#' Determines whether a topic needs a package qualifier when linking from the
#' documentation of another package. The current package is checked first,
#' followed by `dependencies`, and then the base packages. Re-exported objects
#' are attributed to their original package.
#'
#' @param topic A single string naming an alias, matched exactly.
#' @param package The name or source directory of the current package.
#' @param dependencies A character vector of dependencies to search.
#'   [Base packages][pkgs_search_base] are always included.
#' @returns A character vector with special length and missingness semantics:
#'   * `NA_character_` means the topic was found but needs no qualification.
#'   * `character()` means the topic was not found.
#'   * One package name means the topic has one unambiguous qualifier.
#'   * Multiple package names mean the topic is ambiguous.
#' @export
#' @examples
#' topic_find_package("rnorm", "stats", character())
topic_find_package <- function(topic, package, dependencies) {
  check_string(topic)
  check_string(package)
  check_character(dependencies)

  if (topic_has(package, topic)) {
    return(NA_character_)
  }

  packages <- unique(c(dependencies, pkgs_search_base()))
  matches <- topic_find_all(topic, packages)
  found <- vapply(matches, `[[`, character(1), "package")
  found <- unique(vapply(found, topic_source, character(1), topic = topic))

  base <- pkgs_search_base()
  if (length(found) == 0) {
    character()
  } else if (length(found) == 1) {
    if (found %in% base) NA_character_ else found
  } else if (all(found %in% base)) {
    NA_character_
  } else {
    found
  }
}

topic_has <- function(package, topic) {
  !is.null(topic_find(topic, package))
}

topic_source <- function(package, topic) {
  if (package %in% pkgs_search_base()) {
    return(package)
  }

  ns <- asNamespace(package)
  if (!exists(topic, envir = ns, inherits = TRUE)) {
    return(package)
  }

  object <- get(topic, envir = ns, inherits = TRUE)
  if (is.primitive(object)) {
    return("base")
  }
  if (is.function(object)) {
    env <- environment(object)
    if (isNamespace(env)) {
      return(getNamespaceName(env))
    }
    return(package)
  }

  imports <- getNamespaceImports(ns)
  imports <- imports[names(imports) != ""]
  matches <- vapply(imports, `%in%`, logical(1), x = topic)
  if (!any(matches)) {
    return(package)
  }

  packages <- names(matches)[matches]
  packages[[length(packages)]]
}

#' Get the parsed Rd for a topic
#'
#' Retrieves the parsed Rd object documenting `topic` in `package`. For
#' installed packages the topic is fetched lazily from the package's help
#' database (no parsing needed); for source and in-development packages the
#' Rd file is parsed with [tools::parse_Rd()], with the package's Rd macros
#' loaded. Results are cached per topic, so repeated access (e.g. roxygen2
#' inheriting several fields from one topic) only pays once.
#'
#' @inheritParams topic_parse
#' @param package A package name, or a path to the source directory of a
#'   package.
#' @returns An Rd object: a recursive structure of class `"Rd"`, as returned
#'   by [tools::parse_Rd()].
#' @export
#' @examples
#' rd <- topic_rd("rnorm", "stats")
#' class(rd)
topic_rd <- function(topic, package) {
  check_string(topic)

  entry <- index(package)
  file <- get0(topic, envir = entry$env, inherits = FALSE)
  if (is.null(file)) {
    stop(sprintf("Can't find topic '%s' in package '%s'.", topic, entry$name))
  }

  rd <- get0(file, envir = entry$rd, inherits = FALSE)
  if (is.null(rd)) {
    rd <- switch(
      entry$backend,
      installed = rd_fetch_installed(entry$path, entry$name, file),
      source = rd_parse_source(entry, file)
    )
    if (!inherits(rd, "Rd")) {
      class(rd) <- "Rd"
    }
    assign(file, rd, envir = entry$rd)
  }
  rd
}

# Lazily fetch one topic from an installed package's help database
# (help/<pkg>.rdb + .rdx). Same mechanics as tools:::fetchRdDB(), but
# built on the exported base lazy-load primitives so we don't depend on
# unexported internals.
rd_fetch_installed <- function(pkg_path, package, file) {
  filebase <- file.path(pkg_path, "help", package)
  lazyLoadDBexec(filebase, function(db) {
    if (!file %in% db$vars) {
      stop(
        sprintf(
          "Rd file '%s' missing from help database for '%s'.",
          file,
          package
        ),
        call. = FALSE
      )
    }
    lazyLoadDBfetch(db$vals[file][[1L]], db$datafile, db$compressed, db$envhook)
  })
}

rd_parse_source <- function(entry, file) {
  if (is.null(entry$macros)) {
    entry$macros <- rd_macros(entry$path)
  }
  tools::parse_Rd(
    entry$files[[file]],
    macros = entry$macros,
    encoding = "UTF-8"
  )
}

# The package's own Rd macros (RdMacros field + man/macros/), layered over
# R's system macros — reproduces what R does at install time.
rd_macros <- function(pkg_path) {
  macros <- tools::loadPkgRdMacros(pkg_path)
  tools::loadRdMacros(
    file.path(R.home("share"), "Rd", "macros", "system.Rd"),
    macros = macros
  )
}
