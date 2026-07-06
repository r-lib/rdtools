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
#' topic_split("stats::rnorm")
#' topic_split("rnorm")
#' topic_split("speak,foo::Dog-method")
topic_split <- function(topic) {
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
#'   Use [topic_split()] first if you need to handle qualified topics like
#'   `"pkg::foo"`.
#' @param packages A character vector of package names (and/or source
#'   package paths) to search, in order. Defaults to [pkg_search_attached()].
#'   Unavailable packages are skipped.
#' @returns `topic_find()` returns `NULL` if the topic isn't found; otherwise a
#'   list with elements:
#'   * `package`: the package that documents the topic.
#'   * `file`: the name of the Rd file (without extension).
#'
#'   `topic_find_all()` returns a data frame with columns `package` and
#'   `file`, containing one row per hit (and no rows if the topic isn't
#'   found).
#' @export
#' @examples
#' topic_find("rnorm")
#' topic_find("mean", c("stats", "base"))
#' topic_find_all("plot", c("graphics", "base"))
#' topic_find("no-such-topic")
topic_find <- function(topic, packages = pkg_search_attached()) {
  found <- topic_find_all(topic, packages)
  if (nrow(found) == 0) {
    NULL
  } else {
    list(package = found$package[[1]], file = found$file[[1]])
  }
}

#' @rdname topic_find
#' @export
topic_find_all <- function(topic, packages = pkg_search_attached()) {
  check_string(topic)
  check_character(packages)

  found_package <- character()
  found_file <- character()
  for (package in packages) {
    if (!is_pkg_path(package) && !pkg_is_installed(package)) {
      next
    }
    entry <- index(package)
    file <- get0(topic, envir = entry$env, inherits = FALSE)
    if (!is.null(file)) {
      found_package <- c(found_package, entry$name)
      found_file <- c(found_file, file)
    }
  }
  data.frame(package = found_package, file = found_file)
}

#' Find the package qualifier for a topic
#'
#' Determines whether a topic needs a package qualifier when linking from the
#' documentation of another package. The `from` package is checked first,
#' followed by `packages`, and then the base packages. Re-exported objects
#' are attributed to their original package.
#'
#' @param topic A single string naming an alias, matched exactly.
#' @param from The name or source directory of the package you are linking
#'   from. If it documents `topic`, no qualifier is needed.
#' @param packages A character vector of additional packages to search,
#'   typically the dependencies of `from`. [Base
#'   packages][pkg_search_base] are always included.
#' @returns
#' * `NULL` if the topic isn't documented by `from`, `packages`, or the base
#'   packages.
#' * `NA_character_` if the topic needs no qualifier because it's documented
#'   by `from` or a base package.
#' * Otherwise, the package name(s) that could qualify the topic: usually
#'   one, but more if the topic is ambiguous, in which case it's up to the
#'   caller to decide how to respond.
#' @export
#' @examples
#' topic_qualifier("rnorm", "stats")
topic_qualifier <- function(topic, from, packages = character()) {
  check_string(topic)
  check_string(from)
  check_character(packages)

  if (topic_exists(topic, from)) {
    return(NA_character_)
  }

  base <- pkg_search_base()
  matches <- topic_find_all(topic, unique(c(packages, base)))
  found <- unique(
    vapply(matches$package, \(pkg) topic_origin(topic, pkg), character(1))
  )

  if (length(found) == 0) {
    NULL
  } else if (all(found %in% base)) {
    NA_character_
  } else {
    found
  }
}

#' Is a topic documented?
#'
#' Checks whether any of `packages` has an exact alias matching `topic`.
#'
#' @inheritParams topic_find
#' @returns A single `TRUE` or `FALSE`.
#' @export
#' @examples
#' topic_exists("rnorm", "stats")
#' topic_exists("rnorm", c("base", "stats"))
#' topic_exists("median")
#' topic_exists("not-a-topic", "stats")
topic_exists <- function(topic, packages = pkg_search_attached()) {
  !is.null(topic_find(topic, packages))
}

#' Find the package where a topic's object originates
#'
#' Determines which package supplies the object associated with a documented
#' topic. This resolves re-exported functions and imported objects to the
#' package where they originate.
#'
#' @param topic A single string naming a topic.
#' @param package A package name.
#' @returns A single package name.
#' @export
#' @examples
#' topic_origin("rnorm", "stats")
topic_origin <- function(topic, package) {
  check_string(topic)
  check_string(package)

  if (package %in% pkg_search_base()) {
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
      return(unname(getNamespaceName(env)))
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
#' @inheritParams topic_split
#' @param package A package name, or a path to the source directory of a
#'   package.
#' @returns An Rd object: a recursive structure of class `"Rd"`, as returned
#'   by [tools::parse_Rd()]. Returns `NULL` if the package or topic doesn't
#'   exist; use [topic_exists()] first if you need to distinguish that from
#'   other problems.
#' @export
#' @examples
#' rd <- topic_rd("rnorm", "stats")
#' class(rd)
topic_rd <- function(topic, package) {
  check_string(topic)
  check_string(package)

  if (!is_pkg_path(package) && !pkg_is_installed(package)) {
    return(NULL)
  }
  entry <- index(package)
  file <- get0(topic, envir = entry$env, inherits = FALSE)
  if (is.null(file)) {
    return(NULL)
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

#' Get the path to the Rd file for a topic
#'
#' Returns the path to the `.Rd` file that documents `topic`. Rd files only
#' exist on disk for source and in-development packages; installed packages
#' store their documentation in a binary database, so `topic_rd_path()`
#' returns `NULL` for them. Use [topic_rd()] if you want the parsed contents
#' regardless of where they are stored.
#'
#' @inheritParams topic_rd
#' @returns The path to a `.Rd` file, or `NULL` if the package or topic
#'   doesn't exist, or if the package doesn't have Rd files on disk.
#' @export
#' @examples
#' # Rd files only exist on disk for source packages, so make a minimal one
#' pkg <- tempfile()
#' dir.create(file.path(pkg, "man"), recursive = TRUE)
#' writeLines("Package: demo", file.path(pkg, "DESCRIPTION"))
#' writeLines(
#'   c("\\name{foo}", "\\alias{foo}", "\\title{Foo}", "\\description{Foo.}"),
#'   file.path(pkg, "man", "foo.Rd")
#' )
#'
#' topic_rd_path("foo", pkg)
topic_rd_path <- function(topic, package) {
  check_string(topic)
  check_string(package)

  if (!is_pkg_path(package) && !pkg_is_installed(package)) {
    return(NULL)
  }
  entry <- index(package)
  if (entry$backend != "source") {
    return(NULL)
  }
  file <- get0(topic, envir = entry$env, inherits = FALSE)
  if (is.null(file)) {
    return(NULL)
  }
  entry$files[[file]]
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
    entry$macros <- pkg_macros(entry$path)
  }
  tools::parse_Rd(
    entry$files[[file]],
    macros = entry$macros,
    encoding = "UTF-8"
  )
}
