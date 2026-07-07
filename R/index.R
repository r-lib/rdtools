# Per-package topic indexes, cached in `the$index` keyed by the caller's
# `package` argument (a name, or a normalized path for source packages).
# Each entry is an environment so the lazily-populated caches (entry$rd,
# entry$origin, entry$qualifier, entry$deps) persist without reassignment. Entries remain valid until explicitly reset;
# installed entries are reset automatically when their namespace is unloaded.

index <- function(package) {
  check_string(package)
  key <- index_key(package)

  entry <- get0(key, envir = the$index, inherits = FALSE)
  if (is.null(entry)) {
    entry <- index_build(package, key)
    assign(key, entry, envir = the$index)
  }
  entry
}

#' Reset cached package indexes
#'
#' Clears the cached topic index, parsed Rd objects, and dependency search
#' set for `package`, or for every package when `package` is `NULL`. This will generally be called
#' automatically by roxygen2 for source packages. Installed package indexes
#' are automatically reset when their namespace is unloaded.
#'
#' @param package A package name, or a path to the source directory of a
#'   package. Use `NULL`, the default, to reset every cached index.
#' @returns `NULL`, invisibly.
#' @export
#' @examples
#' head(pkg_topics("stats"))
#' pkg_cache_reset("stats")
#'
#' # Reset every cached index
#' pkg_cache_reset()
pkg_cache_reset <- function(package = NULL) {
  if (is.null(package)) {
    rm(list = ls(the$index, all.names = TRUE), envir = the$index)
  } else {
    check_string(package)
    index_drop(index_key(package))
  }
  invisible(NULL)
}

index_key <- function(package) {
  if (is_pkg_path(package)) {
    normalizePath(package, mustWork = TRUE)
  } else {
    package
  }
}

index_drop <- function(key) {
  if (exists(key, envir = the$index, inherits = FALSE)) {
    rm(list = key, envir = the$index)
  }
}

index_register_unload <- function(package, key) {
  if (exists(package, envir = the$unload_hooks, inherits = FALSE)) {
    return()
  }

  setHook(
    packageEvent(package, "onUnload"),
    function(...) index_drop(key),
    action = "append"
  )
  assign(package, TRUE, envir = the$unload_hooks)
}

# A path (as opposed to a package name) is anything containing a path
# separator, or a directory that holds a DESCRIPTION.
is_pkg_path <- function(package) {
  grepl("[/\\\\]", package) || file.exists(file.path(package, "DESCRIPTION"))
}

is_dev_package <- function(package) {
  isNamespaceLoaded(package) &&
    !is.null(asNamespace(package)$.__DEVTOOLS__)
}

# Decide where a package's documentation lives and in what form:
# * "installed": help/aliases.rds + help/<pkg>.rdb next to it
# * "source":    man/*.Rd (source checkouts and pkgload-loaded packages)
index_resolve <- function(package) {
  if (is_pkg_path(package)) {
    path <- normalizePath(package, mustWork = TRUE)
    desc <- file.path(path, "DESCRIPTION")
    if (file.exists(desc)) {
      name <- unname(read.dcf(desc, "Package")[[1, 1]])
    } else {
      name <- basename(path)
    }
    list(name = name, path = path, backend = "source")
  } else if (is_dev_package(package)) {
    path <- getNamespaceInfo(asNamespace(package), "path")
    list(name = package, path = path, backend = "source")
  } else {
    path <- pkg_find(package)
    if (file.exists(file.path(path, "help", "aliases.rds"))) {
      backend <- "installed"
    } else {
      backend <- "source"
    }
    list(name = package, path = path, backend = backend)
  }
}

# A minimal find.package() for a single package. Unlike find.package(), this
# avoids reading and validating package metadata from every matching library.
pkg_find <- function(package) {
  path <- pkg_find_path(package)
  if (is.null(path)) {
    stop(packageNotFoundError(package, .libPaths(), call = sys.call()))
  }
  path
}

# A minimal rlang::is_installed() for a single package without version checks.
pkg_is_installed <- function(package) {
  exists(package, envir = the$index, inherits = FALSE) ||
    !is.null(pkg_find_path(package))
}

pkg_find_path <- function(package) {
  if (package == "base") {
    return(system.file())
  }
  if (isNamespaceLoaded(package)) {
    return(getNamespaceInfo(asNamespace(package), "path"))
  }

  paths <- file.path(.libPaths(), package)
  found <- which(file.exists(file.path(paths, "DESCRIPTION")))[1]
  if (is.na(found)) NULL else paths[[found]]
}

index_build <- function(package, key) {
  res <- index_resolve(package)

  entry <- new.env(parent = emptyenv())
  entry$name <- res$name
  entry$path <- res$path
  entry$backend <- res$backend
  entry$rd <- new.env(parent = emptyenv())
  entry$origin <- new.env(parent = emptyenv())
  entry$qualifier <- new.env(parent = emptyenv())

  if (res$backend == "installed") {
    index_register_unload(res$name, key)
    topics <- readRDS(file.path(res$path, "help", "aliases.rds"))
  } else {
    files <- rd_files(file.path(res$path, "man"))
    rd_names <- sub("\\.[Rr]d$", "", names(files))
    entry$files <- stats::setNames(unname(files), rd_names)
    topics <- source_topics(files, rd_names)
  }

  entry$topics <- topics
  entry$env <- list2env(
    as.list(topics),
    parent = emptyenv(),
    hash = TRUE,
    size = max(29L, 2L * length(topics))
  )
  entry
}

source_topics <- function(files, rd_names) {
  if (length(files) == 0) {
    return(stats::setNames(character(), character()))
  }

  aliases <- lapply(files, function(path) .Call(c_rd_aliases, path))
  alias <- unlist(aliases, use.names = FALSE) %||% character()
  file <- rep(rd_names, lengths(aliases))

  dup <- unique(alias[duplicated(alias)])
  if (length(dup) > 0) {
    info <- vapply(
      dup,
      function(a) {
        sprintf(
          "'%s' (%s)",
          a,
          paste0(file[alias == a], ".Rd", collapse = ", ")
        )
      },
      character(1)
    )
    warning(
      "Aliases documented in multiple Rd files: ",
      paste(info, collapse = "; "),
      call. = FALSE
    )
  }

  topics <- stats::setNames(file, alias)
  topics[!duplicated(alias, fromLast = TRUE)]
}

# *.[Rr]d files in `path`, full paths, basenames as names, sorted bytewise
# by basename for deterministic order.
rd_files <- function(path) {
  names <- dir(path, pattern = "\\.[Rr]d$")
  names <- names[order(names, method = "radix")]
  stats::setNames(file.path(path, names), names)
}
