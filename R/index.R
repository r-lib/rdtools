# Per-package topic indexes, cached in `the$index` keyed by the caller's
# `package` argument (a name, or a normalized path for source packages).
# Each entry is an environment so the lazily-populated Rd cache (entry$rd)
# persists without reassignment. Entries are validated on every access
# against cheap filesystem stamps and rebuilt when stale, so callers never
# need to invalidate by hand (index_reset() exists for stubborn cases).

index <- function(package) {
  check_string(package)

  if (is_pkg_path(package)) {
    key <- normalizePath(package, mustWork = TRUE)
  } else {
    key <- package
  }

  entry <- get0(key, envir = the$index, inherits = FALSE)
  if (is.null(entry) || !index_valid(entry)) {
    entry <- index_build(package)
    assign(key, entry, envir = the$index)
  }
  entry
}

index_reset <- function() {
  the$index <- new.env(parent = emptyenv())
  invisible(NULL)
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
    path <- find.package(package)
    if (file.exists(file.path(path, "help", "aliases.rds"))) {
      backend <- "installed"
    } else {
      backend <- "source"
    }
    list(name = package, path = path, backend = backend)
  }
}

# Cheap change-detection stamp: one stat() for installed packages; a
# directory listing plus vectorized stat() for source packages (~0.5 ms
# for a large man/, vs ~7 ms to rebuild). rd_files() sorts bytewise so
# the stamp is insensitive to readdir order.
index_stamp <- function(res) {
  if (res$backend == "installed") {
    path <- file.path(res$path, "help", "aliases.rds")
    list(mtime = file.mtime(path), size = file.size(path))
  } else {
    files <- rd_files(file.path(res$path, "man"))
    list(files = files, mtime = file.mtime(files))
  }
}

index_valid <- function(entry) {
  res <- tryCatch(index_resolve(entry$input), error = function(e) NULL)
  identical(res, entry$resolution) &&
    identical(index_stamp(res), entry$stamp)
}

index_build <- function(package) {
  res <- index_resolve(package)
  stamp <- index_stamp(res)

  entry <- new.env(parent = emptyenv())
  entry$input <- package
  entry$name <- res$name
  entry$path <- res$path
  entry$backend <- res$backend
  entry$resolution <- res
  entry$stamp <- stamp
  entry$rd <- new.env(parent = emptyenv())

  if (res$backend == "installed") {
    topics <- readRDS(file.path(res$path, "help", "aliases.rds"))
  } else {
    files <- stamp$files
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
# by basename for deterministic order (see src/rd-index.c, note 3).
rd_files <- function(path) {
  files <- .Call(c_rd_files, path)
  if (length(files) == 0) {
    return(stats::setNames(character(), character()))
  }
  files[order(names(files), method = "radix")]
}
