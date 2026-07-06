#' @keywords internal
#' @useDynLib rdtools, .registration = TRUE
"_PACKAGE"

the <- new.env(parent = emptyenv())
the$index <- new.env(parent = emptyenv())
the$unload_hooks <- new.env(parent = emptyenv())
