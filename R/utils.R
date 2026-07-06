`%||%` <- function(x, y) if (is.null(x)) y else x

check_string <- function(x, arg = deparse(substitute(x))) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop(sprintf("`%s` must be a single string.", arg), call. = FALSE)
  }
}

check_character <- function(x, arg = deparse(substitute(x))) {
  if (!is.character(x) || anyNA(x)) {
    stop(sprintf("`%s` must be a character vector.", arg), call. = FALSE)
  }
}
