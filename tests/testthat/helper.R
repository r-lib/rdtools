# Create a minimal source package with the given Rd files; the directory
# is deleted when the calling frame exits (its cached index entry is
# harmless: keyed by a unique temp path that is never looked up again).
# `...` is a named list: name.Rd = character vector of aliases.
local_test_pkg <- function(..., name = "testpkg", env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)
  writeLines(
    c(paste0("Package: ", name), "Version: 0.0.1"),
    file.path(path, "DESCRIPTION")
  )
  dir.create(file.path(path, "man"))

  rd <- list(...)
  for (file in names(rd)) {
    write_rd(path, file, rd[[file]])
  }

  path
}

write_rd <- function(path, file, aliases, title = "A title") {
  name <- sub("\\.[Rr]d$", "", file)
  lines <- c(
    paste0("\\name{", name, "}"),
    paste0("\\alias{", aliases, "}"),
    paste0("\\title{", title, "}"),
    "\\description{A description.}"
  )
  writeLines(lines, file.path(path, "man", file))
}
