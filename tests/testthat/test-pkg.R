test_that("pkg_topics() reads installed packages from aliases.rds", {
  topics <- pkg_topics("stats")
  expect_type(topics, "character")
  expect_equal(topics[["rnorm"]], "Normal")

  expect_identical(
    sort(topics),
    sort(readRDS(file.path(find.package("stats"), "help", "aliases.rds")))
  )
})

test_that("pkg_topics() indexes source packages from man/", {
  path <- local_test_pkg(
    "foo.Rd" = c("foo", "foo-extra"),
    "bar.Rd" = "bar"
  )
  expect_equal(
    pkg_topics(path),
    c(bar = "bar", foo = "foo", "foo-extra" = "foo")
  )
})

test_that("pkg_topics() unescapes aliases and keeps dotted names", {
  path <- local_test_pkg()
  write_rd(path, "ops.Rd", c("a\\%b\\%c", ".data"))
  expect_equal(
    pkg_topics(path),
    c("a%b%c" = "ops", ".data" = "ops")
  )
})

test_that("pkg_topics() finds multiple and mid-line aliases", {
  path <- local_test_pkg()
  writeLines(
    c(
      "\\name{multi}",
      "\\alias{multi}\\alias{multi2}",
      "\\title{A title}\\alias{multi3}",
      "% \\alias{commented-out}",
      "\\alias{multi4} % trailing comment",
      "\\alias{unterminated",
      "\\\\alias{escaped-backslash}",
      "\\description{A description.}"
    ),
    file.path(path, "man", "multi.Rd")
  )
  expect_equal(
    names(pkg_topics(path)),
    c("multi", "multi2", "multi3", "multi4")
  )
})

test_that("alias extraction matches parse_Rd() on tricky lines", {
  path <- local_test_pkg()
  writeLines(
    c(
      "\\name{tricky}",
      "\\alias{a\\%b}\\alias{x{y}z} \\alias{tricky}",
      "\\title{A title}",
      "\\description{A description.}"
    ),
    file.path(path, "man", "tricky.Rd")
  )

  mine <- names(pkg_topics(path))
  rd <- tools::parse_Rd(file.path(path, "man", "tricky.Rd"))
  tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
  reference <- vapply(
    rd[tags == "\\alias"],
    function(x) paste0(unlist(x), collapse = ""),
    character(1)
  )
  expect_equal(mine, reference)
})

test_that("pkg_topics() warns on duplicated aliases, last file wins", {
  path <- local_test_pkg(
    "a.Rd" = c("a", "dup"),
    "b.Rd" = c("b", "dup")
  )
  expect_warning(topics <- pkg_topics(path), "'dup' \\(a\\.Rd, b\\.Rd\\)")
  expect_equal(topics[["dup"]], "b")
})

test_that("pkg_topics() handles packages with no man directory", {
  path <- withr::local_tempdir()
  writeLines(c("Package: empty", "Version: 0.0.1"), file.path(path, "DESCRIPTION"))

  expect_equal(pkg_topics(path), setNames(character(), character()))
})

test_that("index is invalidated when man/ changes", {
  path <- local_test_pkg("foo.Rd" = "foo")
  expect_named(pkg_topics(path), "foo")

  # adding a file changes the directory listing
  write_rd(path, "baz.Rd", "baz")
  expect_true("baz" %in% names(pkg_topics(path)))

  # editing a file changes its mtime (forced, in case of coarse clocks)
  write_rd(path, "baz.Rd", c("baz", "qux"))
  Sys.setFileTime(file.path(path, "man", "baz.Rd"), Sys.time() + 10)
  expect_true("qux" %in% names(pkg_topics(path)))
})

test_that("index_reset() drops all cached indexes", {
  pkg_topics("stats")
  index_reset()
  expect_length(ls(the$index), 0)

  # still works after a full reset
  expect_equal(pkg_topics("stats")[["rnorm"]], "Normal")
})

test_that("pkg_search_path() puts attached packages before base fallbacks", {
  packages <- pkg_search_path()
  expect_true("base" %in% packages)
  expect_true("stats" %in% packages)
  expect_equal(anyDuplicated(packages), 0L)
  expect_equal(packages[[length(packages)]], "base")
})
