test_that("pkg_topics() reads installed packages from aliases.rds", {
  topics <- pkg_topics("stats")
  expect_type(topics, "character")
  expect_equal(topics[["rnorm"]], "Normal")

  expect_equal(
    sort(topics),
    sort(readRDS(file.path(find.package("stats"), "help", "aliases.rds")))
  )
})

test_that("pkg_macros() loads package and system macros", {
  macros <- pkg_macros("stats")
  expect_type(macros, "environment")
  expect_setequal(intersect(ls(macros), "\\doi"), "\\doi")
})

test_that("local package lookup finds installed packages efficiently", {
  expect_equal(pkg_find("stats"), find.package("stats"))
  expect_equal(pkg_is_installed("stats"), TRUE)
  expect_equal(pkg_is_installed("not-an-installed-package"), FALSE)
  expect_error(
    pkg_find("not-an-installed-package"),
    class = "packageNotFoundError"
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
  writeLines(
    c("Package: empty", "Version: 0.0.1"),
    file.path(path, "DESCRIPTION")
  )

  expect_equal(pkg_topics(path), setNames(character(), character()))
})

test_that("source indexes remain cached until explicitly reset", {
  path <- local_test_pkg("foo.Rd" = "foo")
  other <- local_test_pkg("qux.Rd" = "qux")
  expect_named(pkg_topics(path), "foo")
  expect_named(pkg_topics(other), "qux")

  write_rd(path, "baz.Rd", "baz")
  write_rd(other, "zap.Rd", "zap")
  expect_named(pkg_topics(path), "foo")
  expect_named(pkg_topics(other), "qux")

  pkg_cache_reset(path)
  expect_named(pkg_topics(path), c("baz", "foo"))
  expect_named(pkg_topics(other), "qux")
})

test_that("pkg_cache_reset() drops the requested cached index", {
  pkg_topics("stats")
  pkg_cache_reset("stats")
  expect_equal(intersect(ls(the$index), "stats"), character())

  # still works after a full reset
  expect_equal(pkg_topics("stats")[["rnorm"]], "Normal")
})

test_that("pkg_cache_reset() drops every cached index by default", {
  pkg_topics("stats")
  pkg_topics("utils")
  pkg_cache_reset()
  expect_length(ls(the$index, all.names = TRUE), 0)

  expect_equal(pkg_topics("stats")[["rnorm"]], "Normal")
})

test_that("installed package unload hooks drop the cached index", {
  package <- "rdtoolsHookTest"
  key <- "hook-test"
  event <- packageEvent(package, "onUnload")
  old_hooks <- getHook(event)
  withr::defer(setHook(event, old_hooks, action = "replace"))

  assign(key, new.env(), envir = the$index)
  index_register_unload(package, key)
  index_register_unload(package, key)

  hooks <- getHook(event)
  expect_length(hooks, length(old_hooks) + 1)
  hooks[[length(hooks)]]()
  expect_equal(intersect(ls(the$index), key), character())
})

test_that("pkg_search_attached() puts attached packages before base fallbacks", {
  packages <- pkg_search_attached()
  expect_setequal(intersect(packages, pkg_search_base()), pkg_search_base())
  expect_equal(anyDuplicated(packages), 0L)
})

test_that("pkg_search_deps() extracts dependencies from the DESCRIPTION", {
  path <- local_test_pkg()
  writeLines(
    c(
      "Package: testpkg",
      "Version: 0.0.1",
      "Depends: R (>= 4.0), datasets",
      "Imports: withr (>= 2.0.0),",
      "    testthat",
      "Suggests: rlang"
    ),
    file.path(path, "DESCRIPTION")
  )

  expect_equal(
    pkg_search_deps(path),
    unique(c("datasets", "withr", "testthat", "rlang", pkg_search_base()))
  )
})

test_that("pkg_search_deps() falls back to base packages", {
  path <- local_test_pkg()
  expect_equal(pkg_search_deps(path), pkg_search_base())
})

test_that("pkg_search_deps() is cached until reset", {
  path <- local_test_pkg()
  expect_equal(pkg_search_deps(path), pkg_search_base())

  writeLines(
    c("Package: testpkg", "Version: 0.0.1", "Imports: withr"),
    file.path(path, "DESCRIPTION")
  )
  expect_equal(pkg_search_deps(path), pkg_search_base())

  pkg_cache_reset(path)
  expect_equal(pkg_search_deps(path), c("withr", pkg_search_base()))
})
