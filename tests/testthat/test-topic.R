test_that("topic_split() splits qualified topics", {
  expect_equal(
    topic_split("stats::rnorm"),
    list(package = "stats", topic = "rnorm")
  )
  expect_equal(
    topic_split("stats:::rnorm"),
    list(package = "stats", topic = "rnorm")
  )
  expect_equal(topic_split("rnorm"), list(package = NULL, topic = "rnorm"))
})

test_that("topic_split() only splits on valid package names", {
  # S7 method aliases contain :: but aren't qualified topics
  expect_equal(
    topic_split("speak,foo::Dog-method"),
    list(package = NULL, topic = "speak,foo::Dog-method")
  )
  # package names must be at least two characters and start with a letter
  expect_equal(
    topic_split("x::foo"),
    list(package = NULL, topic = "x::foo")
  )
  expect_equal(
    topic_split(".pkg::foo"),
    list(package = NULL, topic = ".pkg::foo")
  )
})

test_that("topic_split() checks its input", {
  expect_error(topic_split(1), "single string")
  expect_error(topic_split(c("a", "b")), "single string")
})

test_that("topic_find() finds topics in installed packages", {
  expect_equal(
    topic_find("rnorm", "stats"),
    list(package = "stats", file = "Normal")
  )
})

test_that("topic_find() respects package order", {
  # "filter" is documented in both stats and dplyr-style packages;
  # use two base packages with a shared alias-free check instead:
  found <- topic_find("mean", c("stats", "base"))
  expect_equal(found$package, "base")
})

test_that("topic_find_all() returns every match", {
  expect_equal(
    topic_find_all("plot", c("graphics", "base")),
    data.frame(
      package = c("graphics", "base"),
      file = c("plot.default", "plot")
    )
  )
  expect_equal(
    topic_find_all("not-a-topic", "base"),
    data.frame(package = character(), file = character())
  )
})

test_that("topic_find_all() does not recheck cached packages", {
  pkg_topics("stats")
  local_mocked_bindings(
    pkg_find_path = function(package) stop("unexpected filesystem lookup")
  )

  expect_equal(
    topic_find_all("rnorm", "stats"),
    data.frame(package = "stats", file = "Normal")
  )
})

test_that("topic_find() returns NULL for missing topics", {
  expect_null(topic_find("definitely-not-a-topic", "stats"))
})

test_that("topic_exists() detects documented topics", {
  expect_identical(topic_exists("rnorm", "stats"), TRUE)
  expect_identical(topic_exists("not-a-topic", "stats"), FALSE)
  expect_identical(topic_exists("foo", "not-an-installed-package"), FALSE)
})

test_that("topic_exists() searches multiple packages", {
  expect_identical(topic_exists("rnorm", c("base", "stats")), TRUE)
  expect_identical(topic_exists("mean"), TRUE)
})

test_that("topic_origin() finds the package supplying an object", {
  expect_identical(topic_origin("rnorm", "stats"), "stats")
  expect_identical(topic_origin("local_tempdir", "withr"), "withr")
})

test_that("topic_find() searches the search path by default", {
  found <- topic_find("mean")
  expect_equal(found$package, "base")
})

test_that("topic_find() skips uninstalled packages", {
  expect_null(topic_find("foo", "not-an-installed-package"))
  expect_equal(
    topic_find("rnorm", c("not-an-installed-package", "stats")),
    list(package = "stats", file = "Normal")
  )
})

test_that("topic_find() finds topics in source packages", {
  path <- local_test_pkg("foo.Rd" = c("foo", "bar"))
  expect_equal(
    topic_find("bar", path),
    list(package = "testpkg", file = "foo")
  )
})

test_that("topic_qualifier() determines package qualification", {
  path <- local_test_pkg("foo.Rd" = "foo")

  expect_identical(topic_qualifier("foo", path, "withr"), NA_character_)
  expect_identical(topic_qualifier("rnorm", path, "withr"), NA_character_)
  expect_identical(topic_qualifier("local_tempdir", path, "withr"), "withr")
  expect_identical(
    topic_qualifier("definitely-not-a-topic", path, "withr"),
    character()
  )
})

test_that("topic_qualifier() defaults to searching only base packages", {
  path <- local_test_pkg("foo.Rd" = "foo")
  expect_identical(topic_qualifier("rnorm", path), NA_character_)
})

test_that("topic_rd() fetches parsed Rd from installed packages", {
  rd <- topic_rd("rnorm", "stats")
  expect_s3_class(rd, "Rd")

  tags <- vapply(rd, function(x) attr(x, "Rd_tag") %||% "", character(1))
  expect_true("\\name" %in% tags)
})

test_that("topic_rd() parses Rd from source packages", {
  path <- local_test_pkg("foo.Rd" = "foo")
  rd <- topic_rd("foo", path)
  expect_s3_class(rd, "Rd")

  tags <- vapply(rd, function(x) attr(x, "Rd_tag") %||% "", character(1))
  expect_true("\\title" %in% tags)
})

test_that("topic_rd() caches parsed Rd", {
  path <- local_test_pkg("foo.Rd" = "foo")
  rd1 <- topic_rd("foo", path)
  rd2 <- topic_rd("foo", path)
  expect_identical(rd1, rd2)
})

test_that("topic_rd() errors clearly on missing topics", {
  expect_error(
    topic_rd("not-a-topic", "stats"),
    "Can't find topic 'not-a-topic' in package 'stats'"
  )
})

test_that("topic_rd_path() returns the path for source packages", {
  path <- local_test_pkg("foo.Rd" = c("foo", "bar"))
  expect_equal(
    topic_rd_path("bar", path),
    file.path(normalizePath(path), "man", "foo.Rd")
  )
})

test_that("topic_rd_path() errors on missing topics and installed packages", {
  expect_error(
    topic_rd_path("not-a-topic", "stats"),
    "Can't find topic 'not-a-topic' in package 'stats'"
  )
  expect_error(
    topic_rd_path("rnorm", "stats"),
    "don't keep Rd files on disk"
  )
})
