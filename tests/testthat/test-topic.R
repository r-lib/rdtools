test_that("topic_parse() splits qualified topics", {
  expect_equal(
    topic_parse("stats::rnorm"),
    list(package = "stats", topic = "rnorm")
  )
  expect_equal(
    topic_parse("stats:::rnorm"),
    list(package = "stats", topic = "rnorm")
  )
  expect_equal(topic_parse("rnorm"), list(package = NULL, topic = "rnorm"))
})

test_that("topic_parse() only splits on valid package names", {
  # S7 method aliases contain :: but aren't qualified topics
  expect_equal(
    topic_parse("speak,foo::Dog-method"),
    list(package = NULL, topic = "speak,foo::Dog-method")
  )
  # package names must be at least two characters and start with a letter
  expect_equal(
    topic_parse("x::foo"),
    list(package = NULL, topic = "x::foo")
  )
  expect_equal(
    topic_parse(".pkg::foo"),
    list(package = NULL, topic = ".pkg::foo")
  )
})

test_that("topic_parse() checks its input", {
  expect_error(topic_parse(1), "single string")
  expect_error(topic_parse(c("a", "b")), "single string")
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

test_that("topic_find() returns NULL for missing topics", {
  expect_null(topic_find("definitely-not-a-topic", "stats"))
})

test_that("topic_find() searches the search path by default", {
  found <- topic_find("mean")
  expect_equal(found$package, "base")
})

test_that("topic_find() errors on uninstalled packages", {
  expect_error(topic_find("foo", "not-an-installed-package"))
})

test_that("topic_find() finds topics in source packages", {
  path <- local_test_pkg("foo.Rd" = c("foo", "bar"))
  expect_equal(
    topic_find("bar", path),
    list(package = "testpkg", file = "foo")
  )
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
