context("Simple Classes")

test_that("empty", {
  e <- testExamples:::simple_empty()
  expect_that(e, is_a("simple_empty"))
  expect_that(ls(e), is_identical_to(c("initialize", "private", "self")))
  expect_that(ls(e$private), is_identical_to("ptr"))
})
