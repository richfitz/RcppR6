context("Simple Classes")

test_that("empty", {
  e <- testExamples:::simple_empty()
  expect_that(e, is_a("simple_empty"))
  expect_that(ls(e), is_identical_to("initialize"))
  expect_that(ls(e, all.names=TRUE),
              is_identical_to(c(".ptr", "initialize")))
  expect_that(e$.ptr, is_a("externalptr"))
})
