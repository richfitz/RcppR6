context("Simple Classes")

test_that("empty", {
  e <- empty()
  expect_that(e, is_a("empty"))
  expect_that(sort(ls(e)),
              is_identical_to(sort(c("clone", "initialize"))))
  expect_that(e$.ptr, is_a("externalptr"))
})
