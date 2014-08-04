source("helper-pair.R")

context("pair1")

test_that("construction", {
  xi <- pair1(1L, 2L, "int")
  xd <- pair1(1.0, 2.0, "double")
  xs <- pair1("foo", "bar", "string")

  expect_that(xi, is_a("pair1___int"))
  expect_that(xd, is_a("pair1___double"))
  expect_that(xs, is_a("pair1___string"))

  expect_that(xi, is_a("pair1"))
  expect_that(xd, is_a("pair1"))
  expect_that(xs, is_a("pair1"))

  expect_that(xi, is_a("R6"))
  expect_that(xd, is_a("R6"))
  expect_that(xs, is_a("R6"))
})
