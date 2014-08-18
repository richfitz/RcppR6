context("Circle")

test_that("Code from README", {
  obj <- circle(1)
  expect_that(obj, is_a("circle"))

  expect_that(obj$radius, equals(1))
  obj$radius <- 2
  expect_that(obj$radius, equals(2))
  expect_that(obj$area(), equals(pi * 4))
  obj$circumference <- 1
  expect_that(obj$circumference, equals(1))
  expect_that(obj$radius, equals(1 / (2 * pi)))
})
