source("helper-examples.R")

context("Uniform")

test_that("Creation", {
  expect_that(Uniform(), throws_error("missing, with no default"))
  expect_that(Uniform(1), throws_error("missing, with no default"))

  u <- Uniform(0, pi)
  expect_that(u, is_a("Uniform"))
  expect_that(u, is_a("R6"))

  ## Some internal details:
  expect_that(u$private$ptr, is_a("externalptr"))
  expect_that(attr(u$private$ptr, "type"),
              is_identical_to("examples__Uniform"))
})

test_that("Methods", {
  u <- Uniform(0, pi)
  set.seed(1)
  r1 <- u$draw(10)
  set.seed(1)
  r2 <- runif(10, 0, pi)
  expect_that(r1, is_identical_to(r2))
  r3 <- u$draw(10)
  expect_that(r3, not(equals(r1)))
  expect_that(u$range(), equals(pi))
})
