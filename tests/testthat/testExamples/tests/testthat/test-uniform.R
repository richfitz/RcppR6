source("helper-examples.R")

context("uniform")

test_that("Creation", {
  expect_that(uniform(), throws_error("missing, with no default"))
  expect_that(uniform(1), throws_error("missing, with no default"))

  u <- uniform(0, pi)
  expect_that(u, is_a("uniform"))
  expect_that(u, is_a("R6"))

  ## Some internal details:
  expect_that(u$private$ptr, is_a("externalptr"))
  expect_that(attr(u$private$ptr, "type"),
              is_identical_to("testExamples__uniform"))
})

test_that("Methods", {
  u <- uniform(0, pi)
  set.seed(1)
  r1 <- u$draw(10)
  set.seed(1)
  r2 <- runif(10, 0, pi)
  expect_that(r1, is_identical_to(r2))
  r3 <- u$draw(10)
  expect_that(r3, not(equals(r1)))
  expect_that(u$range(), equals(pi))
})

test_that("Active", {
  u <- uniform(0, pi)
  expect_that(u$min, equals(0))
  expect_that(u$max, equals(pi))
  ## Member function
  expect_that(u$the_min, equals(0))
  ## Free function
  expect_that(u$the_max, equals(pi))

  ## Free function that modifies state
  set.seed(1)
  r <- runif(3, 0, pi)
  set.seed(1)
  for (ri in r) {
    expect_that(u$u, is_identical_to(ri))
  }

  u$min <- -1
  expect_that(u$min, equals(-1))
  expect_that(u$the_min, equals(-1))

  u$the_min <- -2
  expect_that(u$min, equals(-2))
  expect_that(u$the_min, equals(-2))

  u$max <- 1
  expect_that(u$max, equals(1))
  expect_that(u$the_max, equals(1))

  u$the_max <- 2
  expect_that(u$max, equals(2))
  expect_that(u$the_max, equals(2))
})
