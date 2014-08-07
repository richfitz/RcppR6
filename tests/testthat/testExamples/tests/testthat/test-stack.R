context("stack")

test_that("Creation", {
  s <- stack()
  expect_that(s, is_a("stack"))
  expect_that(s, is_a("R6"))

  ## Some internal details:
  expect_that(s$private$ptr, is_a("externalptr"))
})

test_that("Empty stack", {
  s <- stack()
  expect_that(s$size,  equals(0))
  expect_that(s$empty, is_true())
  ## The empty stack has a missing top, and can be popped with no
  ## effect.
  expect_that(s$top,  is_identical_to(NA_integer_))
  expect_that(s$pop,  not(throws_error()))
  expect_that(s$size, equals(0))
})

test_that("Add things to the stack", {
  s <- stack()
  s$push(1)
  expect_that(s$size,  equals(1))
  expect_that(s$empty, is_false())
  expect_that(s$top,   equals(1))

  s$push(pi)
  expect_that(s$top, equals(as.integer(pi))) # 3L
  expect_that(s$push("foo"), throws_error("not compatible"))
  s$push(NA)
  expect_that(s$top, is_identical_to(NA_integer_))
  expect_that(s$size, equals(3))
})

test_that("Pop things off the stack", {
  s <- stack()
  r <- sample(5)
  for (i in rev(r)) {
    s$push(i)
  }
  expect_that(s$size, equals(length(r)))
  for (i in seq_along(r)) {
    expect_that(s$top, equals(r[[i]]))
    s$pop()
  }
  expect_that(s$size, equals(0))
  expect_that(s$empty, is_true())
})

test_that("Comparison operators", {
  s <- stack()
  t <- stack()
  expect_that(s$equals(t),  is_true())
  expect_that(s$differs(t), is_false())

  s$push(1)
  expect_that(s$equals(t),  is_false())
  expect_that(s$differs(t), is_true())

  t$push(1)
  expect_that(s$equals(t),  is_true())
  expect_that(s$differs(t), is_false())
})
