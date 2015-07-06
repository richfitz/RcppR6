context("positive")

test_that("construction", {
  xi <- positive("int")(value=1L)
  xd <- positive("double")(value=pi)

  expect_that(xi, is_a("positive<int>"))
  expect_that(xd, is_a("positive<double>"))

  expect_that(xi, is_a("positive"))
  expect_that(xd, is_a("positive"))

  expect_that(is.list(xi), is_true())
  expect_that(is.list(xd), is_true())

  expect_that(xi$value,  is_identical_to(1L))
  expect_that(xd$value,  is_identical_to(pi))

  expect_that(positive("int")(value=-1),
              throws_error("value must be positive"))
  expect_that(positive("double")(value=-pi),
              throws_error("value must be positive"))
})
