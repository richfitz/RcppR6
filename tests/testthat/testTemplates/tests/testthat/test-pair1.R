context("pair1")

test_that("construction", {
  xi <- pair1("int")(1L, 2L)
  xd <- pair1("double")(1.0, 2.0)
  xs <- pair1("string")("one", "two")

  expect_that(xi, is_a("pair1<int>"))
  expect_that(xd, is_a("pair1<double>"))
  expect_that(xs, is_a("pair1<string>"))

  expect_that(xi, is_a("pair1"))
  expect_that(xd, is_a("pair1"))
  expect_that(xs, is_a("pair1"))

  expect_that(xi, is_a("R6"))
  expect_that(xd, is_a("R6"))
  expect_that(xs, is_a("R6"))

  expect_that(xi$first,  is_identical_to(1L))
  expect_that(xi$second, is_identical_to(2L))
  expect_that(xd$first,  is_identical_to(1.0))
  expect_that(xd$second, is_identical_to(2.0))
  expect_that(xs$first,  is_identical_to("one"))
  expect_that(xs$second, is_identical_to("two"))
})
