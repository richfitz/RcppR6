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

test_that("functions", {
  xi <- make_pair1("int")(1L, 2L)
  expect_that(xi, is_a("pair1<int>"))
  xd <- make_pair1("double")(1.1, 2.2)
  expect_that(xd, is_a("pair1<double>"))
  xs <- make_pair1("string")("one", "two")
  expect_that(xs, is_a("pair1<string>"))

  ## Explicit type interface:
  expect_that(pair1_sum("int")(xi), equals(3))
  expect_that(pair1_sum("double")(xd), equals(3.3))
  expect_that(pair1_sum("string")(xs), equals("onetwo"))

  ## Implicit type interface:
  xi2 <- combine(xi, xi)
  expect_that(xi2, is_a("pair1<int>"))
  expect_that(xi2$first, equals(2L))

  expect_that(combine(xi, xd), throws_error("Expected an object of type"))
  expect_that(combine(1, 2), throws_error("Unknown type: numeric"))

  xd2 <- combine(xd, xd)
  expect_that(xd2, is_a("pair1<double>"))
  expect_that(xd2$first, equals(2.2))

  xs2 <- combine(xs, xs)
  expect_that(xs2, is_a("pair1<string>"))
  expect_that(xs2$first, equals("oneone"))
})
