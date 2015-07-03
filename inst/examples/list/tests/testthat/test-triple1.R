context("triple1")

test_that("construction", {
  xi <- triple1("int")(first=1L, second=2L, third=3L)
  xd <- triple1("double")(first=1.1, second=2.2, third=3.3)
  xs <- triple1("string")(first="one", second="two", third="three")

  expect_that(xi, is_a("triple1<int>"))
  expect_that(xd, is_a("triple1<double>"))
  expect_that(xs, is_a("triple1<string>"))

  expect_that(xi, is_a("triple1"))
  expect_that(xd, is_a("triple1"))
  expect_that(xs, is_a("triple1"))

  expect_that(is.list(xi), is_true())
  expect_that(is.list(xd), is_true())
  expect_that(is.list(xs), is_true())

  expect_that(xi$first,  is_identical_to(1L))
  expect_that(xi$second, is_identical_to(2L))
  expect_that(xi$third,  is_identical_to(3L))

  expect_that(xd$first,  is_identical_to(1.1))
  expect_that(xd$second, is_identical_to(2.2))
  expect_that(xd$third,  is_identical_to(3.3))

  expect_that(xs$first,  is_identical_to("one"))
  expect_that(xs$second, is_identical_to("two"))
  expect_that(xs$third,  is_identical_to("three"))
})
