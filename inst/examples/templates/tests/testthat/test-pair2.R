context("pair2")

test_that("construction", {
  xid <- pair2("int", "double")(1L, 2.0)
  xsd <- pair2("string", "double")("one", 2.0)

  expect_that(xid, is_a("pair2<int,double>"))
  expect_that(xsd, is_a("pair2<string,double>"))

  expect_that(xid, is_a("pair2"))
  expect_that(xsd, is_a("pair2"))

  expect_that(xid, is_a("R6"))
  expect_that(xsd, is_a("R6"))

  expect_that(xid$first,  is_identical_to(1L))
  expect_that(xid$second, is_identical_to(2.0))
  expect_that(xsd$first,  is_identical_to("one"))
  expect_that(xsd$second, is_identical_to(2.0))
})
