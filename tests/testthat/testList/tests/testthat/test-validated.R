context("validated")

test_that("Creation", {
  x <- validated()
  expect_that(x, is_a("validated"))
  expect_that(x$n_elements, is_identical_to(0L))
  expect_that(x$list, is_identical_to(numeric(0)))

  ## The type coersion here shows that we have gone through to C++:
  x <- validated(n_elements=1.0, list=pi)
  expect_that(x$n_elements, is_identical_to(1L))
  expect_that(x$list, is_identical_to(pi))

  expect_that(x <- validated(n_elements=100.0, list=pi),
              throws_error("list is incorrect length"))
  x <- validated(n_elements=100.0, list=rep(pi, 100))
  expect_that(length(x$list), equals(100))

  expect_that(x <- validated(n_elements=-1, list=numeric(0)),
              throws_error("Negative lengths are not allowed"))
})


