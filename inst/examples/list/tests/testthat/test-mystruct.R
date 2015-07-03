context("mystruct")

test_that("Creation", {
  x <- mystruct()
  ## This is hard coded with the default:
  cmp <- structure(list(a_bool=TRUE, an_int=3L, a_real_number=3.141,
                        a_string="hello world"),
                   class="mystruct")
  expect_that(x, is_identical_to(cmp))

  y <- mystruct(a_bool=FALSE, an_int=4L)
  cmp$a_bool <- FALSE
  cmp$an_int <- 4L
  expect_that(y, is_identical_to(cmp))

  z <- mystruct(values=list(an_int=100L))
  expect_that(z$an_int, is_identical_to(100L))

  expect_that(mystruct(foo=1), throws_error("Unknown fields: foo"))
})

test_that("Loading", {
  x <- mystruct()
  x$an_int <- 7L
  y <- test_flip(x)
  expect_that(x$a_bool, is_true())  # unchanged
  expect_that(y$a_bool, is_false()) # changed

  ## Set the bool:
  x$a_bool <- FALSE
  expect_that(x, is_identical_to(y))
})

test_that("Class checking", {
  x <- mystruct()
  class(x) <- NULL
  expect_that(test_flip(x),
              throws_error("Expected an object of type mystruct"))
  expect_that(test_flip(NULL),
              throws_error("Expected an object of type mystruct"))
})
