context("examples")

test_that("examples", {
  ## Because of the devtools issue (hadley/devtools#531) we need to use
  ## a non-standard temporary file location for the tests.
  path <- system.file("examples/examples", package="RcppR6")
  pkg <- RcppR6:::prepare_temporary(path)

  RcppR6::install(pkg)
  devtools::document(pkg)
  expect_that(RcppR6::check(pkg), not(throws_error()))
  ## fresh=TRUE here would be nice, but can't happen.
  devtools::test(pkg)

  expect_that(RcppR6::install(pkg), shows_message("RcppR6 up to date"))

  ## Should always clean up here, really.  Will go away when the
  ## tempdir issue does.
  unlink(pkg, recursive=TRUE)
})
