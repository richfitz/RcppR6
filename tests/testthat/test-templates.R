context("pair")

test_that("pair", {
  path <- system.file("examples/templates", package="RcppR6")
  pkg <- RcppR6:::prepare_temporary(path)
  RcppR6::install(pkg)
  devtools::document(pkg)
  expect_that(RcppR6::check(pkg), not(throws_error()))
  devtools::load_all(pkg)
  ## fresh=TRUE here would be nice, but can't happen.
  expect_that(pkg, passes_tests())
  ## Should always clean up here, really.  Will go away when the
  ## tempdir issue does.
  unlink(pkg, recursive=TRUE)
})
