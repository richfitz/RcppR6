source("helper-rcppr6.R")

context("pair")

test_that("pair", {
  pkg <- rcppr6:::prepare_temporary(rcppr6:::rcppr6_file("pair"))
  rcppr6::install(pkg)
  devtools::document(pkg)
  devtools::load_all(pkg)
  ## fresh=TRUE here would be nice, but can't happen.
  devtools::test(pkg)
  ## Should always clean up here, really.  Will go away when the
  ## tempdir issue does.
  unlink(pkg, recursive=TRUE)
})
