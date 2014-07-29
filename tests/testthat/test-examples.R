source("helper-rcppr6.R")

context("examples")

test_that("examples", {
  ## Because of the devtools issue (hadley/devtools#531) we need to use
  ## a non-standard temporary file location for the tests.
  path <- "~/tmp"
  if (!file.exists(path)) {
    dir.create(path)
  }
  pkg <- file.path(path, "examples")
  if (file.exists(pkg)) {
    unlink(pkg, recursive=TRUE)
  }

  file.copy(rcppr6_file("examples"), path, recursive=TRUE)

  rcppr6::install(pkg)
  devtools::document(pkg)
  devtools::load_all(pkg)
  ## fresh=TRUE here would be nice, but can't happen.
  devtools::test(pkg)
  ## Should always clean up here, really.  Will go away when the
  ## tempdir issue does.
  unlink(pkg, recursive=TRUE)
})
