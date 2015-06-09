passes_tests <- function() {
  function(package) {
    res <- devtools::test(package)
    expectation(!any(as.data.frame(res)$error),
                "tests failed", "tests passed")
  }
}
