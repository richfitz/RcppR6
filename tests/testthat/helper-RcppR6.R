passes_tests <- function() {
  function(package) {
    res <- devtools::test(package)
    expectation(sum(as.data.frame(res)$failed) == 0,
                "tests failed", "tests passed")
  }
}
