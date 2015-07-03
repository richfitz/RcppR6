// Code used in testing only.
#include <list.h>

// [[Rcpp::export]]
examples::mystruct test_flip(examples::mystruct x) {
  x.a_bool = !x.a_bool;
  return x;
}
