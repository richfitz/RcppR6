#include <list/validated.hpp>
#include <Rcpp.h>

namespace examples {
void validated::validate() const {
  if (n_elements < 0) {
    Rcpp::stop("Negative lengths are not allowed");
  }
  if (list.size() != static_cast<size_t>(n_elements)) {
    Rcpp::stop("list is incorrect length");
  }
}
}
