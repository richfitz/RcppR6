#ifndef _EXAMPLES_DEFINITIONS_HPP_
#define _EXAMPLES_DEFINITIONS_HPP_

#include <Rcpp.h>

namespace examples {

class Uniform {
public:
  Uniform(double min_, double max_) : min(min_), max(max_) {}
  Rcpp::NumericVector draw(int n) const {
    Rcpp::RNGScope scope;
    return Rcpp::runif(n, min, max);
  }
  double min, max;
};

// Because this is defined within the header, we need to declare it
// inline (or it will be emmited in every compilation unit).
// Alternatively just declare it:
//   double uniform_range(const Uniform&);
// and then define the function within a cpp file within src/
inline double uniform_range(const Uniform& w) {
  return w.max - w.min;
}

}

#endif
