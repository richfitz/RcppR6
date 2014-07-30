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
  // This is additional to the Rcpp stuff, and is to exercise the
  // active bindings.
  double get_min() const {
    return min;
  }
  void set_min(double value) {
    min = value;
  }
  double min, max;
};

// Because this is defined within the header, we need to declare it
// inline (or it will be emmited in every compilation unit).
// Alternatively just declare it:
//   double uniform_range(const Uniform&);
// and then define the function within a cpp file within src/
//
// Note that we're not using pointers (as Rcpp modules does), but
// using references/const references.
inline double uniform_range(const Uniform& w) {
  return w.max - w.min;
}

// This is new, compared with the modules example.  Draws a single
// random number.  This is used to exercise the active bindings.
inline Rcpp::NumericVector draw1(Uniform& x) {
  return x.draw(1);
}

// These are also new, and act as active bindings for the 'max'
// element.  You'd not usually use this, because that's what field is
// for.
inline double uniform_get_max(const Uniform& x) {
  return x.max;
}
inline void uniform_set_max(Uniform& x, double value) {
  x.max = value;
}

}

#endif
