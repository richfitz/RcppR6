// -*-c++-*-
#ifndef _TESTREADME_H_
#define _TESTREADME_H_

#include <RcppCommon.h>

class circle {
public:
  double radius;
  circle(double r) : radius(r) {}
  double area() const {
    return M_PI * radius * radius;
  }
  double circumference() const {
    return M_PI * 2 * radius;
  }
  void set_circumference(double c) {
    if (c < 0) {
      Rcpp::stop("Circumference must be positive");
    }
    radius = c / (2 * M_PI);
  }
};

// Include this early on.  It can be either after classes have been
// define (but before Rcpp has been loaded) or first.  This file will
// attempt to provide declarations for the classes and namespaces that
// you use, but this might be fragile.
#include <testREADME/RcppR6_pre.hpp>

// Anything after this point is OK to include Rcpp.h.  This is
// probably where the meat of the included material goes if your
// classes directly use Rcpp types.  Otherwise you can just declare
// them earlier up.

// This line can safely be the last line in the file, but may go any
// point after RcppR6_pre.hpp is included.
#include <testREADME/RcppR6_post.hpp>

#endif
