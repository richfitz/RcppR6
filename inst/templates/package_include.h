// -*-c++-*-
#ifndef _{{PACKAGE}}_H_
#define _{{PACKAGE}}_H_

// If your class definitions don't depend on Rcpp types, include them
// here, e.g.:
//   #include <{{package}}/myclass.hpp>
// If they do, then forward declare them, e.g.:
//   class myclass;
//
// Once that is done, this line is OK:
#include <rcppr6_pre.hpp>

// Anything after this point is OK to include Rcpp.h, so if your class
// definitions *do* depend on Rcpp, this is the point to include them.

// This line can safely be the last line in the file, but may go any
// point after rcppr6_pre.hpp is included.
#include <rcppr6_post.hpp>

#endif
