// -*-c++-*-
#ifndef _PAIR_H_
#define _PAIR_H_

#include <pair/pair1.hpp>

// Include this early on.  It can be either after classes have been
// define (but before Rcpp has been loaded) or first.  This file will
// attempt to provide declarations for the classes and namespaces that
// you use, but this might be fragile.
#include <pair/rcppr6_pre.hpp>

// Anything after this point is OK to include Rcpp.h.  This is
// probably where the meat of the included material goes if your
// classes directly use Rcpp types.  Otherwise you can just declare
// them earlier up.

// This line can safely be the last line in the file, but may go any
// point after rcppr6_pre.hpp is included.
#include <pair/rcppr6_post.hpp>

#endif
