// -*-c++-*-
#ifndef _TESTPKG_H_
#define _TESTPKG_H_

#include <testpkg/definitions.hpp>

// These are included in two totally different files because the first
// requires only that class declarations have been made.  Full
// definitions could go between these two files if the classes need to
// reference Rcpp types.
#include <testpkg/rcpp_pre.hpp>
#include <testpkg/rcpp_post.hpp>

#endif
