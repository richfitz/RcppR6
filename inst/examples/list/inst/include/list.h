// -*-c++-*-
#ifndef _LIST_H_
#define _LIST_H_

#include <list/mystruct.hpp>
#include <list/validated.hpp>
#include <list/triple1.hpp>

// Include this early on.  It can be either after classes have been
// defined (but before Rcpp has been loaded) or first.  This file will
// attempt to provide declarations for the classes and namespaces that
// you use, but this might be fragile.
#include <list/RcppR6_pre.hpp>

// Anything after this point is OK to include Rcpp.h.  This is
// probably where the meat of the included material goes if your
// classes directly use Rcpp types.  Otherwise you can just declare
// them earlier up.

// This line can safely be the last line in the file, but may go any
// point after RcppR6_pre.hpp is included.
#include <list/RcppR6_post.hpp>

#endif
