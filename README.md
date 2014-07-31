# rcppr6

[![Build Status](https://travis-ci.org/richfitz/rcppr6.png?branch=master)](https://travis-ci.org/richfitz/rcppr6)

# Requirements

Class definitions are written in [YAML](http://en.wikipedia.org/wiki/YAML), and parsed using the [yaml package](cran.r-project.org/web/packages/yaml), from CRAN.

The [Rcpp](http://rcpp.org) R package is of course needed.  Interfaces this way build a set of code that is then run through Rcpp's "[attributes](http://dirk.eddelbuettel.com/code/rcpp/Rcpp-attributes.pdf)" facilities to build the actual R/C++ glue.

The [R6](https://github.com/wch/R6) R package is the reference class that we use for wrapping the generated class.  It's available on CRAN.  It's in a state of flux though, so things may break.

Roxygen comments are propagaged from the class definition into the created R files: to do anything with these you need the [devtools](https://github.com/hadley/devtools) package and its dependencies.

# Preparation

There are many requirements here, but almost all are really the same as for using Rcpp attributes.  If you can use Rcpp attributes in your project, you're probably OK.

1. `DESCRIPTION`: The package must have "Rcpp" listed under `LinkingTo` and under `Imports`.  `R6` must be listed under `Imports`.  The Rcpp requirements here are standard for packages using Rcpp attributes.  These will be set up automatically using `install()` or the eventual skeleton function.

2. `NAMESPACE`: Two requirements here:
  * Must import *something* from Rcpp.  The [Rcpp mailing list](http://permalink.gmane.org/gmane.comp.lang.r.rcpp/6744) suggests importing `evalCpp` because it's short to type.  If you use roxygen, the support for this will be done automatically.  I think this requirement is actually to satisfy `R CMD check`, and things will work so long as Rcpp is listed under `Imports:`.  But that might not be correct.
  * Must import *something* from R6.  I suggest `R6::R6Class`.
  * Must load the package's dynamic library (of course)
If you use roxygen these will be automatically set up for you.

3. A file `inst/include/<package_name>.h` must exist ("main package header file").  This is also an Rcpp attributes requirement.  This file must include the definitions of classes that you want to wrap.  It also needs to include two files:
  - `inst/include/<package_name>/rcppr6_pre.hpp` must be included *after* classes have been declared, but *before* `Rcpp.h` has been included.  This is often a pain, especially if you want to use Rcpp types within the class.  It may be sufficient to forward declare the classes that you export, but this will work badly with templated classes potentially (e.g., you can write `class foo;` but not `class foo<bar>`).  This reason for this load order is outlined in the "[Extending Rcpp](http://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-extending.pdf)" manual -- this file contains the prototypes for "non-intrusive extension".
  - `inst/include/<package_name>/rcppr6_post.hpp`, which may be included last in the main package header file (but must be included).  `Rcpp.h` can be safely loaded before this file, and this file will itself include `Rcpp.h` if it has not been loaded.

4. `src/Makevars` must be set up to add `-I../inst/include/` to the search path (also an Rcpp attributes requirement).  This will be automatically added, but the file can simply contain a line saying `PKG_CPPFLAGS += -I../inst/include/`

# Installation/updating

We look after a bunch of files.  This is not really ideal, but does avoid the load time work that Rcpp modules cost.

* `inst/include/<package_name>/rcppr6_pre.hpp`
* `inst/include/<package_name>/rcppr6_post.hpp`
* `inst/include/<package_name>/rcppr6_support.hpp`
* `src/rcppr6.cpp`
* `R/rcppr6.R`

These files are entirely rcppr6's - don't add anything to them.  Upgrades might totally alter these files at any point.  There is a little warning at the top that indicates this!  The contents of these files will morph and change, and running `install()` / `rcppr6()` will *always* update these files.
