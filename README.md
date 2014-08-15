# rcppr6

[![Build Status](https://travis-ci.org/richfitz/rcppr6.png?branch=master)](https://travis-ci.org/richfitz/rcppr6)

**Warning: this package is under heavy development and everything may change.  I welcome any comments on design ideas though**

# What is this thing?

This package aims to provide a simple way of generating boilerplate code for exposing C++ classes to R.  It is similar in many ways to Rcpp "modules".

There will be vignettes explaining the idea more fully, but here is the basic idea.  Suppose we have a class like this

```c++
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
```

This simple class represents a circle, and has one data member (`radius`), and methods to compute the area and circumference.  The method `set_circumference` sets the radius so that it gives the required circumference.  (Yes, this is very silly.  This would also be trivial to write using `R6` or reference classes directly, but perhaps this is needed as some part of a larger set of C++ code?).

To expose the class, we write a small piece of yaml:

```yaml
circle:
  constructor:
    args: [radius: double]
  methods:
    area: [return_type: double]
  active:
    circumference:
      name_cpp: [circumference, get_circumference]
      access: member
      type: double
    radius: {access: field, type: double}
```

After running rcppr6 on this, we can interact with objects of this type from R

```r
obj <- circle(1)
obj$radius # 1
obj$radius <- 2
obj$radius # 2
obj$area() # 12.56637
obj$circumference <- 1
obj$circumference # 1
obj$radius # 0.1591549
```

A couple of notes here:

* The name of the class is the top-most yaml key - in this case 'circle'.  This will generate an R function `circle` that generates R6 objects.
* There are three types of entities exported: the constructor (in contrast with Rcpp modules there can be only one), methods, and active-bound fields (which simulate data members in the R6 object but using these involves calling functions behind the scenes).
* In contrast with Rcpp modules we must be explicit about types, and about where methods are found.  C++ is notoriusly difficult to parse, and I've avoided trying to infer these from signatures (in contrast with Rcpp attributes).  This leads to some undesirable doubling up of effort.  Eventually I plan on using libclang to infer types when they are ommited, but this will be optional.
* This is yaml, so the format is very flexible: the last active member could be equivalently written as:

```
    radius:
      access: field
      type: double
```

# How is this run?

rcppr6 assumes you are building a package.  There is currently no support for inline use.  A file `inst/include/rcppr6_classes.yml` needs to exist with class definitions (though see Configuration, below).  Running `rcppr6::rcppr6()` will generate a bunch of code, and re-run Rcpp attributes.  The package can then be built as usual.  Importantly, your package does not need to depend on rcppr6 at all -- once the code has been generated your package is independent of rcppr6.

# When is this the right sort of thing to use?

* You want reference semantics
* You have existing C++ code to wrap, especially templated classes
* You have time consuming code that you want to expose
* You don't want to write lots of boilerplate glue, and Rcpp modules won't work for you

# Why not use [Rcpp modules](http://dirk.eddelbuettel.com/code/rcpp/Rcpp-modules.pdf)?

* Modules can be slow to load (on a complicated project we have load times of ~5s for a package that uses modules)
* The compile times using modules can be slow, and the compiler error messages are inscruitiable
* Support for templated classes is patchy
* There is some sort of garbage collection [issue](http://r.789695.n4.nabble.com/Reference-class-finalize-fails-with-attempt-to-apply-non-function-td4174697.html), at least on OSX that prints warnings that seem to be harmless.
* It is not currently under active development, with the author apparently having left Rcpp to work on Rcpp11, and removing modules from that version!

# Requirements

Class definitions are written in [YAML](http://en.wikipedia.org/wiki/YAML), and parsed using the [yaml package](cran.r-project.org/web/packages/yaml), from CRAN.

The [Rcpp](http://rcpp.org) R package is of course needed.  Interfaces this way build a set of code that is then run through Rcpp's "[attributes](http://dirk.eddelbuettel.com/code/rcpp/Rcpp-attributes.pdf)" facilities to build the actual R/C++ glue.

The [R6](https://github.com/wch/R6) R package is the reference class that we use for wrapping the generated class.  It's available on CRAN.  It's in a state of flux though, so things may break.

Roxygen comments are propagaged from the class definition into the created R files: to do anything with these you need the [devtools](https://github.com/hadley/devtools) and [roxygen2](http://cran.r-project.org/web/packages/roxygen2) packages and their dependencies.

Nothing is really documented about these yet, but see the example packages in `tests/testthat`.

# Preparation

There are many requirements here, but almost all are really the same as work well for using Rcpp attributes.  If you can use Rcpp attributes in your project, you're probably OK.

1. `DESCRIPTION`: The package must have "Rcpp" listed under `LinkingTo` and under `Imports`.  `R6` must be listed under `Imports`.  The Rcpp requirements here are standard for packages using Rcpp attributes.  These will be set up automatically using `rcppr6::install()`.

2. `NAMESPACE`: Two requirements here:
  * Must import *something* from Rcpp.  The [Rcpp mailing list](http://permalink.gmane.org/gmane.comp.lang.r.rcpp/6744) suggests importing `evalCpp`.
  * Must import *something* from R6.  I suggest `R6::R6Class`.
  * Must load the package's dynamic library (of course)
If you use roxygen these will be automatically set up for you by leaving the appropriate `@importFrom` directives in an R file.

3. A file `inst/include/<package_name>.h` must exist ("main package header file").  This is the convention used by Rcpp attributes and is required for use by the `LinkingTo` convention.  This file must include the definitions of classes that you want to export.  It also needs to include two files:
  - `inst/include/<package_name>/rcppr6_pre.hpp` must be included *after* classes have been declared, but *before* `Rcpp.h` has been included.  This is often a pain, especially if you want to use Rcpp types within the class.  It may be sufficient to forward declare the classes that you export, but this will work badly with templated classes potentially (e.g., you can write `class foo;` but not `class foo<bar>`).  This reason for this load order is outlined in the "[Extending Rcpp](http://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-extending.pdf)" manual -- this file contains the prototypes for "non-intrusive extension".
  - `inst/include/<package_name>/rcppr6_post.hpp`, which may be included last in the main package header file (but must be included).  `Rcpp.h` can be safely loaded before this file, and this file will itself include `Rcpp.h` if it has not been loaded.

4. `src/Makevars` must be set up to add `../inst/include/` to the header search path so that we can find the main package header.  This will be automatically added by `rcppr6::install()`, but the file can simply contain a line saying `PKG_CPPFLAGS += -I../inst/include/`

# Installation/updating

We look after a bunch of files.

* `inst/include/<package_name>/rcppr6_pre.hpp`
* `inst/include/<package_name>/rcppr6_post.hpp`
* `inst/include/<package_name>/rcppr6_support.hpp`
* `src/rcppr6.cpp`
* `R/rcppr6.R`

These files are entirely rcppr6's - don't add anything to them.  Upgrades might totally alter these files at any point.  There is a little warning at the top that indicates this!  The contents of these files will morph and change, and running `install()` / `rcppr6()` may alter the contents of these files.  This is similar to the strategy used by Rcpp attributes.

# Configuration

A package may have a file `inst/rcppr6.yml` containing overall configuration information.  If this file is absent, a default configuration is used.  This is always available from rcppr6 (`as.yaml(rcppr6:::config_default())`) and is currently:

```
classes: inst/rcppr6_classes.yml
```

This indicates the files to search though.  Multiple files can be given:

```
classes:
  - inst/part1.yml
  - inst/part2.yml
```

These will be read together before any processing happens, so the order does not matter.  They are intepreted relative to the package root.

It's not totally clear that keeping these files in `inst/` is the best bet, but seems preferable to many options.  Having the file in inst means that it may be possible in future to define concrete versions of template classes defined in another package.  If the file moves anywhere it will probably be into the root as `.rcppr6.yml`, which means that that file need adding to `.Rbuildignore`.
