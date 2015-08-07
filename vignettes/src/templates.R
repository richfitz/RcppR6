## ---
## title: "RcppR6 templates"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{RcppR6 templates}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

## One of the reasons for writing RcppR6 is for exporting templated
## classes.  I think I have the basics working reasonably well here,
## but this is definitely an area that might get changed.

## The problem is this: if you have some templated type, say
## ```std::pair<T,U>```, then you need to write wrappers for all the
## types `T` and `U` that you need, and arrange for the correct
## dispatch on the R side.

## The way that this is done behind the scenes in RcppR6 is not
## particularly pretty and might change.

## To see how this works, we'll start wrapping `std::pair`.  This is
## just a tuple of data of some type.  C++ will need to know the
## *actual* types.  This example is in the `templates` example package.

## First, consider the simple case of a pair of the same type.  To do
## this, here's a small class definition (in
## `inst/include/templates/pair1.hpp`).
##+ echo=FALSE, results="asis"
set.seed(1)
source(system.file("vignette_common.R", package="RcppR6"))
path <- vignette_prepare("templates")
cpp_output(readLines(file.path(path, "inst/include/templates/pair1.hpp")))

## There's not much going on here: this is just a class that stores
## two things of the same type.  It's fairly compatible with
## `std::pair`, having members `first` and `second`.  Note that this
## is in the `examples` namespace (namespaces are optional but
## supported).

## Suppose we want to generate an interface for this class supporting
## integers, doubles and strings.  We can write yaml:
##+ echo=FALSE, results="asis"
yaml <- readLines(file.path(path, "inst/RcppR6_classes.yml"))
i_pair2 <- grep("pair2:", yaml)
yaml1 <- yaml[1:(i_pair2 - 2)]
yaml2 <- yaml[i_pair2:length(yaml)]
i_templates <- grep("\\s+templates:", yaml1)[[1]]
i_constructor <- grep("\\s+constructor:", yaml1)[[1]]
i_active <- grep("\\s+active:", yaml1)[[1]]
yaml_output(yaml1)

## There's a new section here compared with the previous classes:
## `templates:`.  The presence of this element means that RcppR6 will
## generate templated classes.
##+ echo=FALSE, results="asis"
yaml_output(yaml1[i_templates:(i_constructor - 1)])

## The `parameters:` field indicates which bits of the full name
## `name_cpp:`, here `examples::pair1<T>`, are types.
##
## This is paired with a field `concrete` which contains a list of
## substitutions.  So this will create interfaces for
## `examples::pair1<int>`, `examples::pair1<double>` and
## `examples::pair1<std::string>`.  The `std::string` type contains an
## *alias* here to `string`; this is the name that will be used on the
## R side (see below).

## After that is `constructor:` and `active:` fields the same as
## before.  However, types with a `T` (or whatever was declared in the
## `parameters:` field) can be used and they will be mapped onto a
## concrete type in the generated object.  So `first:` will return an
## `int` from a `examples::pair1<int>` for example.
RcppR6::install(path)

## Run `devtools::document` to create the `NAMESPACE` file
devtools::document(path)
## and load the generated code:
devtools::load_all(path)

## RcppR6 has generated a `pair` function that takes an argument `T`;
## this is the *name* of the type.  (In theory, S3 dispatch could be
## better here, with the the generator as a generic function, but that
## would require that the templated type was always first)
args(pair1)

## Specifying a type here, returns a function that takes the
## arguments `a` and `b`.
args(pair1("int"))

## which we could use like:
p <- pair1("int")(1L, 2L)

## The generated object can be used according to the interface
## specified above: all it has are read/write fields that type
## integers:
p$first
p$first <- 10
p$second
p$second <- 20

## and these fields are restricted to being integers:
##+ error=TRUE
p$second <- "second"
p$second

## The object has multiple S3 types:
class(p)

## ...so generic functions can be written for `pair1` and they'll
## dispatch for all `pair` types.  If special treatment is required
## for a single type, then use `pair`<int>`.

## Similarly, for `pair1<double>`:
p_double <- pair1("double")(exp(1), pi)
p_double$first
p_double$second
class(p_double)

## ...and for `pair1<std::string>`:
p_string <- pair1("string")("first", "second")
p_string$first
p_string$second
class(p_string)

## Similarly, template types can be generated for types that have more
## than one template parameter, such as `std::pair` itself.
##+ echo=FALSE, results="asis"
yaml_output(yaml2)

## This is basically the same as above, except that:
##
## * an ordered of type parameters are given for `parameters`
## * the concrete types are given as yaml lists or ordered maps (to
## handle renaming).
##
## Apart from that, nothing is different.

## This is already compiled in from above.  `pair` takes two arguments:
args(pair2)

## and is initialised in the same way as above: types go in the first
## call, arguments in the second.  This generates a `std::pair<int,
## double>`:
p2 <- pair2("int", "double")(1L, pi)
p2$first
p2$second

## and this generates a `std::pair<std::string, double>`
p2 <- pair2("string", "double")("first", pi)
p2$first
p2$second

## The approach RcppR6 takes is very naive and will just go ahead and
## generate a lot of boilerplate.  That could create large binaries
## (though probably no larger than `boost::variant` or `boost::any`).

## # Using templated types in functions

## Now we have a problem.  With non-templated types we can use Rcpp to
## easily write functions that use the generated classes.  You can
## still do that for fully-specified types:

## ```c++
## // [[Rcpp::export]]
## int first(pair<int, double> x) {
##   return x.first;
## }
## ```

## But how to write a function that would return the first of *any*
## pair?  This *will not work*:

## ```c++
## // [[Rcpp::export]]
## template <typename T, typename U>
## T first(pair<T, U> x) {
##   return x.first;
## }
## ```

## It won't work because Rcpp does not know what combinations of `T`
## and `U` to generate code for.  RcppR6 can help here by doing some
## code generation for you.

## # Contents of generated files:

## `inst/include/templates/RcppR6_pre.hpp`:
##+ echo=FALSE, results="asis"
cpp_output(readLines(file.path(path, "inst/include/templates/RcppR6_pre.hpp")))

## `inst/include/templates/RcppR6_post.hpp`:
##+ echo=FALSE, results="asis"
cpp_output(readLines(file.path(path, "inst/include/templates/RcppR6_post.hpp")))

## `inst/include/templates/RcppR6_support.hpp`:
##+ echo=FALSE, results="asis"
cpp_output(readLines(file.path(path, "inst/include/templates/RcppR6_support.hpp")))

## `R/RcppR6.R`:
##+ echo=FALSE, results="asis"
r_output(readLines(file.path(path, "R/RcppR6.R")))

## `R/RcppR6.R`:
##+ echo=FALSE, results="asis"
cpp_output(readLines(file.path(path, "src/RcppR6.cpp")))
