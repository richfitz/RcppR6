mangle_template_type <- function(class, template_type) {
  sprintf("%s___%s", class, paste(template_type, collapse="__"))
}

## There are a few different options for nice mangling of templated
## names in R:
##   Container.Contents   # Not-quite-S3 style
##   Container<Contents>  # C++ style
##   Container(Contents)  # New constructor style
##   Container___Contents # Safe mangling style
## The middle two will work the best for display and for dealing with
## multiple parameters.  However, it's not syntactically valid, so
## needs to go into backticks.  That's not actually that bad as it
## will encourage using the generic type.
mangle_template_type_r <- function(class, template_type) {
  sprintf("%s<%s>", class, paste(template_type, collapse=","))
}

mangle_R6_generator <- function(class) {
  sprintf(".R6_%s", class)
}

mangle_active <- function(class, name, direction) {
  direction <- match_value(direction, c("get", "set"))
  sprintf("%s__%s__%s", class, name, direction)
}

mangle_method <- function(class, name) {
  sprintf("%s__%s", class, name)
}

mangle_constructor <- function(class) {
  sprintf("%s__ctor", class)
}

mangle_validator <- function(class) {
  sprintf("%s__vdor", class)
}

mangle_input <- function(package, name_cpp) {
  sprintf("%s::RcppR6::RcppR6<%s>",
          package, cpp_template_parameters(name_cpp))
}
