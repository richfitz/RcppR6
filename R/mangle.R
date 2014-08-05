## There are a bunch of issues with mangling.  I *still* need R and
## C++ dual viability.  It might be better long term to abandon
## readability and just use C++ mangling!  Probably not.  This *all*
## needs revisiting after allowing >1 template argument.  But I think
## that using '___' or '_T_' to separate class and template
## definitions should be easy enough and avoid issues.  Needs
## documenting and the restrictions on names needs checking
## throughout.
mangle_template_type <- function(class, template_type) {
  sprintf("%s___%s", class, template_type)
}

mangle_r6_generator <- function(class) {
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
