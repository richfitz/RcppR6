## There are a bunch of issues with mangling.  I *still* need R and
## C++ dual viability.  It might be better long term to abandon
## readability and just use C++ mangling!  Probably not.  This *all*
## needs revisiting after allowing >1 template argument.  But I think
## that using '___' or '_T_' to separate class and template
## definitions should be easy enough and avoid issues.  Needs
## documenting and the restrictions on names needs checking
## throughout.
mangle_template_type <- function(class_r, template_type) {
  sprintf("%s___%s", class_r, template_type)
}

mangle_r6_generator <- function(class_r) {
  sprintf(".R6_%s", class_r)
}
