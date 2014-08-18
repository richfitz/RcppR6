## This file refers using C++ templating rather than whisker text
## substitutions.  It's all a bit unfortunate, really.
cpp_template_name <- function(template, pars) {
  sprintf("%s<%s>", template, cpp_template_parameters(pars))
}

cpp_template_parameters <- function(pars) {
  pars <- paste(pars, collapse=", ")
  if (grepl(">$", pars)) {
    pars <- paste0(pars, " ")
  }
  pars
}

cpp_template_rewrite_class <- function(obj) {
  generic <- obj$parent$parent

  ## Build a new class but with templates disabled.  This could be
  ## achived more easily if we had a copy() method.  Need to delete
  ## the templates *first* because otherwise we get a loop.
  defn <- generic$defn_yaml
  defn[[1]]$templates <- NULL # disable templating
  ret <- RcppR6_class$new(defn, generic$package())

  ret$name_r    <- obj$name_r
  ret$name_cpp  <- obj$name_cpp
  ret$name_safe <- obj$name_safe
  ret$inherits  <- generic$name_r

  ## Bunch of type rewriting:
  cpp_template_rewrite_constructor(ret$constructor, obj)
  for (x in ret$methods) {
    cpp_template_rewrite_method(x, obj)
  }
  for (x in ret$active) {
    cpp_template_rewrite_active(x, obj)
  }

  ## The yaml definition is invalid now so drop them:
  ret$defn <- ret$defn_yaml <- NULL

  ret
}

cpp_template_rewrite_constructor <- function(obj, concrete) {
  obj$roxygen <- NULL
  obj$name_cpp <- cpp_template_rewrite_types(obj$name_cpp, concrete)
  cpp_template_rewrite_args(obj$args, concrete)
}

cpp_template_rewrite_method <- function(obj, concrete) {
  if (obj$access == "function") {
    obj$name_cpp <- cpp_template_rewrite_types(obj$name_cpp, concrete)
  }
  obj$return_type <- cpp_template_rewrite_types(obj$return_type, concrete)
  cpp_template_rewrite_args(obj$args, concrete)
}

cpp_template_rewrite_active <- function(obj, concrete) {
  if (obj$access == "function") {
    obj$name_cpp <- cpp_template_rewrite_types(obj$name_cpp, concrete)
    if (!is.null(obj$name_cpp_set)) {
      obj$name_cpp_set <-
        cpp_template_rewrite_types(obj$name_cpp_set, concrete)
    }
  }
  obj$type <- cpp_template_rewrite_types(obj$type, concrete)
}

cpp_template_rewrite_args <- function(obj, concrete) {
  obj$types <- cpp_template_rewrite_types(obj$types, concrete)
}

cpp_template_rewrite_types <- function(x, template) {
  from <- names(template$parameters_cpp)
  to <- unname(template$parameters_cpp)

  ## First do any literals:
  i <- match(x, from)
  j <- !is.na(i)
  x[j] <- to[i[j]]

  ## Sort out templated types.  This is very basic, probably prone to
  ## failure.  But it serves as an interface at least.
  if (any(k <- !j & grepl("<", x, fixed=TRUE))) {
    if (any(k)) {
      xk <- x[k]
      for (i in seq_along(from)) {
        xk <- gsub(sprintf("\\b%s\\b", from[i]),
                   cpp_template_parameters(to[i]), xk)
      }
      x[k] <- xk
    }
  }
  x
}
