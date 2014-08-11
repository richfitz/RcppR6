## This file refers using C++ templating rather than whisker text
## substitutions.  It's all a bit unfortunate, really.
## Construct full C++ template names:
cpp_template_name <- function(template, pars) {
  paste0(template, cpp_template_parameters(pars))
}

cpp_template_parameters <- function(pars) {
  pars <- paste(pars, collapse=", ")
  if (grepl(">$", pars)) {
    pars <- paste0(pars, " ")
  }
  sprintf("<%s>", pars)
}

cpp_template_rewrite_types <- function(x, template) {
  from <- names(template$parameters)
  to <- unname(template$parameters)

  ## First do any literals:
  i <- match(x, from)
  j <- !is.na(i)
  x[j] <- to[i[j]]

  ## Sort out templated types.  This is very basic, probably prone to
  ## failure.  But it serves as an interface at least.
  ##
  if (any(k <- !j & grepl("<", x, fixed=TRUE))) {
    if (any(k)) {
      if (length(from) != 1) {
        stop("Only working for single types at the moment")
      }
      re <- sprintf("<[[:space:]]*%s[[:space:]]*>", from)
      x[k] <- sub(re, cpp_template_parameters(to), x[k])
    }

    ## It's possible that I'm overthinking this and we can just
    ## subtsitute directly for things that are on word boundaries.
    ##   sub("\\bT\\b", "U", "MyType<T>")
    ## which allows easy substitution of all types.  I.e.:
    ##
    ## xk <- x[k]
    ## for (i in seq_along(from)) {
    ##   xk <- gsub(sprintf("\\b%s\\b", from[i]),
    ##              cpp_template_parameters(to[i]), xk)
    ## }
    ## x[k] <- xk
  }
  x
}

cpp_template_rewrite_constructor <- function(x, template) {
  x$roxygen <- NULL
  x$name_cpp <- cpp_template_rewrite_types(x$name_cpp, template)
  x$args$type <- cpp_template_rewrite_types(x$args$type, template)
  x
}

cpp_template_rewrite_method <- function(x, template) {
  if (x$access == "function") {
    x$name_cpp <- cpp_template_rewrite_types(x$name_cpp, template)
  }
  x$args$type <- cpp_template_rewrite_types(x$args$type, template)
  x$return_type <- cpp_template_rewrite_types(x$type, template)
  x
}

cpp_template_rewrite_active <- function(x, template) {
  if (x$access == "function") {
    x$name_cpp <- cpp_template_rewrite_types(x$name_cpp, template)
  }
  x$type <- cpp_template_rewrite_types(x$type, template)
  x
}

cpp_template_rewrite_class <- function(x, template) {
  ## Turn off templating and rewrite names:
  x$templates <- list(is_templated=FALSE)
  x$inherits  <- x$name_safe
  x$name_r    <- template$name_r
  x$name_safe <- template$name_safe
  x$name_cpp  <- template$name_cpp
  x$constructor <-
    cpp_template_rewrite_constructor(x$constructor, template)
  x$methods <- lapply(x$methods, cpp_template_rewrite_method, template)
  x$active  <- lapply(x$active, cpp_template_rewrite_active, template)
  x
}
