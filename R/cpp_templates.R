## This file refers using C++ templating rather than whisker text
## substitutions.  It's all a bit unfortunate, really.
cpp_template_name <- function(template, pars) {
  sprintf("%s<%s>", template, cpp_template_parameters(pars))
}

cpp_template_parameters <- function(pars) {
  pars <- paste(pars, collapse=", ")
  cpp_pad_template(pars)
}

cpp_pad_template <- function(str) {
  if (grepl(">$", str)) {
    str <- paste0(str, " ")
  }
  str
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

## The idea here is to generate a concrete type by rewriting all the
## <T> bits with the concrete representation.
cpp_template_rewrite_class <- function(defn, parent, parent_class) {
  ret <- parent_class

  ret$name_r    <- defn$name_r
  ret$name_cpp  <- defn$name_cpp
  ret$name_safe <- defn$name_safe
  ret$inherits  <- parent_class$name_safe
  ret$is_templated <- FALSE

  ## Bunch of type rewriting:
  ret$constructor <- cpp_template_rewrite_constructor(ret$constructor, defn)
  ret$methods <- lapply(ret$methods, cpp_template_rewrite_method, defn)
  ret$active  <- lapply(ret$active,  cpp_template_rewrite_active, defn)

  ret
}

cpp_template_rewrite_constructor <- function(defn, concrete) {
  defn$roxygen <- NULL
  defn$name_cpp <- cpp_template_rewrite_types(defn$name_cpp, concrete)
  defn$args     <- cpp_template_rewrite_args(defn$args, concrete)
  defn
}

cpp_template_rewrite_method <- function(defn, concrete) {
  if (defn$access == "function") {
    defn$name_cpp <- cpp_template_rewrite_types(defn$name_cpp, concrete)
  }
  defn$return_type <- cpp_template_rewrite_types(defn$return_type, concrete)
  defn$args <- cpp_template_rewrite_args(defn$args, concrete)
  defn
}

cpp_template_rewrite_active <- function(defn, concrete) {
  if (defn$access == "function") {
    defn$name_cpp <- cpp_template_rewrite_types(defn$name_cpp, concrete)
    if (!is.null(defn$name_cpp_set)) {
      defn$name_cpp_set <-
        cpp_template_rewrite_types(defn$name_cpp_set, concrete)
    }
  }
  defn$type <- cpp_template_rewrite_types(defn$type, concrete)
  defn
}

cpp_template_rewrite_args <- function(defn, concrete) {
  defn$types <- cpp_template_rewrite_types(defn$types, concrete)
  ## TODO: I think this can actually just be concrete$name_cpp
  defn$parent_class_name_cpp <-
    cpp_template_rewrite_types(defn$parent_class_name_cpp, concrete)
  defn
}
