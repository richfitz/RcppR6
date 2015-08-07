## TODO: The thing that is needed here is the backtrace that I
## generated last time.  That was pretty useful.
RcppR6_validate <- function(dat) {
  if (length(dat$classes) > 0) {
    assert_list(dat$classes)
    assert_named(dat$classes)
    dat$classes <- lapply(seq_along(dat$classes), function(i)
      RcppR6_validate_class(dat$classes[i]))
    dat$functions <- lapply(seq_along(dat$functions), function(i)
      RcppR6_validate_function(dat$functions[i], dat$classes))
  }

  dat
}

RcppR6_validate_class <- function(defn) {
  if ("list" %in% names(defn[[1]])) {
    RcppR6_validate_class_list(defn)
  } else {
    RcppR6_validate_class_ref(defn)
  }
}

RcppR6_validate_class_ref <- function(defn) {
  valid <- c("name_cpp", "forward_declare",
             "constructor", "methods", "active",
             "templates")
  ret <- RcppR6_validate_common(defn, valid)
  defn <- ret$defn
  ret$defn <- NULL

  ret <- modifyList(ret,
                    RcppR6_validate_forward_declare(defn$forward_declare))

  ret$constructor <- RcppR6_validate_constructor(defn$constructor, ret)
  ret$methods     <- RcppR6_validate_method_list(defn$methods, ret)
  ret$active      <- RcppR6_validate_active_list(defn$active, ret)
  ret$templates   <- RcppR6_validate_templates(defn$templates, ret)

  ret$is_templated <- !is.null(ret$templates)
  ret$type <- "class_ref"

  ret
}

## This checks that the yaml chunk looks reasonable and checks names.
## It's used by:
##   class
##   constructor
##   method
##   active
RcppR6_validate_common <- function(defn, valid=NULL) {
  assert_scalar_list(defn)
  assert_named(defn)
  ret <- list()
  ret$name_r <- names(defn)
  ret$defn <- defn[[1]]
  if (!is.null(valid)) {
    warn_unknown(ret$name_r, ret$defn, valid)
  }
  ret$name_safe <- RcppR6_validate_name(ret$name_r)
  ## NOTE: Be careful about partial matching on name_cpp / name_cpp_set
  ret$name_cpp  <- with_default(ret$defn[["name_cpp"]], ret$name_r)
  assert_scalar_character(ret$name_cpp)
  ret
}

## TODO: all of these build the returned object piecewise; better to
## do that all at once.
RcppR6_validate_constructor <- function(defn, parent) {
  if (!is.null(defn)) {
    assert_list(defn)
    assert_named(defn)
  }
  warn_unknown("constructor", defn, c("roxygen", "name_cpp", "args"))

  ret <- list()
  ret$name_cpp <- with_default(defn$name_cpp, parent$name_cpp)
  assert_scalar_character(ret$name_cpp)

  if (!is.null(defn$roxygen)) {
    ret$roxygen <- defn$roxygen
    assert_scalar_character(ret$roxygen)
  }

  ret$args <- RcppR6_validate_args(defn$args, ret, "constructor", parent)
  ret
}

RcppR6_validate_args <- function(defn, parent, parent_type, parent_class) {
  defn_args <- yaml_seq_map(defn)
  ret <- list()
  ret$names <- names(defn_args)

  contents <- vcapply(defn_args, first, USE.NAMES=FALSE)
  re_default <- "\\s*=\\s*"
  if (any(grepl(re_default, contents))) {
    info <- strsplit_first(contents, re_default)
    ret$types <- info[, 1]
    ret$defaults <- info[, 2]
  } else {
    ret$types <- contents
    ret$defaults <- NULL
  }

  ## These are both needed later on, in the templating stage.  This
  ## might change.
  ## TODO: consider "function_type" rather than "parent_type"?
  ## or even "parent_function_type".  Affects RcppR6_generate_args()
  ret$parent_type <- match_value(parent_type,
                                 c("constructor", "member", "function"))
  ret$parent_class_name_cpp <- parent_class$name_cpp

  ret
}

RcppR6_validate_method_list <- function(defn, parent) {
  if (length(defn) > 0) {
    assert_list(defn)
    assert_named(defn)
    lapply(seq_along(defn), function(i) RcppR6_validate_method(defn[i], parent))
  }
}

RcppR6_validate_active_list <- function(defn, parent) {
  if (length(defn) > 0) {
    assert_list(defn)
    assert_named(defn)
    lapply(seq_along(defn), function(i) RcppR6_validate_active(defn[i], parent))
  }
}

RcppR6_validate_method <- function(defn, parent) {
  valid <- c("name_cpp", "return_type", "access", "args")
  ret <- RcppR6_validate_common(defn, valid)
  defn <- ret$defn
  ret$defn <- NULL

  ret$return_type <- defn$return_type
  assert_scalar_character(ret$return_type)

  access <- with_default(defn$access, "member")
  ret$access <- match_value(access, c("member", "function"))
  assert_scalar_character(ret$access)

  ret$args <- RcppR6_validate_args(defn$args, ret, ret$access, parent)
  ret
}

RcppR6_validate_active <- function(defn, parent) {
  valid <- c("name_cpp", "name_cpp_set", "type", "access",
             "readonly")
  ret <- RcppR6_validate_common(defn, valid)

  defn <- ret$defn
  ret$defn <- NULL

  ret$type <- defn$type
  assert_scalar_character(ret$type)

  ret$access <- match_value(defn$access,
                            c("field", "member", "function"))
  assert_scalar_character(ret$access)

  if (ret$access == "field") {
    ret$readonly <- with_default(defn$readonly, FALSE)
    assert_scalar_logical(ret$readonly)
    if (!is.null(defn$name_cpp_set)) {
      stop('name_cpp_set may not be given when access is "field"')
    }
  } else {
    ret$name_cpp_set <- defn$name_cpp_set
    ret$readonly <- is.null(ret$name_cpp_set)
    if (!ret$readonly) {
      assert_scalar_character(ret$name_cpp_set)
    }
  }

  ret
}

RcppR6_validate_templates <- function(defn, parent) {
  if (length(defn) > 0) {
    ret <- list()
    warn_unknown("templates", defn, c("parameters", "concrete"))
    ret$parameters <- defn$parameters
    assert_character(ret$parameters)
    assert_nonempty(ret$parameters)
    if (any(grepl("[^[:alnum:]_.]", ret$parameters))) { # check for "T1, T2"
      stop("Parameters need to be given as a yaml list of valid identifiers")
    }

    ## Simplest solution of several possibilities:
    re <- sprintf("[[:space:]]*<%s>",
                  paste(sprintf("[[:space:]]*%s[[:space:]]*",
                                ret$parameters), collapse=","))
    if (!grepl(re, parent$name_cpp)) {
      stop("name_cpp must be a templated type")
    }

    ret$concrete <- RcppR6_validate_concrete_list(defn$concrete, ret, parent)
    ret
  }
}

RcppR6_validate_concrete_list <- function(defn, parent, parent_class) {
  if (length(defn) > 0) {
    if (is.character(defn)) {
      defn <- as.list(defn)
    }
    assert_list(defn)
    ret <- lapply(seq_along(defn), function(i)
      RcppR6_validate_concrete(defn[i], parent, parent_class))

    parameters_r <- lapply(ret, "[[", "parameters_r")
    if (any(duplicated(parameters_r))) {
      dups <- parameters_r[duplicated(parameters_r)]
      stop(sprintf("Duplicated parameter names in class %s: %s",
                   parent_class$name_r, paste(dups, collapse=", ")))
    }
    ret
  }
}

RcppR6_validate_concrete <- function(defn, parent, parent_class) {
  ## TODO: This is required, but I don't remember why that is the case...
  defn <- defn[[1]]
  ## This allows  '- int' to be treated as '- [int: int]'
  x <- unlist(yaml_seq_map(as.list(defn), named=FALSE))
  ok <- (is.character(x) && !is.null(names(x)) &&
         length(x) == length(parent$parameters))
  if (!ok) {
    stop("Invalid concrete representation.\n\t", yaml::as.yaml(defn))
  }

  ret <- list()
  ret$parameters_r    <- setNames(names(x),  parent$parameters)
  ret$parameters_cpp  <- setNames(unname(x), parent$parameters)
  ret$parameters_safe <- RcppR6_validate_name(ret$parameters_r)
  ret$name_r          <- mangle_template_type_r(parent_class$name_r,
                                                ret$parameters_r)
  ret$name_cpp        <- cpp_template_rewrite_types(parent_class$name_cpp, ret)
  ret$name_safe       <- mangle_template_type(parent_class$name_safe,
                                              ret$parameters_safe)

  ret$class <- cpp_template_rewrite_class(ret, parent, parent_class)
  ret
}

RcppR6_validate_class_list <- function(defn) {
  valid <- c("name_cpp", "forward_declare", "list",
             "templates", "roxygen", "validator")
  ret <- RcppR6_validate_common(defn, valid)
  defn <- ret$defn
  ret$defn <- NULL

  ## Check the actual list here, in self$list; basically all we need
  ## is a named list with no duplicate names, and every element of
  ## this is a character vector.  Pretty easy!
  ret$list <- yaml_seq_map(defn$list)
  assert_named(ret$list)
  if (!all(vlapply(ret$list, is_scalar_character))) {
    stop("All elements of 'list' must be a scalar character")
  }

  if (!is.null(defn$roxygen)) {
    ret$roxygen <- defn$roxygen
    assert_scalar_character(ret$roxygen)
  }

  if (!is.null(defn$validator)) {
    ret$validator <- RcppR6_validate_validator(defn$validator, ret)
  }

  ret <- modifyList(ret,
                    RcppR6_validate_forward_declare(defn$forward_declare))

  ret$templates <- RcppR6_validate_templates(defn$templates, ret)
  ret$is_templated <- !is.null(ret$templates)

  ret$type <- "class_list"

  ret
}

RcppR6_validate_forward_declare <- function(defn) {
  ## Three major options: FALSE / TRUE / {class | struct}
  ret <- list(forward_declare=with_default(defn, FALSE))
  if (isFALSE(ret$forward_declare)) {
    ret$forward_declare <- FALSE
    ret$forward_declare_type <- NULL
  } else if (isTRUE(ret$forward_declare)) {
    ret$forward_declare_type <- "class"
  } else {
    ret$forward_declare_type <- match_value(ret$forward_declare,
                                             c("class", "struct"))
    ret$forward_declare <- TRUE
  }
  ret
}

## This might change to check on both R and C sides.
## Not checked:
##   Can't start with a number
##   If it starts with a period, second character must be a letter
##   Can't be a reserved word (in either language)
## http://stackoverflow.com/questions/15285787/can-you-start-a-class-name-with-a-numeric-digit
RcppR6_validate_name <- function(x) {
  x <- gsub(".", "_", x, fixed=TRUE)
  if (any(i <- grepl("[^[:alnum:]_]", x))) {
    stop("Name ", collapse(dQuote(x[i])), " does not look valid in R & C")
  }
  x
}

RcppR6_validate_validator <- function(defn, parent) {
  ret <- list()
  ret$name_cpp  <- defn$name_cpp
  ret$name_safe <- mangle_validator(parent$name_safe)
  assert_scalar_character(ret$name_cpp)
  access <- with_default(defn$access, "member")
  ret$access <- match_value(access, c("member", "function"))
  if (ret$access != "member") {
    ## TODO: To support free functions we need to generalise the template
    ## rcpp_list_definitions.whisker; specificially:
    ##
    ##   {{{#class.validator}}}
    ##     ret.{{{class.validator.name_cpp}}}();
    ##   {{{/class.validator}}}
    ##
    ## See method_cpp.whisker for how to do this; it's not that hard.
    ## I'll hold off implementing it until this basically works
    ## though.  I also need to get the template rewriting done and
    ## that'll be easier to do if there's only one moving part (free
    ## functions and members have different rewriting rules).
    stop("Not yet supported")
  }
  ret
}

RcppR6_validate_function <- function(defn, classes) {
  valid <- c("name_cpp", "templates", "args", "return_type")
  ret <- RcppR6_validate_common(defn, valid)
  defn <- ret$defn
  ret$defn <- NULL

  ## These are copied over from RcppR6_validate_method:
  ret$return_type <- defn$return_type
  assert_scalar_character(ret$return_type)

  ret$args <- RcppR6_validate_args(defn$args, ret, "function", ret)
  ret$args <- rename(ret$args, "parent_class_name_cpp", "generic_name_cpp")

  ret$type <- "function"

  ## What this does is different to the class approach; this is going
  ## to look at the class definition and work out what the allowable
  ## types are.
  ret$templates <-
    RcppR6_validate_function_templates(defn$templates, classes, ret)

  ## And this also varies.  Rather than iterating over ret$concrete
  ## (which does not exist here) we iterate over the same within the
  ## class, which at this point has been validated for us.
  tmp <- ret$templates$class$templates$concrete
  ret$concrete <- RcppR6_validate_function_concrete(tmp, ret)

  ret
}

RcppR6_validate_function_templates <- function(defn, classes, parent) {
  assert_list(defn)
  warn_unknown("templates", defn, c("class", "parameters", "concrete"))

  ret <- list()
  ret$parameters <- defn$parameters
  assert_scalar_character(ret$parameters)

  class_names <- vcapply(classes, "[[", "name_r")
  class_name <- match_value(defn$class, class_names)
  i <- match(class_name, class_names)
  ret$class <- classes[[i]]

  concrete <- vcapply(ret$class$templates$concrete, "[[", "parameters_r")
  if (is.null(defn$concrete)) {
    defn$concrete <- concrete
  } else {
    assert_character(defn$concrete)
    nok <- setdiff(defn$concrete, concrete)
    if (length(nok) > 0) {
      stop(sprintf("Unknown concrete types %s", collapse(nok)))
    }
    ret$concrete <- defn$concrete
  }

  ret
}

## TODO: These can be merged and simplified considerably...
RcppR6_validate_function_concrete <- function(defn, parent) {
  if (length(defn) > 0) {
    assert_list(defn)
    lapply(defn, cpp_template_rewrite_function, parent)
  }
}
