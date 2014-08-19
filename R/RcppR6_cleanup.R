## These functions are actually all out-of-line methods, operating on
## their respective classes.  They're self-destructing, in that once
## they're included in the classes they will be removed.

cleanup_class <- function(...) {
  cleanup_common(self, c("name_cpp", "forward_declare",
                         "constructor", "methods", "active",
                         "templates", "inherits"))

  ## Three major options: FALSE / TRUE / {class | struct}
  self$forward_declare <- with_default(self$defn$forward_declare, FALSE)
  if (identical(self$forward_declare, FALSE)) {
    self$forward_declare <- FALSE
    self$forward_declare_type <- NULL
  } else if (identical(self$forward_declare, TRUE)) {
    self$forward_declare_type <- "class"
  } else {
    self$forward_declare_type <- match_value(self$forward_declare,
                                             c("class", "struct"))
    self$forward_declare <- TRUE
  }

  if (!is.null(self$defn$inherit)) {
    self$inherit <- self$defn$inherit
    assert_character(self$inherit)
  }

  self$constructor <-
    RcppR6_constructor$new(self$defn$constructor, self)
  self$methods <-
    RcppR6_method_list(self$defn$methods, self)
  self$active <-
    RcppR6_active_list(self$defn$active, self)
  self$templates <-
    RcppR6_templates$new(self$defn$templates, self)
}

## We should allow a missing constructor for things that are default
## constructible.  Not yet supported, but see how this goes.
cleanup_constructor <- function(...) {
  self$defn <- self$defn_yaml
  self$defn_yaml <- list(constructor=self$defn_yaml)
  if (!is.null(self$defn)) {
    assert_list(self$defn)
    assert_named(self$defn)
  }
  warn_unknown("constructor", self$defn, c("roxygen", "name_cpp", "args"))

  self$name_cpp <- with_default(self$defn$name_cpp, self$parent$name_cpp)
  assert_scalar_character(self$name_cpp)

  if (!is.null(self$defn$roxygen)) {
    self$roxygen <- self$defn$roxygen
    assert_scalar_character(self$roxygen)
  }

  self$args <- RcppR6_args$new(self$defn$args, self)
}

## The problem here is that we're really doing several sets of things
## with the sanitisation: we're checking that things are OK and we're
## filling in default values, *and* we're remapping things onto
## different keys.  This is OK, but makes it hard to ensure
## transitiveness.
cleanup_method <- function(...) {
  cleanup_common(self, c("name_cpp", "return_type", "access", "args"))

  self$return_type <- self$defn$return_type
  assert_scalar_character(self$return_type)

  access <- with_default(self$defn$access, "member")
  self$access <- match_value(access, c("member", "function"))
  assert_scalar_character(self$access)

  self$args <- RcppR6_args$new(self$defn$args, self)
}

## Will become sanitise again
cleanup_active <- function(...) {
  cleanup_common(self, c("name_cpp", "name_cpp_set", "type", "access",
                        "readonly"))

  self$type <- self$defn$type
  assert_scalar_character(self$type)

  self$access <- match_value(self$defn$access,
                            c("field", "member", "function"))
  assert_scalar_character(self$access)

  if (self$access == "field") {
    self$readonly <- with_default(self$defn$readonly, FALSE)
    assert_scalar_logical(self$readonly)
    if (!is.null(self$defn$name_cpp_set)) {
      stop('name_cpp_set may not be given when access is "field"')
    }
  } else {
    self$name_cpp_set <- self$defn$name_cpp_set
    self$readonly <- is.null(self$name_cpp_set)
    if (!self$readonly) {
      assert_scalar_character(self$name_cpp_set)
    }
  }
}

cleanup_templates <- function(...) {
  self$defn <- self$defn_yaml
  self$defn_yaml <- list(args=self$defn_yaml)

  self$parent$is_templated <- length(self$defn) > 0
  if (self$parent$is_templated) {
    warn_unknown("templates", self$defn, c("parameters", "concrete"))
    self$parameters <- self$defn$parameters
    assert_character(self$parameters)
    assert_nonempty(self$parameters)
    if (any(grepl("[^[:alnum:]_.]", self$parameters))) { # check for "T1, T2"
      stop("Parameters need to be given as a yaml list of valid identifiers")
    }

    ## Simplest solution of several possibilities:
    re <- sprintf("[[:space:]]*<%s>",
                  paste(sprintf("[[:space:]]*%s[[:space:]]*",
                                self$parameters), collapse=","))
    if (!grepl(re, self$parent$name_cpp)) {
      stop("name_cpp must be a templated type")
    }

    self$concrete <-
      RcppR6_concrete_list(self$defn$concrete, self)
  }
}

cleanup_concrete <- function(...) {
  self$defn <- self$defn_yaml[[1]]
  parameters <- self$parent$parameters

  ## This allows  '- int' to be treated as '- [int: int]'
  x <- unlist(yaml_seq_map(as.list(self$defn), named=FALSE))
  ok <- (is.character(x) && !is.null(names(x)) &&
         length(x) == length(parameters))
  if (!ok) {
    stop("Invalid concrete representation.\n\t", yaml::as.yaml(self$defn))
  }

  generic <- self$parent$parent
  self$parameters_r    <- structure(names(x), names=parameters)
  self$parameters_cpp  <- structure(unname(x), names=parameters)
  self$parameters_safe <- cleanup_name(self$parameters_r)
  self$name_r          <- mangle_template_type_r(generic$name_r,
                                                 self$parameters_r)
  self$name_cpp        <- cpp_template_rewrite_types(generic$name_cpp, self)
  self$name_safe       <- mangle_template_type(generic$name_safe,
                                               self$parameters_safe)

  self$class <- cpp_template_rewrite_class(self)
}

cleanup_args <- function(...) {
  self$defn <- yaml_seq_map(self$defn_yaml)
  self$defn_yaml <- list(args=self$defn_yaml)
  ## TODO: store versions of the names for C++
  self$names <- names(self$defn)

  contents <- vapply(self$defn, first, character(1), USE.NAMES=FALSE)
  re_default <- "\\s*=\\s*"
  if (any(grepl(re_default, contents))) {
    info <- strsplit_first(contents, re_default)
    self$types <- info[,1]
    self$defaults <- info[,2]
  } else {
    self$types <- contents
    self$defaults <- NULL
  }
}

## Used within the above, not self-destructing:

## Really simple name sanitisation: convert dots to underscores :)
cleanup_name <- function(x) {
  x <- gsub(".", "_", x, fixed=TRUE)
  check_name(x)
  x
}

## This checks that the yaml chunk looks reasonable and checks names.
## It's used by:
##   class
##   constructor
##   method
##   active
cleanup_common <- function(obj, known=NULL) {
  assert_scalar_list(obj$defn_yaml)
  assert_named(obj$defn_yaml)
  obj$name_r <- names(obj$defn_yaml)
  obj$defn <- obj$defn_yaml[[1]]
  if (!is.null(known)) {
    warn_unknown(obj$name_r, obj$defn, known)
  }
  obj$name_safe <- cleanup_name(obj$name_r)
  obj$name_cpp  <- with_default(obj$defn$name_cpp, obj$name_r)
  assert_scalar_character(obj$name_cpp)
}

## This might change to check on both R and C sides.
## Not checked:
##   Can't start with a number
##   If it starts with a period, second character must be a letter
##   Can't be a reserved word (in either language)
## http://stackoverflow.com/questions/15285787/can-you-start-a-class-name-with-a-numeric-digit
check_name <- function(x) {
  if (any(i <- grepl("[^[:alnum:]_]", x))) {
    stop("Name ", collapse(dQuote(x[i])), " does not look valid in R & C")
  }
}
