load_rcppr6_yml <- function(path=".", verbose=TRUE) {
  filename <- file.path(path, "inst/rcppr6.yml")
  if (file.exists(filename)) {
    dat <- yaml_read(filename)
  } else {
    if (verbose) {
      message("No configuration found - using default")
    }
    dat <- config_default()
  }
  info <- sanitise_rcppr6(dat)
  filename_classes <- file.path(path, info$classes)
  classes <- join_lists(lapply(filename_classes, load_rcppr6_classes, verbose))
  list(classes=classes)
}

config_default <- function() {
  list(classes="inst/rcppr6_classes.yml")
}

load_rcppr6_classes <- function(filename, verbose) {
  if (verbose) {
    message("Reading classes from ", filename)
  }
  sanitise_class_list(yaml_read(filename))
}

sanitise_rcppr6 <- function(dat) {
  warn_unknown("rcppr6", dat, "classes")
  assert_character(dat$classes)
  if (length(dat$classes) == 0) {
    stop("Need at least one set of classes")
  }
  dat
}

## In the absence of a proper schema, this does a first round of
## cleaning.  Keys and contents are checked, default values are added,
## etc.  If this loads, then all the template information should flow
## fairly easily.

sanitise_class_list <- function(yaml) {
  if (is.null(yaml) || length(yaml) == 0) {
    list()
  } else {
    assert_list(yaml)
    assert_named(yaml)
    lnapply(yaml, sanitise_class)
  }
}

sanitise_class <- function(name, defn) {
  assert_scalar_character(name)
  warn_unknown(paste("class", name), defn,
               c("name_cpp", "forward_declare", "namespace",
                 "constructor", "methods", "active",
                 "templates", "inherits"))

  ret <- list()
  ## NOTE: this pattern is used in a few places: however, if
  ## sanitise_name() changes name, then we never want to draw the
  ## default for name_cpp -- this will certainly be a syntax error.
  ## It's possible though that for some templated class names this
  ## won't be the case, though it could lead to odd names.
  ret$name_r    <- name
  ret$name_safe <- sanitise_name(name)
  ret$name_cpp  <- with_default(defn$name_cpp, name)
  assert_scalar_character(ret$name_cpp)

  ret$templates <- sanitise_templates(defn$templates, ret, ret$name_safe)
  ## This is unfortunate, but if the class is templated, we might want
  ## to update the name of the C++ class:
  if (ret$templates$is_templated) {
    ret$name_cpp <- ret$templates$name_cpp
  }

  forward_declare_default <-
    if (ret$templates$is_templated) FALSE else "class"
  ret$forward_declare   <- with_default(defn$forward_declare,
                                        forward_declare_default)
  if (identical(ret$forward_declare, FALSE)) {
    ret$forward_declare <- ""
  }
  ret$forward_declare <- match_value(ret$forward_declare,
                                     c("class", "struct", ""))

  ret$namespace   <- with_default(defn$namespace,
                                  guess_namespace(ret$name_cpp))
  assert_scalar_character(ret$namespace)
  if (ret$namespace != "") {
    ret$namespace <- sub("^::", "", ret$namespace)
    if (!grepl(sprintf("^(::)?%s::", ret$namespace), defn$name_cpp)) {
      stop("Provided namespace does not look valid")
    }
  }

  ## Descend into complicated daughter elements:
  ret$constructor <- sanitise_constructor(defn$constructor,
                                          ret$name_cpp, name)
  ret$methods     <- sanitise_methods_list(defn$methods, name)
  ret$active      <- sanitise_active_list(defn$active,   name)

  class(ret) <- "rcppr6_class"

  ret
}

sanitise_templates <- function(defn, class, parent) {
  if (is.null(defn)) {
    list(is_templated=FALSE)
  } else {
    warn_unknown(sprintf("%s::templates", parent), defn,
                 c("parameters", "concrete"))
    ret <- list(is_templated=TRUE)

    ## For now, just allow single parameters!  Later allow multiple
    ## parameters, but we never allow single parameters?  Or perhaps if
    ## a single parameter is given we'll bail out with
    ## is_templated=FALSE.  Decisions can wait.
    ret$parameters <- defn$parameters
    ## This will miss when parameters are given as
    ##   parameters: T1, T2
    ## rather than
    ##   parameters: [T1, T2]
    if (length(ret$parameters) == 1 && grepl(",", ret$parameters)) {
      stop("Parameters need to be given as a yaml list")
      ## Alternatively, be polite and do this outselves:
      ## yaml_load(sprintf("[%s]", ret$parameters))
    }
    assert_character(ret$parameters)
    assert_nonempty(ret$parameters)

    ## Attempt to detect if the template has been specified properly.
    ## We need this to match or the later substitutions will likely
    ## fail.
    re <- sprintf("[[:space:]]*<%s>",
                  paste(sprintf("[[:space:]]*%s[[:space:]]*",
                                ret$parameters), collapse=","))
    if (!grepl(re, class$name_cpp)) {
      if (grepl("<", class$name_cpp)) {
        ## This could be triggered by giving conflicting names for the
        ## template parameters (T, U vs T1, T2 for instance).  Because
        ## it would be ideal to name things only once, I'm in favour
        ## of not listing template parameters in the yml name at all.
        stop("I'm confused about your templated class: add the parameters")
      }
      class$name_cpp <- sprintf("%s<%s>", class$name_cpp,
                                cpp_template_parameters(ret$parameters))
    }
    ret$name_cpp <- class$name_cpp

    ## Then build a list of concrete versions of the templates.
    assert_nonempty(defn$concrete)
    concrete <- lapply(defn$concrete, yaml_seq_map, name=FALSE)
    concrete <- lapply(concrete, unlist)

    f <- function(x, parameters) {
      ok <- (is.character(x) &&
             length(x) == length(parameters) &&
             !is.null(names(x)))
      if (!ok) {
        stop(sprintf("All versions must be %d element named character vectors",
                     length(parameters)))
      }
      parameters_r    <- names(x)
      parameters_cpp  <- unname(x)
      parameters_safe <- sanitise_name(parameters_r)
      ret <- list()
      ret$name_r         <- mangle_template_type_r(parent, parameters_r)
      ret$name_safe      <- mangle_template_type(parent, parameters_safe)
      ret$parameters_r   <- structure(parameters_r, names=parameters)
      ret$parameters_cpp <- structure(parameters_cpp, names=parameters)
      ret$name_cpp       <- cpp_template_rewrite_types(class$name_cpp, ret)
      ret
    }

    ## TODO: check no duplicate entries.

    ret$concrete <- lapply(concrete, f, ret$parameters)
    ret
  }
}

sanitise_constructor <- function(defn, class_cpp, parent) {
  warn_unknown(sprintf("%s::constructor", parent), defn,
               c("roxygen", "name_cpp", "args"))
  ret <- list()

  ## The name_cpp may be provided -- if so, rather than using an
  ## actual construtor we'll use some free function that returns an
  ## object of the right type.  Otherwise we'll use a constructor for
  ## the class.
  ret$name_cpp <- with_default(defn$name_cpp, class_cpp)
  assert_scalar_character(ret$name_cpp)

  ret$args <- sanitise_args(defn$args, parent)

  ## Roxygen information information may be provided, though this is
  ## optional.  It's useful for organising for the object to be
  ## exported and for documentation of course.  At some point we might
  ## want to use includes here, but roxygen offers those itself
  ## already so something like
  ##   roxygen: "@template template-file"
  ## might just work fine, with template-file being a file in
  ## man-roxygen/
  if (!is.null(defn$roxygen)) {
    ret$roxygen <- defn$roxygen
    assert_scalar_character(ret$roxygen)
  }

  class(ret) <- "rcppr6_constructor"

  ret
}

## The next two just orchestrate looping over the lists of
## methods/active bindings.
sanitise_methods_list <- function(methods, parent) {
  if (is.null(methods) || length(methods) == 0) {
    list()
  } else {
    assert_list(methods)
    assert_named(methods)
    lnapply(methods, sanitise_method, sprintf("%s::method::", parent))
  }
}
sanitise_active_list <- function(active, parent) {
  if (is.null(active) || length(active) == 0) {
    list()
  } else {
    assert_list(active)
    assert_named(active)
    lnapply(active, sanitise_active, sprintf("%s::active::", parent))
  }
}

sanitise_method <- function(name, defn, parent) {
  assert_scalar_character(name)
  warn_unknown(sprintf("::%s", parent), defn,
               c("name_cpp", "return_type", "access", "args"))
  ret <- list()
  ret$name_r    <- name
  ret$name_safe <- sanitise_name(name)
  ret$name_cpp  <- with_default(defn$name_cpp, name)
  assert_scalar_character(ret$name_cpp)

  ret$return_type <- defn$return_type
  assert_scalar_character(ret$return_type)

  ret$access <- match_value(with_default(defn$access, "member"),
                            c("member", "function"))

  ret$args <- sanitise_args(defn$args, parent)

  class(ret) <- "rcppr6_method"
  ret
}

sanitise_active <- function(name, defn, parent) {
  assert_scalar_character(name)
  warn_unknown(sprintf("::%s", parent), defn,
               c("name_cpp", "type", "access", "readonly"))

  ret <- list()
  ret$name_r    <- name
  ret$name_safe <- sanitise_name(name)
  ret$name_cpp  <- with_default(defn$name_cpp, name)
  assert_character(ret$name_cpp)

  ret$type <- defn$type
  assert_scalar_character(ret$type)

  ret$access <- match_value(defn$access,
                            c("field", "member", "function"))

  if (ret$access == "field") {
    assert_scalar(ret$name_cpp)
    ret$readonly <- with_default(defn$readonly, FALSE)
    assert_scalar_logical(ret$readonly)
  } else {
    n <- length(ret$name_cpp)
    if (n < 1 || n > 2) {
      stop("defn$name_cpp must be length 1 or 2")
    }
    if (!is.null(defn$readonly)) {
      stop("defn cannot contain readonly unless access = field")
    }
    ret$readonly <- n == 1
    ret$name_cpp_get <- ret$name_cpp[[1]]
    if (!ret$readonly) {
      ret$name_cpp_set <- ret$name_cpp[[2]]
    }
  }
  class(ret) <- "rcppr6_active"
  ret
}

sanitise_args <- function(args, parent) {
  if (is.null(args) || length(args) == 0) {
    ret <- list(name=character(0), type=character(0))
  } else {
    assert_list(args)
    ## Each element of args these must be a list one element, named.
    ok <- function(x) {
      is.list(x) && length(x) == 1 &&
        !is.null(names(x)) && is.character(x[[1]])
    }
    if (!all(sapply(args, ok))) {
      stop(parent, " args must be a list of named single-element lists")
    }
    ## Alternatively, unlist(args) does the same thing...
    ret <- list(name=vapply(args, function(x) names(x), character(1)),
                type=vapply(args, function(x) x[[1]], character(1)))
  }
  class(ret) <- "rcppr6_args"
  ret
}

## Really simple name sanitisation: convert dots to underscores :)
sanitise_name <- function(x) {
  x <- gsub(".", "_", x, fixed=TRUE)
  check_name(x)
  x
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

has_roxygen <- function(info) {
  assert_inherits(info, "rcppr6_class")
  any(sapply(info, function(x) !is.null(x$constructor$roxygen)))
}

guess_namespace <- function(name) {
  re <- '^(::)?([[:alnum:]_:]+)::(.+)$'
  if (grepl(re, name)) {
    sub(re, "\\2", name)
  } else {
    ""
  }
}
