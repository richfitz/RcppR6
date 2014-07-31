## In the absence of a proper schema, this does a first round of
## cleaning.  Keys and contents are checked, default values are added,
## etc.  If this loads, then all the template information should flow
## fairly easily.
load_rcppr6_yml <- function(filename) {
  sanitise_class_list(yaml_read(filename))
}

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
                 "constructor", "methods", "active"))
  ret <- list()
  ret$name        <- name
  ret$name_r      <- name
  ret$name_cpp    <- with_default(defn$name_cpp, name)
  assert_scalar_character(ret$name_cpp)

  ret$forward_declare   <- with_default(defn$forward_declare, "class")
  if (identical(ret$forward_declare, FALSE)) {
    ret$forward_declare <- ""
  }
  ret$forward_declare <- match_value(ret$forward_declare,
                                     c("class", "struct", ""))

  ret$namespace   <- with_default(defn$namespace,
                                  guess_namespace(defn$name_cpp))
  assert_scalar_character(ret$namespace)
  if (ret$namespace != "") {
    ret$namespace <- sub("^::", "", ret$namespace)
    if (!grepl(sprintf("^(::)?%s::", ret$namespace), defn$name_cpp)) {
      stop("Provided namespace does not look valid")
    }
  }

  ## Descend into complicated daughter elements:
  ret$constructor <- sanitise_constructor(defn$constructor,
                                          defn$name_cpp, name)
  ret$methods     <- sanitise_methods_list(defn$methods, name)
  ret$active      <- sanitise_active_list(defn$active,   name)

  class(ret) <- "rcppr6_class"

  ret
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
               c("name_cpp", "return_type", "member", "args"))
  ret <- list()
  ret$name     <- name
  ret$name_r   <- name
  ret$name_cpp <- with_default(defn$name_cpp, name)
  assert_scalar_character(ret$name_cpp)

  ret$return_type <- defn$return_type
  assert_scalar_character(ret$return_type)

  ret$member <- with_default(defn$member, TRUE)
  assert_scalar_logical(ret$member)

  ret$args <- sanitise_args(defn$args, parent)

  class(ret) <- "rcppr6_method"
  ret
}

sanitise_active <- function(name, defn, parent) {
  assert_scalar_character(name)
  warn_unknown(sprintf("::%s", parent), defn,
               c("name_cpp", "type", "access", "readonly"))

  ret <- list()
  ret$name     <- name
  ret$name_r   <- name
  ret$name_cpp <- with_default(defn$name_cpp, name)
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
