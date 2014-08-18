##' @include RcppR6_cleanup.R
##' @include RcppR6_template_info.R
NULL

## This is a base class that needs to be listed first:
RcppR6_obj <-
  R6::R6Class("RcppR6_obj",
              public=list(
                initialize=function(defn, parent=NULL) {
                  self$defn_yaml <- defn
                  self$parent <- parent
                  ## So, this is going to be useful except it will
                  ## prevent options(error=recover) working.  Might be
                  ## worth making this configurable.  Without this
                  ## though, error messages are inscrutiable.
                  tryCatch(private$cleanup(), error=try_cleanup(self))
                },
                to_yaml=function(...) {
                  yaml::as.yaml(self$defn_yaml, ...)
                },
                get_templates=function() {
                  if (is.null(self$parent)) { # top
                    if (is.null(self$templates)) {
                      RcppR6_read_templates()
                    } else {
                      self$templates
                    }
                  } else {
                    self$parent$get_templates()
                  }
                },
                defn_yaml=NULL,
                defn=NULL,
                parent=NULL),
              private=list(cleanup=function(obj) {}))

RcppR6_package <-
  R6::R6Class("RcppR6_package",
              inherit=RcppR6_obj,
              public=list(
                initialize=function(path, verbose=TRUE) {
                  yaml <- read_RcppR6_yml(path, verbose, FALSE)
                  super$initialize(yaml, NULL)
                  self$path <- path
                  self$name <- package_name(self$path)
                  self$defn <- self$defn_yaml
                  self$classes <- RcppR6_class_list(self$defn$classes, self)
                  self$templates <- self$get_templates()
                },
                template_info=template_info_package,
                create_directories=function() {
                  for (p in self$template_info()$paths) {
                    dir.create(p, FALSE)
                  }
                },
                write_files=function(verbose=TRUE) {
                  processed <- list()
                  core_types <-
                    c("r", "cpp", "RcppR6_pre", "RcppR6_post", "support")
                  for (type in core_types) {
                    processed[[type]] <- self[[paste0("format_", type)]]()
                  }
                  self$create_directories()
                  files <- self$template_info()$files
                  for (type in core_types) {
                    update_file(processed[[type]], files[[type]], verbose)
                  }
                },
                format_r=function() {
                  str <- wr(self$get_templates()$RcppR6.R,
                            list(RcppR6=template_info_RcppR6(),
                                 package=self$template_info("r")))
                  if (any(collect("is_templated", self$classes))) {
                    str <- paste(str,
                                 wr(self$get_templates()$RcppR6_support.R),
                                 sep="\n\n")
                  }
                  str
                },
                format_cpp=function() {
                  wr(self$get_templates()$RcppR6.cpp,
                     list(RcppR6=template_info_RcppR6(),
                          package=self$template_info("cpp")))
                },
                format_RcppR6_pre=function() {
                  types <- c("forward_declaration",
                             "rcpp_prototypes")
                  wr(self$get_templates()$RcppR6_pre.hpp,
                     list(RcppR6=template_info_RcppR6(),
                          package=self$template_info(types)))
                },
                format_RcppR6_post=function() {
                  types <- c("traits",
                             "rcpp_definitions")
                  wr(self$get_templates()$RcppR6_post.hpp,
                     list(RcppR6=template_info_RcppR6(),
                          package=self$template_info(types)))
                },
                format_RcppR6_support=function() {
                  wr(self$get_templates()$RcppR6_support.hpp,
                     list(RcppR6=template_info_RcppR6(),
                          package=self$template_info()))
                },
                format_support=function() {
                  wr(self$get_templates()$RcppR6_support.hpp,
                     list(RcppR6=template_info_RcppR6(),
                          package=self$template_info()))
                },
                path=NULL,
                name=NULL,
                classes=NULL,
                templates=NULL),
              ## Still a bug in R6 here...
              private=list(dummy=NULL))

## Might make a second class for templated things?  Might be a good
## idea.  But hard to pull off because we don't know if the class is
## templated before we start reading it.
RcppR6_class <-
  R6::R6Class("RcppR6_class",
              inherit=RcppR6_obj,
              public=list(
                package=function() {
                  if (!inherits(self$parent, "RcppR6_package")) {
                    stop("Cannot get parent package")
                  }
                  self$parent
                },
                template_info=template_info_class,
                format_r=function() {
                  if (self$is_templated) {
                    paste(self$constructor$format_r(),
                          private$collect_templates("r"),
                          sep="\n\n")
                  } else {
                    wr(self$get_templates()$R6_generator,
                       list(RcppR6=template_info_RcppR6(),
                            constructor=self$constructor$template_info(),
                            class=self$template_info("r")))
                  }
                },
                format_cpp=function() {
                  if (self$is_templated) {
                    paste(private$collect_templates("cpp"),
                          sep="\n\n")
                  } else {
                    dat <- self$template_info("cpp")
                    paste(c(dat$constructor_cpp,
                            dat$methods_cpp,
                            dat$active_cpp), collapse="\n")
                  }
                },
                format_forward_declaration=function() {
                  if (self$is_templated) {
                    private$collect_templates("forward_declaration", "\n")
                  } else {
                    self$template_info()$forward_declaration
                  }
                },
                format_rcpp_prototypes=function() {
                  if (self$is_templated) {
                    private$collect_templates("rcpp_prototypes")
                  } else {
                    wr(self$get_templates()$rcpp_prototypes,
                       list(class=self$template_info()))
                  }
                },
                format_traits=function() {
                  if (self$is_templated) {
                    private$collect_templates("traits")
                  } else {
                    wr(self$get_templates()$RcppR6_traits,
                       ## TODO: fix this:
                       list(package=list(name=self$package()$name),
                            class=self$template_info()))
                  }
                },
                format_rcpp_definitions=function() {
                  if (self$is_templated) {
                    private$collect_templates("rcpp_definitions")
                  } else {
                    wr(self$get_templates()$rcpp_definitions,
                       list(class=self$template_info()))
                  }
                },
                name_r=NULL,
                name_safe=NULL,
                name_cpp=NULL,
                forward_declare=NULL,
                forward_declare_type=NULL,
                inherits=NULL,
                is_templated=FALSE,
                templates=NULL,
                constructor=NULL,
                methods=NULL,
                active=NULL),
              private=list(
                cleanup=cleanup_class,
                collect_templates=function(name, join="\n\n") {
                  res <- lapply(self$templates$concrete, function(x)
                                x$class[[paste0("format_", name)]]())
                  paste(unlist(res), collapse=join)
                }))

RcppR6_constructor <-
  R6::R6Class("RcppR6_constructor",
              inherit=RcppR6_obj,
              public=list(
                class=function() {
                  if (!inherits(self$parent, "RcppR6_class")) {
                    stop("Cannot get parent class")
                  }
                  self$parent
                },
                template_info=template_info_constructor,
                format_r=function() {
                  if (self$class()$is_templated) {
                    wr(self$get_templates()$R6_generator_generic,
                       list(constructor=self$template_info(),
                            class=self$class()$template_info()))
                  } else {
                    NULL
                  }
                },
                format_cpp=function() {
                  wr(self$get_templates()$constructor_cpp,
                     list(#RcppR6=template_info_RcppR6(),
                          constructor=self$template_info()))
                },
                name_cpp=NULL,
                roxygen=NULL,
                args=NULL),
              private=list(
                cleanup=cleanup_constructor))

RcppR6_method <-
  R6::R6Class("RcppR6_method",
              inherit=RcppR6_obj,
              public=list(
                class=function() {
                  if (!inherits(self$parent, "RcppR6_class")) {
                    stop("Cannot get parent class")
                  }
                  self$parent
                },
                template_info=template_info_method,
                format_r=function() {
                  wr(self$get_templates()$method_r,
                     list(#RcppR6=template_info_RcppR6(),
                          method=self$template_info()))
                },
                format_cpp=function() {
                  drop_blank(wr(self$get_templates()$method_cpp,
                                list(RcppR6=template_info_RcppR6(),
                                     method=self$template_info())))
                },
                name_r=NULL,
                name_safe=NULL,
                name_cpp=NULL,
                return_type=NULL,
                access=NULL,
                args=NULL),
              private=list(
                cleanup=cleanup_method))

RcppR6_active <-
  R6::R6Class("RcppR6_active",
              inherit=RcppR6_obj,
              public=list(
                class=function() {
                  if (!inherits(self$parent, "RcppR6_class")) {
                    stop("Cannot get parent class")
                  }
                  self$parent
                },
                template_info=template_info_active,
                format_r=function() {
                  drop_blank(wr(self$get_templates()$active_r,
                                list(RcppR6=template_info_RcppR6(),
                                     active=self$template_info())))
                },
                format_cpp=function() {
                  drop_blank(wr(self$get_templates()$active_cpp,
                                list(RcppR6=template_info_RcppR6(),
                                     active=self$template_info())))
                },
                name_r=NULL,
                name_safe=NULL,
                name_cpp=NULL,
                name_cpp_set=NULL,
                type=NULL,
                access=NULL,
                readonly=NULL),
              private=list(
                cleanup=cleanup_active))

RcppR6_templates <-
  R6::R6Class("RcppR6_templates",
              inherit=RcppR6_obj,
              public=list(
                parameters=NULL,
                concrete=NULL),
              private=list(cleanup=cleanup_templates))

RcppR6_concrete <-
  R6::R6Class("RcppR6_concrete",
              inherit=RcppR6_obj,
              public=list(
                name_r=NULL,
                name_cpp=NULL,
                name_safe=NULL,
                parameters_r=NULL,
                parameters_cpp=NULL,
                parameters_safe=NULL,
                class=NULL),
              private=list(cleanup=cleanup_concrete))

RcppR6_args <-
  R6::R6Class("RcppR6_args",
              inherit=RcppR6_obj,
              public=list(
                template_info=template_info_args,
                calling_function=function() {
                  if (!(inherits(self$parent, "RcppR6_method") ||
                        inherits(self$parent, "RcppR6_constructor"))) {
                    stop("Cannot get calling function")
                  }
                  self$parent
                },
                names=NULL,
                types=NULL),
              private=list(
                cleanup=cleanup_args))

## Some for iteration helpers:
RcppR6_class_list <- function(yaml, parent=NULL) {
  if (length(yaml) > 0) {
    assert_list(yaml)
    assert_named(yaml)
    lapply(seq_along(yaml),
           function(i) RcppR6_class$new(yaml[i], parent))
  }
}
RcppR6_method_list <- function(yaml, parent=NULL) {
  if (length(yaml) > 0) {
    assert_list(yaml)
    assert_named(yaml)
    lapply(seq_along(yaml),
           function(i) RcppR6_method$new(yaml[i], parent))
  }
}
RcppR6_active_list <- function(yaml, parent=NULL) {
  if (length(yaml) > 0) {
    assert_list(yaml)
    assert_named(yaml)
    lapply(seq_along(yaml),
           function(i) RcppR6_active$new(yaml[i], parent))
  }
}
RcppR6_concrete_list <- function(yaml, parent=NULL) {
  assert_list(yaml)
  lapply(seq_along(yaml),
         function(i) RcppR6_concrete$new(yaml[i], parent))
}

## Helpers for clearer debugging information:
try_cleanup <- function(obj) {
  function(e) {
    stop(sprintf("while processing %s:\n\t%s", name(obj), e$message),
         call.=FALSE)
  }
}

name <- function(x) {
  ret <- x$name_r
  if (is.null(ret)) {
    if (inherits(x, "RcppR6_concrete")) {
      ret <- sprintf("{ %s }",
                     strip_trailing_newline(yaml::as.yaml(x$defn)))
    } else {
      ## Should only be for constructor & args?
      ret <- sub("^RcppR6_", "", class(x)[[1]])
    }
  }
  if (is.null(x$parent)) {
    ret
  } else {
    paste(name(x$parent), ret, sep="::")
  }
}

## Destroy out-of-line members:
rm(cleanup_class)
rm(cleanup_constructor)
rm(cleanup_method)
rm(cleanup_active)
rm(cleanup_templates)
rm(cleanup_concrete)
rm(cleanup_args)

rm(template_info_package)
rm(template_info_class)
rm(template_info_constructor)
rm(template_info_method)
rm(template_info_active)
rm(template_info_args)
