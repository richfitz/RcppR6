## Package (relatively global) level information:
template_info_rcppr6 <- function() {
  list(input_type="Rcpp::RObject",
       input_name="obj_",
       ptr_name="ptr_",
       type_name="type",
       ## These should be constant, but would vary if using RC backend
       r_self_name="self",
       r_value_name="value",
       r6_generator_prefix=mangle_r6_generator(""),
       version=rcppr6_version())
}

template_info_package <- function(package, path=".") {
  paths <-
    list(root        = path,
         inst        = file.path(path, "inst"),
         include     = file.path(path, "inst/include"),
         include_pkg = file.path(path, "inst/include", package),
         yml         = file.path(path, "inst"),
         r           = file.path(path, "R"),
         src         = file.path(path, "src"))
  files <- list(
    yml             = file.path(paths$yml,         "rcppr6.yml"),
    r               = file.path(paths$r,           "rcppr6.R"),
    cpp             = file.path(paths$src,         "rcppr6.cpp"),
    rcppr6_pre      = file.path(paths$include_pkg, "rcppr6_pre.hpp"),
    rcppr6_post     = file.path(paths$include_pkg, "rcppr6_post.hpp"),
    support         = file.path(paths$include_pkg, "rcppr6_support.hpp"),
    package_include = file.path(paths$include, sprintf("%s.h", package)))
  list(name=package, NAME=toupper(package), paths=paths, files=files)
}

template_info_class_list <- function(x) {
  lapply(x, template_info_class)
}

template_info_class <- function(x) {
  assert_inherits(x, "rcppr6_class")
  ret <- x[c("name_r", "name_cpp")]
  ret$r6_generator <- mangle_r6_generator(ret$name_r)
  ## TODO: Only used in generic functions, and incorrectly named from
  ## a OOP POV.  Could be useful for objects that implement a common
  ## interface though.
  ##
  ## NOTE: The peculiar "NULL" as a string here is so that the
  ## template ends up with a 'proper' NULL after substitution (see
  ## r6_generator.whisker)
  ret$inherits <- if (is.null(x$inherits))
    "NULL" else mangle_r6_generator(x$inherits)
  ret$forward_declaration <- template_info_forward_declaration(x)
  ret$is_generic <- x$templates$is_templated
  if (ret$is_generic) {
    ret$is_generic <- TRUE
    ret$constructor <-
      template_info_constructor_generic(x$constructor, x$templates)
    ret$templates <-
      lapply(x$templates$concrete, function(t)
             template_info_class(cpp_template_rewrite_class(x, t)))
  } else {
    ret$constructor <- template_info_constructor(x$constructor)
    ret$methods     <- lapply(x$methods, template_info_method)
    ret$active      <- lapply(x$active,  template_info_active)
  }
  ret
}

template_info_constructor_generic <- function(x, templates) {
  assert_inherits(x, "rcppr6_constructor")
  ret <- list()

  ## TODO: consider mangling or use templates/constructor_name.whisker
  ret$list <- sprintf("%s__ctor",
                      sapply(templates$concrete, "[[", "name_r"))
  names(ret$list) <- names(templates$concrete)
  ret$list_r_repr <-
    sprintf("c(%s)",
            paste(sprintf('"%s"=%s', names(ret$list), ret$list),
                  collapse=", "))

  ret$roxygen <- template_info_roxygen(x$roxygen)
  if (!is.null(ret$roxygen)) {
    ## TODO: This won't work well if there are examples, I think.
    ## TODO: Ideally put after the last @param argument or right
    ## before @export.  Before @export has a lot of appeal because
    ## that will only modify exported documentation.
    ## ret$roxygen <- paste(ret$roxygen,
    ##                      "@param type generic type information")
  }

  ret$args <- template_info_args(x$args, TRUE, FALSE)
  ret
}

template_info_constructor <- function(x) {
  assert_inherits(x, "rcppr6_constructor")
  ret <- x["name_cpp"]
  ret$roxygen <- template_info_roxygen(x$roxygen)
  ret$args <- template_info_args(x$args, TRUE, FALSE)
  ret
}

template_info_method <- function(x) {
  assert_inherits(x, "rcppr6_method")
  ret <- x[c("name_r", "name_cpp", "return_type")]
  ret$return_statement <- if (x$return_type == "void") "" else "return "
  ret$is_member   <- x$access == "member"
  ret$is_function <- x$access == "function"
  ret$args <- template_info_args(x$args, FALSE, ret$is_member)
  ret
}

template_info_active <- function(x) {
  assert_inherits(x, "rcppr6_active")
  ret <- x[c("name_r", "name_cpp")]
  ret$name_cpp_get <- x$name_cpp_get
  ret$name_cpp_set <- x$name_cpp_set
  ret$return_type  <- x$type
  ret$is_field     <- x$access == "field"
  ret$is_member    <- x$access == "member"
  ret$is_function  <- x$access == "function"
  ret$is_readonly  <- x$readonly
  ret
}

template_info_args <- function(x, constructor, member) {
  assert_inherits(x, "rcppr6_args")
  rcppr6 <- template_info_rcppr6()
  defn_cpp_type <-
    c(if (!constructor) rcppr6$input_type, x$type)
  defn_cpp_name <-
    c(if (!constructor) rcppr6$input_name, x$name)

  ret <- list()
  ret$defn_cpp <- paste(defn_cpp_type, defn_cpp_name, collapse=", ")
  use_cpp_prefix <- if (!constructor && !member) paste0("*", rcppr6$ptr_name)
  ret$use_cpp  <-  collapse(c(use_cpp_prefix, x$name))
  ret$defn_r   <- collapse(x$name)
  ret$use_r    <- collapse(c(if (!constructor) rcppr6$r_self_name,
                             x$name))
  ret
}

template_info_roxygen <- function(str) {
  if (length(str) > 0) {
    paste(paste0("##' ", strsplit(str, "\n", fixed=TRUE)[[1]]),
          collapse="\n")
  } else {
    ""
  }
}

template_info_forward_declaration <- function(x) {
  if (x$forward_declare == "") {
    character(0)
  } else {
    ns <- strsplit(x$namespace, "::", fixed=TRUE)[[1]]
    ## NOTE: This is the same regexp as sanitise_class
    name_cpp <- sub(sprintf("^(::)?%s::", x$namespace), "", x$name_cpp)
    paste0(paste(sprintf("namespace %s { ", ns), collapse=""),
           sprintf("%s %s;", x$forward_declare, name_cpp),
           paste(rep(" }", length(ns)), collapse=""))
  }
}

## Read all templates.  This makes things slightly simpler later.
rcppr6_templates <- function() {
  path <- rcppr6_file("templates")
  files <- dir(path, pattern=glob2rx("*.whisker"))
  dat <- lapply(file.path(path, files), read_file)
  names(dat) <- sub("\\.whisker$", "", files)
  dat
}
