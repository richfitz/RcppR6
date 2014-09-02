template_info_package <- function(type=NULL) {
  path <- self$path
  name <- self$name
  paths <-
    list(root        = path,
         inst        = file.path(path, "inst"),
         include     = file.path(path, "inst/include"),
         include_pkg = file.path(path, "inst/include", name),
         r           = file.path(path, "R"),
         src         = file.path(path, "src"))
  files <- list(
    r               = file.path(paths$r,           "RcppR6.R"),
    cpp             = file.path(paths$src,         "RcppR6.cpp"),
    RcppR6_pre      = file.path(paths$include_pkg, "RcppR6_pre.hpp"),
    RcppR6_post     = file.path(paths$include_pkg, "RcppR6_post.hpp"),
    support         = file.path(paths$include_pkg, "RcppR6_support.hpp"),
    package_include = file.path(paths$include, sprintf("%s.h", name)))

  ret <- list()
  ret$name    <- name
  ret$NAME    <- toupper(name)
  ret$paths   <- paths
  ret$files   <- files

  format <- function(x, type) {
    res <- lapply(x, function(x) x[[paste0("format_", type)]]())
    paste(unlist(res), # drops character(0) / NULL entries
          collapse=if (type == "forward_declaration") "\n" else "\n\n")
  }
  for (t in type) {
    ret[[t]] <- format(self$classes, t)
  }

  ret
}

template_info_class <- function(type=NULL) {
  ret <- list()
  ret$name_r       <- self$name_r
  ret$name_cpp     <- self$name_cpp
  ret$name_safe    <- self$name_safe
  ret$input_type   <- mangle_input(self$package()$name, self$name_cpp)
  ret$R6_generator <- mangle_R6_generator(self$name_safe)
  ret$inherits <- if (is.null(self$inherits))
    "NULL" else mangle_R6_generator(self$inherits)
  ret$forward_declaration <- template_info_forward_declaration(self)
  ret$is_generic   <- self$is_templated ## TODO: terminology change
  if (ret$is_generic) {
    if (length(type) > 0) {
      stop("Really?")
    }
  } else {
    format <- function(x, type, pre=character(0)) {
      if (length(x) == 0) {
        NULL
      } else {
        str <- paste(sapply(x, function(x) x[[paste0("format_", type)]]()),
                     collapse=if (type == "r") ",\n" else "\n")
        if (type == "r") {
          str <- indent(str, 16)
        }
        paste0(pre, str)
      }
    }
    if ("r" %in% type) {
      ret$constructor_r <- self$constructor$format_r()
      ret$methods_r <- format(self$methods, "r", ",\n")
      ret$active_r  <- format(self$active,  "r",  "\n")
    }
    if ("cpp" %in% type) {
      ret$constructor_cpp <- self$constructor$format_cpp()
      ret$methods_cpp <- format(self$methods, "cpp")
      ret$active_cpp  <- format(self$active,  "cpp")
    }
  }
  ret
}

template_info_constructor <- function() {
  ret <- list()
  if (self$class()$is_templated) {
    templates <- self$parent$templates
    ret$types <- collapse(templates$parameters)

    ## Valid template types:
    valid <- sapply(templates$concrete, function(x)
                    dput_to_character(unname(x$parameters_r)))
    names(valid) <- collect("name_r", templates$concrete)
    ret$valid_r_repr <-
      sprintf("list(%s)", collapse(sprintf('"%s"=%s', names(valid), valid)))

    ## Don't use the strings here: we want the actual functions:
    ret$constructors_r_repr <-
      sprintf("list(%s)", collapse(sprintf('"%s"=`%s`',
                                           names(valid), names(valid))))
  } else {
    ret$name_cpp    <- self$name_cpp
    ret$name_safe   <- mangle_constructor(self$class()$name_safe)
    ret$return_type <- self$class()$name_cpp
  }

  ret$roxygen <- template_info_roxygen(self$roxygen)
  ret$args <- self$args$template_info()

  ret
}

template_info_method <- function() {
  ret <- list()
  class <- self$class()

  ret$name_r <- self$name_r
  ret$name_cpp <- self$name_cpp
  ret$name_safe <- mangle_method(class$name_safe, self$name_safe)

  ret$return_type <- self$return_type
  ret$return_statement <- if (self$return_type == "void") "" else "return "
  ret$is_member   <- self$access == "member"
  ret$is_function <- self$access == "function"

  ret$args <- self$args$template_info()
  ret
}

template_info_active <- function() {
  class <- self$class()
  ret <- list()
  ret$name_r       <- self$name_r
  ret$is_readonly  <- self$readonly

  ret$name_safe_get <- mangle_active(class$name_safe, self$name_safe, "get")
  if (self$access == "field") {
    ret$name_cpp <- self$name_cpp
  } else {
    ret$name_cpp_get  <- self$name_cpp
  }
  if (!self$readonly) {
    ret$name_safe_set <- mangle_active(class$name_safe, self$name_safe, "set")
    ret$name_cpp_set  <- self$name_cpp_set
  }
  ## TODO: duplicated with class
  ret$input_type   <- mangle_input(class$package()$name, class$name_cpp)
  ## TODO: duplicated with class
  ret$class_name_r <- class$name_r
  ret$return_type  <- self$type
  ret$is_field     <- self$access == "field"
  ret$is_member    <- self$access == "member"
  ret$is_function  <- self$access == "function"
  ret
}

template_info_args <- function() {
  RcppR6 <- template_info_RcppR6()
  calling_function <- self$calling_function()
  is_constructor <- inherits(calling_function, "RcppR6_constructor")
  is_member <- (inherits(calling_function, "RcppR6_method") &&
                calling_function$access == "member")

  ret <- list()
  if (is.null(self$defaults)) {
    ret$defn_r <- collapse(self$names)
  } else {
    defn_r <- self$names
    i <- !is.na(self$defaults)
    defn_r[i] <- sprintf("%s=%s", self$names[i], self$defaults[i])
    ret$defn_r <- collapse(defn_r)
  }
  ret$use_r  <- collapse(c(if (!is_constructor) RcppR6$r_self_name,
                           self$names))

  ## C++ details are harder:
  if (is_constructor) {
    types_cpp <- self$types
    names_cpp <- self$names
    use_cpp_prefix <- NULL
  } else {
    input_cpp <- mangle_input(calling_function$class()$package()$name,
                              calling_function$class()$name_cpp)
    types_cpp <- c(input_cpp,         self$types)
    names_cpp <- c(RcppR6$input_name, self$names)
    use_cpp_prefix <- if (!is_member) paste0("*", RcppR6$input_name)
  }
  ret$defn_cpp <- paste(types_cpp, names_cpp, collapse=", ")
  ret$use_cpp  <- collapse(c(use_cpp_prefix, self$names))

  ret
}

## Standalone functions, used only in this file

## Package (relatively global) level information:
template_info_RcppR6 <- function() {
  list(input_name="obj_",
       type_name="type",
       ## These should be constant, but would vary if using RC backend
       r_self_name="self",
       r_value_name="value",
       R6_ptr_name=".ptr",
       R6_generator_prefix=mangle_R6_generator(""),
       version=RcppR6_version())
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
  if (x$forward_declare) {
    info <- guess_namespace(x$name_cpp)
    ns <- strsplit(info$namespace, "::", fixed=TRUE)[[1]]
    paste0(paste(sprintf("namespace %s { ", ns), collapse=""),
           sprintf("%s %s;", x$forward_declare_type, info$name),
           paste(rep(" }", length(ns)), collapse=""))
  } else {
    character(0)
  }
}

guess_namespace <- function(name) {
  re <- '^(::)?([[:alnum:]_:]+)::(.+)$'
  if (grepl(re, name)) {
    ns <- sub(re, "\\2", name)
    cl <- sub(re, "\\3", name)
  } else {
    ns <- ""
    cl <- name
  }
  list(namespace=ns, name=cl)
}
