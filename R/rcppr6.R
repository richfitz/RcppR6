read_classes <- function(path=".", package=package_name(path),
                         classfile="inst/rcppr6.yml") {
  classes <- yaml_read(file.path(path, classfile))
  dat <- lnapply(classes, parse_class, package=package)

  info <- list(package=package,
               PACKAGE=toupper(package),
               rcppr6_version=rcppr6_version())

  d <- info[c("package", "PACKAGE", "rcppr6_version")]
  collect <- function(x) {
    paste(sapply(dat, "[[", x), collapse="\n")
  }
  d$r         <- collect("r")
  d$cpp       <- collect("cpp")
  d$rcpp_pre  <- collect("rcpp_pre")
  d$rcpp_post <- collect("rcpp_post")
  ## This is the main bit that could fail.
  info$r           <- wr_file("rcppr6.R",        d)
  info$cpp         <- wr_file("rcppr6.cpp",      d)
  info$rcppr6_pre  <- wr_file("rcppr6_pre.hpp",  d)
  info$rcppr6_post <- wr_file("rcppr6_post.hpp", d)

  ## TODO: This could all do with some streamlining, but the idea is
  ## sound.  We're also not going to update anything unless all
  ## template substitutions passed, which is nice.
  p <- paths(path, package)
  info$update <- function(verbose) {
    update_file(info$r,   file.path(p$R,   "rcppr6.R"),   verbose)
    update_file(info$cpp, file.path(p$src, "rcppr6.cpp"), verbose)
    update_file(info$rcppr6_pre,
                file.path(p$include_pkg, "rcppr6_pre.hpp"), verbose)
    update_file(info$rcppr6_post,
                file.path(p$include_pkg, "rcppr6_post.hpp"), verbose)
  }

  info$classes <- dat

  info
}

parse_class <- function(name, defn, package) {
  info <- list(classname=name,
               package=package,
               namespace=defn$namespace)

  ## Next, define a bunch of useful types:
  info$classname_full <- sprintf("%s::%s", info$namespace, info$classname)
  info$ptr_type       <- sprintf('Rcpp::XPtr< %s >', info$classname_full)
  info$ptr_name       <- "ptr_"
  info$input_type     <- "Rcpp::RObject" # or SEXP?
  info$input_name     <- "obj_"
  info$r_self_name    <- "self"

  ## Here are the standard template parameters:
  info$template_info <- function() {
    info[c("package", "PACKAGE", "namespace",
           "classname", "classname_full", "generator",
           "ptr_type", "ptr_name", "input_type", "input_name",
           "r_self_name")]
  }

  ## At this point we have everything necessary to start building
  ## constructors and methods.
  info$constructor <- parse_constructor(defn$constructor, info)
  info$methods <- lnapply(defn$methods, parse_method, class=info)
  d <- info$template_info()

  ## Rcpp stubs are easy to generate:
  info$rcpp_wrap_prototype  <- wr_file("rcpp_wrap_prototype",  d)
  info$rcpp_wrap_definition <- wr_file("rcpp_wrap_definition", d)
  info$rcpp_as_prototype    <- wr_file("rcpp_as_prototype",    d)
  info$rcpp_as_definition   <- wr_file("rcpp_as_definition",   d)

  ## All the R and C++ versions of the methods:
  info$methods_r <- indent(paste(sapply(info$methods, "[[", "r"),
                                 collapse=",\n"), 16)
  info$methods_cpp <- paste(sapply(info$methods, "[[", "cpp"),
                          collapse="\n")
  info$r6_generator <- wr_file("r6_generator", c(d, info["methods_r"]))

  info$cpp <- paste(info$constructor$cpp, info$methods_cpp,  sep="\n")
  info$r   <- paste(info$constructor$r,   info$r6_generator, sep="\n")
  info$rcpp_pre <- paste(info$rcpp_wrap_prototype,
                         info$rcpp_as_prototype, sep="\n")
  info$rcpp_post <- paste(info$rcpp_wrap_definition,
                          info$rcpp_as_definition, sep="\n")

  info
}

parse_constructor <- function(defn, class) {
  info <- list()
  info$class <- class
  info$roxygen <- roxygen_prepare(defn$roxygen)
  info$args <- parse_args(defn$args, info)

  info$template_info <- function() {
    c(info$class$template_info(),
      info$args$template_info(),
      info["roxygen"])
  }

  d <- info$template_info()
  info$cpp <- wr_file("constructor_cpp", d)
  info$r   <- wr_file("constructor_r",   d)
  info
}

parse_method <- function(name, defn, class) {
  ## TODO: Check here for existance of return_type.
  info <- list()
  info$class       <- class
  info$method      <- name
  info$method_cpp  <- with_default(defn$method_cpp, name)
  info$member      <- with_default(defn$member,     TRUE)
  info$args        <- parse_args(defn$args, info)
  info$return_type <- defn$return_type
  info$return_statement <-
    if (info$return_type == "void") "" else "return "
  info$template_info <- function() {
    c(info$class$template_info(),
      info$args$template_info(),
      info[c("method", "method_cpp", "return_type", "return_statement")])
  }

  partials <- list(method_call=get_template(paste0("method_cpp_",
                     if (info$member) "member" else "free")))
  d <- info$template_info()
  info$cpp <- wr_file("method_cpp", d, partials=partials)
  info$r   <- wr_file("method_r",   d)
  info
}

parse_args <- function(args, method) {
  info <- list()
  ## TODO: Check that args is a list of single-element named lists.
  ## Might need to be a bit careful here to make sure that R's
  ## named-things-everywhere and Python/JSON/YAML's stricter approach
  ## don't get is confused.
  info$args_name <- vapply(args, function(x) names(x), character(1))
  info$args_type <- vapply(args, function(x) x[[1]], character(1))
  info$constructor <- is.null(method$member)
  info$class <- method$class

  cpp_defn_type <-
    c(if (!info$constructor) info$class$input_type, info$args_type)
  cpp_defn_name <-
    c(if (!info$constructor) info$class$input_name, info$args_name)
  cpp_use_name <-
    c(if (!(info$constructor || method$member)) info$class$ptr_name,
      info$args_name)
  r_use_name <-
    c(if (!info$constructor) info$class$r_self_name, info$args_name)

  info$cpp_defn <- paste(cpp_defn_type, cpp_defn_name, collapse=", ")
  info$cpp_use  <- paste(cpp_use_name, collapse=", ")
  info$r_defn   <- paste(info$args_name, collapse=", ")
  info$r_use    <- paste(r_use_name, collapse=", ")

  info$template_info <- function() {
    ret <- info[c("cpp_defn", "cpp_use", "r_defn", "r_use")]
    names(ret) <- paste0("args_", names(ret))
    ret
  }

  info
}

## Need to add strip here.
roxygen_prepare <- function(str) {
  if (length(str) > 0) {
    paste(paste0("##' ", strsplit(str, "\n", fixed=TRUE)[[1]]),
          collapse="\n")
  } else {
    ""
  }
}
