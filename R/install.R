##' Update or install RcppR6 files.  This will copy required files
##' around, parse your \code{inst/classes.yml} file, and generate
##' required files.  Using \code{RcppR6::install()} is equivalent to
##' passing \code{install=TRUE} to \code{RcppR6::RcppR6}.
##'
##' More details coming later!
##' @title Update Or Install RcppR6 Files
##' @param path Path to the package (this directory must contain the
##' DESCRIPTION file)
##' @param verbose Logical indicating if information about the process
##' will be generated.  It's not all that verbose really.
##' @param install Logical indicating if this should be treated as a
##' fresh install.  Specifying \code{TRUE} (not the default) should
##' always be safe, but will copy default or skeleton copyies of files
##' into place if they do not exist, as well as update your
##' DESCRIPTION file.
##' @param attributes Should Rcpp attributes be regenerated as well?
##' This is probably a good idea (and is the default).
##' @param roxygen Should \code{devtools::document} be run to rebuild
##' the NAMESPACE and documentation files?  This is generally a good
##' idea, but will force a rebuild of the source code in the package.
##' That might not always be desirable as you can end up in unloadable
##' states.
##' @author Rich FitzJohn
##' @export
RcppR6 <- function(path=".", verbose=TRUE,
                   install=FALSE, attributes=TRUE, roxygen=FALSE) {
  package <- RcppR6_package$new(path, verbose)

  if (install) {
    RcppR6_install_files(package)
  }

  package$write_files()

  if (attributes) {
    RcppR6_run_attributes(package, verbose)
  }
  if (roxygen) {
    RcppR6_run_roxygen(package, verbose)
  }

  invisible(package)
}

##' @export
##' @rdname RcppR6
##' @param ... Arguments passed to \code{RcppR6()}
install <- function(...) {
  RcppR6(..., install=TRUE)
}

##' Wrapper around \code{devtools::create} that also creates RcppR6
##' files.
##' @title Create New Package, With RcppR6 Support
##' @param path Location of the package.  See
##' \code{\link[devtools]{create}} for more information.
##' @param ... Additional arguments passed to
##' \code{\link[devtools]{create}}.
##' @author Rich FitzJohn
##' @export
create <- function(path, ...) {
  devtools::create(path, ...)
  install(path)
}

## Below here -- no exported functions that do all the work.

RcppR6_install_files <- function(package, verbose=TRUE) {
  package$create_directories()
  info <- package$template_info()

  update_DESCRIPTION(package, verbose)
  install_file("Makevars", info$paths$src, verbose)
  if (!file.exists(info$files$package_include)) {
    template <-
      read_file(RcppR6_file("templates/package_include.h.whisker"))
    update_file(wr(template, list(package=info)),
                info$files$package_include, verbose)
    if (verbose) {
      message("\t...you'll need to edit this file a bunch")
    }
  }
  namespace <- file.path(package$path, "NAMESPACE")
  if (!file.exists(namespace)) {
    if (verbose) {
      message("Writing empty NAMESPACE")
    }
    writeLines(character(0), namespace)
  }
}

## NOTE: This duplicates some of the effort in check_DESCRIPTION
update_DESCRIPTION <- function(package, verbose=TRUE) {
  add_depends_if_missing <- function(package, field, data, verbose) {
    if (!depends(package, field, data)) {
      field <- field[[1]]
      if (verbose) {
        message(sprintf("DESCRIPTION: Adding dependency %s in field %s",
                        package, field))
      }
      if (field %in% names(data)) {
        data[[field]] <- paste(data[[field]], package, sep=", ")
      } else {
        data[[field]] <- package
      }
    }
    data
  }

  filename <- file.path(package$path, "DESCRIPTION")
  if (!file.exists(filename)) {
    stop("Did not find DESCRIPTION file to modify")
  }

  d <- d_orig <- read_dcf(filename)

  d <- add_depends_if_missing("Rcpp", "LinkingTo", d, verbose)
  d <- add_depends_if_missing("Rcpp", c("Imports", "Depends"), d, verbose)
  d <- add_depends_if_missing("R6",   c("Imports", "Depends"), d, verbose)

  if (isTRUE(all.equal(d, d_orig))) {
    if (verbose) {
      message("DESCRIPTION looks good: leaving alone")
    }
  } else {
    s <- paste(capture.output(write.dcf(d)), collapse="\n")
    update_file(s, filename, verbose)
  }
}

RcppR6_run_attributes <- function(package, verbose) {
  if (verbose) {
    message("Compiling Rcpp attributes")
  }
  Rcpp::compileAttributes(package$path)
}

RcppR6_run_roxygen <- function(package, verbose) {
  if (verbose) {
    message("Running devtools::document")
  }
  devtools::document(package$path)
}

## Seriously, don't use this.  This is for testing only.
uninstall <- function(path=".", verbose=TRUE, attributes=TRUE) {
  package <- RcppR6_package$new(path, FALSE)
  info <- package$template_info()

  p <- info$paths
  file_remove_if_exists(file.path(p$include_pkg, "RcppR6_pre.hpp"),
                        file.path(p$include_pkg, "RcppR6_post.hpp"),
                        file.path(p$include_pkg, "RcppR6_support.hpp"),
                        file.path(p$R,           "RcppR6.R"),
                        file.path(p$src,         "RcppR6.cpp"),
                        verbose=verbose)
  ## We leave alone the package include file, Makevars, DESCRIPTION
  if (attributes) {
    RcppR6_run_attributes(package, verbose)
  }
  dir_remove_if_empty(info$paths)
}


## Because of the devtools issue (hadley/devtools#531) we need to use
## a non-standard temporary file location for the tests.
prepare_temporary <- function(pkg, path="~/tmp") {
  if (!file.exists(path)) {
    dir.create(path)
  }
  pkg <- normalizePath(pkg)
  pkg_dest <- file.path(path, basename(pkg))
  if (file.exists(pkg_dest)) {
    unlink(pkg_dest, recursive=TRUE)
  }
  file.copy(pkg, path, recursive=TRUE)
  invisible(pkg_dest)
}
