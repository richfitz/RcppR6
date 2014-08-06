##' Update or install rcppr6 files.  This will copy required files
##' around, parse your \code{inst/classes.yml} file, and generate
##' required files.  Using \code{rcppr6::install()} is equivalent to
##' passing \code{install=TRUE} to \code{rcppr6::rcppr6}.
##'
##' More details coming later!
##' @title Update Or Install rcppr6 Files
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
rcppr6 <- function(path=".", verbose=TRUE,
                   install=FALSE, attributes=TRUE, roxygen=FALSE) {
  package <- template_info_package(package_name(path), path)

  if (install) {
    rcppr6_install_files(package)
  } else {
    rcppr6_create_directories(package)
  }

  ## This bit actually does the hard work:
  ## Load the data:
  classes <- load_rcppr6_yml(path, verbose)$classes
  ## Build template lists:
  template_info <- template_info_class_list(classes)
  ## Process the templates into final strings:
  processed <- format_class_list(template_info, package)
  ## Write these out in the correct file:
  for (type in c("r", "cpp", "rcppr6_pre", "rcppr6_post", "support")) {
    update_file(processed[[type]], package$files[[type]], verbose)
  }

  ## Update generated code that depends on our generated code:
  if (attributes) {
    rcppr6_run_attributes(package, verbose)
  }
  if (roxygen && has_roxygen(classes)) {
    rcppr6_run_roxygen(package, verbose)
  }
  invisible(NULL)
}

##' @export
##' @rdname rcppr6
##' @param ... Arguments passed to \code{rcppr6()}
install <- function(...) {
  rcppr6(..., install=TRUE)
}

##' Wrapper around \code{devtools::create} that also creates rcppr6
##' files.
##' @title Create New Package, With rcppr6 Support
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

rcppr6_create_directories <- function(info) {
  for (pi in info$paths) {
    dir.create(pi, FALSE)
  }
}

rcppr6_install_files <- function(info, verbose=TRUE) {
  rcppr6_create_directories(info)
  update_DESCRIPTION(info, verbose)
  install_file("Makevars", info$paths$src, verbose)
  if (!file.exists(info$files$package_include)) {
    template <-
      read_file(rcppr6_file("templates/package_include.h.whisker"))
    update_file(wr(template, list(package=info)),
                info$files$package_include, verbose)
    if (verbose) {
      message("\t...you'll need to edit this file a bunch")
    }
  }
  namespace <- file.path(info$paths$root, "NAMESPACE")
  if (!file.exists(namespace)) {
    if (verbose) {
      message("Writing empty NAMESPACE")
    }
    writeLines(character(0), namespace)
  }
}

## NOTE: This duplicates some of the effort in check_DESCRIPTION
update_DESCRIPTION <- function(info, verbose=TRUE) {
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

  filename <- file.path(info$paths$root, "DESCRIPTION")
  if (!file.exists(filename)) {
    stop("Did not find DESCRIPTION file to modify")
  }

  d <- data.frame(read.dcf(filename), stringsAsFactors=FALSE)
  d_orig <- d
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

rcppr6_run_attributes <- function(package_info, verbose) {
  if (verbose) {
    message("Compiling Rcpp attributes")
  }
  Rcpp::compileAttributes(package_info$paths$root)
}

rcppr6_run_roxygen <- function(package_info, verbose) {
  if (verbose) {
    message("Running devtools::document")
  }
  devtools::document(package_info$paths$root)
}

## Seriously, don't use this.  This is for testing only.
uninstall <- function(path=".", verbose=TRUE, attributes=TRUE) {
  info <- template_info_package(package_name(path), path)
  p <- info$paths
  file_remove_if_exists(file.path(p$include_pkg, "rcppr6_pre.hpp"),
                        file.path(p$include_pkg, "rcppr6_post.hpp"),
                        file.path(p$include_pkg, "rcppr6_support.hpp"),
                        file.path(p$R,           "rcppr6.R"),
                        file.path(p$src,         "rcppr6.cpp"),
                        verbose=verbose)
  ## We leave alone the package include file, Makevars, DESCRIPTION
  if (attributes) {
    rcppr6_run_attributes(info, verbose)
  }
  dir_remove_if_empty(info$paths)
}
