##' Update or install rcppr6 files.  This will copy required files
##' around, parse your \code{inst/classes.yml} file, and generate
##' required files.  Using \code{rcppr6::install()} is equivalent to
##' passing \code{install=TRUE} to \code{rcppr6::update_rcppr6}.
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
##' @author Rich FitzJohn
##' @export
update_rcppr6 <- function(path=".", verbose=TRUE, install=FALSE) {
  package <- package_name(path)
  info <- list(package=package,
               PACKAGE=toupper(package),
               version=rcppr6_version())

  if (install) {
    p <- paths(path, package)
    for (pi in p) {
      dir.create(pi, FALSE)
    }
    update_DESCRIPTION(path, verbose)

    if (!file.exists(file.path(p$inst, "rcppr6.yml"))) {
      install_file("rcppr6.yml", p$inst)
    }

    if (!file.exists(file.path(p$src, "Makevars"))) {
      install_file("Makevars", p$src, verbose)
    }
    package_include <- file.path(p$include, sprintf("%s.h", package))
    if (!file.exists(package_include)) {
      update_template("package_include.h", package_include, info,
                      verbose, dest_is_directory=FALSE)
      if (verbose) {
        message("\t...you'll need to edit this file a bunch")
      }
    }
  }

  update_template("rcppr6_support.hpp", p$include, info, verbose)

  ## Only add this one if it's not there:

  classes <- read_classes(path)
  classes$update(verbose)

  invisible(NULL)
}

##' @export
##' @rdname update_rcppr6
##' @param ... Arguments passed to \code{update_rcppr6}
install <- function(...) {
  update_rcppr6(..., install=TRUE)
}


## NOTE: This duplicates some of the effort in check_DESCRIPTION
## NOTE: We'll not usually do this one.
update_DESCRIPTION <- function(path=".", verbose=TRUE) {
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

  filename <- file.path(path, "DESCRIPTION")
  if (!file.exists(filename)) {
    stop("Did not find DESCRIPTION file to modify")
  }
  d <- data.frame(read.dcf(filename),
                  stringsAsFactors=FALSE)
  d <- add_depends_if_missing("Rcpp", "LinkingTo", d, verbose)
  d <- add_depends_if_missing("Rcpp", c("Imports", "Depends"), d, verbose)
  d <- add_depends_if_missing("R6",   c("Imports", "Depends"), d, verbose)
  s <- paste(capture.output(write.dcf(d)), collapse="\n")
  update_file(s, filename, verbose)
}

paths <- function(path, package) {
  list(root        = path,
       inst        = file.path(path, "inst"),
       include     = file.path(path, "inst/include"),
       include_pkg = file.path(path, "inst/include", package),
       R           = file.path(path, "R"),
       src         = file.path(path, "src"))
}

## Seriously, don't use this.  This is for testing only.
uninstall <- function(path=".") {
  p <- paths(path, package_name(path))
  file_remove_if_exists(file.path(p$include_pkg, "rcppr6_pre.hpp"),
                        file.path(p$include_pkg, "rcppr6_post.hpp"),
                        file.path(p$R,           "rcppr6.R"),
                        file.path(p$src,         "rcppr6.cpp"),
                        file.path(p$include,     "rcppr6_support.hpp"))
}
