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
##' @export
RcppR6 <- function(path=".", install=FALSE,
                   attributes=TRUE, verbose=TRUE) {
  dat <- RcppR6_read(path)
  dat_valid <- RcppR6_validate(dat)

  ## This is surprisingly slow; whisker render is the culprit.  Need
  ## to get that faster; might try something that allows for compiled
  ## templates.  wr takes 92% and parseTemplate 85% so not great.
  ## Will need to look carefully at this and try to get it faster.
  code <- RcppR6_generate(dat_valid)

  if (install) {
    RcppR6_install_files(code$package, verbose)
  }

  RcppR6_write(code)

  if (attributes) {
    RcppR6_run_attributes(path, verbose)
  }
}

##' @export
##' @rdname RcppR6
##' @param ... Arguments passed to \code{RcppR6()}
install <- function(...) {
  RcppR6(..., install=TRUE)
}

##' Check that a package is ready for use with RcppR6.  This just
##' checks for our requirements and prints diagnostics.  It is
##' probably unsufficient, but hopefully provides enough information.
##' @title Check Package is Ready to Use
##' @return An invisible logical value indicating if the package looks
##' ready for use with RcppR6.  However, if the package is not ready
##' and \code{error} is \code{TRUE}, then nothing is returned as the
##' function will throw an error.
##' @author Rich FitzJohn
##' @param path Path to the package
##' @param error Logical indicating if problems should be treated as errors
##' @param quiet Logical indicating if a description of problems
##' should be printed.
##' @export
check <- function(path=".", error=TRUE, quiet=FALSE) {
  checks <- list(DESCRIPTION=check_DESCRIPTION(path),
                 NAMESPACE=check_NAMESPACE(path),
                 "Main package header"=check_header_main(path),
                 "src/Makevars"=check_Makevars(path),
                 "yml"=check_yml(path))
  failed <- checks[sapply(checks, length) > 0]
  if (length(failed) > 0) {
    title <- paste0(names(failed), ":")
    body <- sapply(failed, function(x)
                   paste(paste0("\t", x, collapse="\n")))
    msg <- paste(c(rbind(title, body, deparse.level=0)),
                 collapse="\n")
    msg <- paste0("RcppR6 problems found in your package:\n", msg)
    if (error) {
      stop(msg, call.=FALSE)
    } else if (!quiet) {
      message(msg)
    }
  }
  invisible(length(failed) == 0)
}
