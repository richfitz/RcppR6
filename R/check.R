##' Check that a package is ready for use with rcppr6.  This just
##' checks for our requirements and prints diagnostics.  It is
##' probably unsufficient, but hopefully provides enough information.
##' @title Check Package is Ready to Use RCPPR6
##' @return An invisible logical value indicating if the package looks
##' ready for use with rcppr6.  However, if the package is not ready
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
                 "Main package header"=check_header_main(),
                 "src/Makevars"=check_Makevars(),
                 "rcppr6.yml"=check_yml())
  failed <- checks[sapply(checks, length) > 0]
  if (length(failed) > 0) {
    title <- paste0(names(failed), ":")
    body <- sapply(failed, function(x)
                   paste(paste0("\t", x, collapse="\n")))
    msg <- paste(c(rbind(title, body, deparse.level=0)),
                 collapse="\n")
    msg <- paste0("rcppr6 problems found in your package:\n", msg)
    if (error) {
      stop(msg, call.=FALSE)
    } else if (!quiet) {
      message(msg)
    }
  }
  invisible(length(failed) == 0)
}

check_DESCRIPTION <- function(path=".") {
  make_msg <- function(package, fields) {
    sprintf("Did not detect %s in %s",
            package, paste(fields, collapse=" or "))
  }
  req <- list(Rcpp=c("LinkingTo"),
              Rcpp=c("Imports", "Depends"),
              R6=c("Imports", "Depends"))
  d <- data.frame(read.dcf(file.path(path, "DESCRIPTION")),
                  stringsAsFactors=FALSE)
  f <- function(package, fields) {
    if (!depends(package, fields, d)) {
      make_msg(package, fields)
    }
  }
  mapply(f, names(req), req, USE.NAMES=FALSE)
}

check_NAMESPACE <- function(path=".") {
  does_import <- function(package, namespace) {
    for (ni in namespace$imports) {
      if ((is.character(ni) && identical(ni, package)) ||
          (is.list(ni) && identical(ni[[1]], package))) {
        return(TRUE)
      }
    }
    FALSE
  }
  n <- devtools::parse_ns_file(path)

  if (does_import("Rcpp", n)) {
    msg_rcpp <- character(0)
  } else {
    msg_rcpp <- "NAMESPACE must import something from Rcpp"
  }

  package <- package_name(path)

  if (package %in% n$dynlibs) {
    msg_dynlib <- character(0)
  } else {
    msg_dynlib <- sprintf("NAMESPACE must load dynamic library (%s)",
                          package)
  }
  c(msg_rcpp, msg_dynlib)
}

check_header_main <- function(path=".") {
  name <- package_name(path)
  header <- paste0(name, ".h")
  header_full <- file.path(path, "inst/include", header)
  if (file.exists(header_full)) {
    ## Ideally we'll check this file for the presence of the
    ## appropriate includes.  However, that might be organised
    ## differently, so I don't want to depend too strongly on it.
    ## Once we get libclang integration we could check for the
    ## inclusion of the rcppr6 headers, but I don't think at the
    ## moment that's tremendously worthwhile.
    character(0)
  } else {
    c("The file %s does not exist", header_full)
  }
}

check_Makevars <- function(path=".") {
  filename <- file.path(path, "src", "Makevars")
  expected <- "-I../inst/include"

  if (file.exists(filename)) {
    d <- readLines(filename)
    ## Really not going to try hard to parse this file for now, but
    ## we'll look for the most likely string:
    if (any(grepl(expected, d, fixed=TRUE))) {
      character(0)
    } else {
      sprintf("src/Makevars must contain 'PKG_CPPFLAGS += %s'", expected)
    }
  } else {
    sprintf("src/Makevars must exist and contain 'PKG_CPPFLAGS += %s'",
            expected)
  }
}

check_yml <- function(path=".") {
  filename <- file.path(path, "inst", "rcppr6.yml")
  if (file.exists(filename)) {
    character(0)
  } else {
    sprintf("inst/rcppr6.yml must exist")
  }
}
