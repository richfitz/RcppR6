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
  unlist(unname(lnapply(req, f)))
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
  does_import_msg <- function(package, namespace) {
    if (does_import(package, n)) {
      character(0)
    } else {
      sprintf("NAMESPACE must import something from %s", package)
    }
  }

  n <- parse_ns_file(path)
  msg_rcpp <- does_import_msg("Rcpp", n)
  msg_R6   <- does_import_msg("R6", n)

  package <- package_name(path)

  if (package %in% n$dynlibs) {
    msg_dynlib <- character(0)
  } else {
    msg_dynlib <- sprintf("NAMESPACE must load dynamic library (%s)",
                          package)
  }
  c(msg_rcpp, msg_R6, msg_dynlib)
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
    ## inclusion of the RcppR6 headers, but I don't think at the
    ## moment that's tremendously worthwhile.
    character(0)
  } else {
    sprintf("The file %s does not exist", header_full)
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
      sprintf("%s must contain 'PKG_CPPFLAGS += %s'",
              filename, expected)
    }
  } else {
    sprintf("%s must exist and contain 'PKG_CPPFLAGS += %s'",
            filename, expected)
  }
}

check_yml <- function(path=".") {
  res <- try(RcppR6_validate(RcppR6_read(path, FALSE)), silent=TRUE)
  if (inherits(res, "try-error")) {
    sprintf("Error loading yml:\n\t%s\n\t", res)
  } else if (length(res$classes) == 0) {
    sprintf("No classes found in package yml")
  } else {
    character(0)
  }
}
