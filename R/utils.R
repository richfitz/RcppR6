##' Internal function to find the location of the package.  This is
##' used in the Makefiles that it provides.
##' @title Path Name of Installed Package
##' @return Nothing -- called for the side effect of printing the path
##' name to the console.
path <- function() {
  writeLines(system.file(package="rcppr6"))
}

##' Determine name of a package
##' @title Determine name of a package
##' @param path Path to the package (DESCRIPTION must be in this
##' directory).  Defaults to the current directory.
package_name <- function(path=".") {
  read.dcf(file.path(path, "DESCRIPTION"), "Package")[[1]]
}

##' Extract a file from the rcppr6 (this!) package.  Just sets some
##' defaults to \code{\link{system.file}}
##'
##' @title Get RCPPR6 File
##' @param ... Passed to \code{\link{system.file}}
rcppr6_file <- function(...) {
  system.file(..., package="rcppr6", mustWork=TRUE)
}

rcppr6_version <- function() {
  as.character(packageVersion("rcppr6"))
}

read_file <- function(...) {
  paste(readLines(...), collapse="\n")
}

with_default <- function(x, default=NULL) {
  if (is.null(x)) default else x
}

indent <- function(str, n) {
  indent <- paste(rep(" ", n), collapse="")
  paste(indent, strsplit(str, "\n", fixed=TRUE)[[1]],
        sep="", collapse="\n")
}


## https://github.com/viking/r-yaml/issues/5#issuecomment-16464325
yaml_read <- function(filename) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full true/false:
  handlers <- list('bool#yes' = function(x) {
    if (identical(toupper(x), "TRUE")) TRUE else x},
                   'bool#no' = function(x) {
    if (identical(toupper(x), "FALSE")) FALSE else x})
  yaml::yaml.load(read_file(filename), handlers=handlers)
}

## Pattern where we have a named list and we want to call function
## 'FUN' with rather than just
##    {FUN(X[[1]], ...), ..., FUN(X[[n]], ...)}
## instead as
##    {FUN{names(X)[1], X[[1]], ...}, ..., names(X)[1], X[[1]], ...}
## this can be achived via mapply, but it's not pleasant.
lnapply <- function(X, FUN, ...) {
  nX <- names(X)
  res <- lapply(seq_along(X), function(i) FUN(nX[[i]], X[[i]], ...))
  names(res) <- nX
  res
}

## Basically just turn down warnings in file.remove to act more like
## shell's 'rm -f'
file_remove_if_exists <- function(...) {
  files <- c(...)
  for (f in files) {
    if (file.exists(f)) {
      file.remove(f)
    }
  }
  invisible(NULL)
}

## Determine if a package is depended on in, in any number of a set of
## fields parsed out of a DESCRIPTION file.
depends <- function(package, field, data) {
  depend1 <- function(field) {
    if (field %in% colnames(data)) {
      package %in% devtools::parse_deps(data[,field])$name
    } else {
      FALSE
    }
  }
  any(sapply(field, depend1))
}
