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
yaml_load <- function(string) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full true/false:
  handlers <- list('bool#yes' = function(x) {
    if (identical(toupper(x), "TRUE")) TRUE else x},
                   'bool#no' = function(x) {
    if (identical(toupper(x), "FALSE")) FALSE else x})
  yaml::yaml.load(string, handlers=handlers)
}

yaml_read <- function(filename) {
  yaml_load(read_file(filename))
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

## Drop blank lines from a string.  Used to work around some
## whisker/mustache inconsistencies.
drop_blank <- function(x) {
  gsub("\n[[:space:]]*\n", "\n", x)
}

## Warn if keys are found in an object that are not in a known set.
warn_unknown <- function(name, defn, known) {
  unknown <- setdiff(names(defn), known)
  if (length(unknown) > 0) {
    warning(sprintf("Unknown fields in %s: %s",
                    name, collapse(unknown)),
            immediate.=TRUE)
  }
}

collect <- function(key, data, FUN=identity, ...) {
  sapply(data, function(x) FUN(x[[key]]))
}

collapse <- function(x, sep=", ") {
  paste(x, collapse=sep)
}

## Wrapper function to help with whisker
wr <- function(...) {
  res <- whisker::whisker.render(...)
  ## This is overly simple but it will do for now, given that whisker
  ## only outputs a few types:
  ##    whisker::escape --> amp, lt, gt, quot
  ## It obviously misses CDATA entities :)
  if (any(grepl("&[#a-zA-Z0-9]+;", res))) {
    stop("HTML entities detected in translated template (use triple '{'")
  }
  res
}

## Because of the devtools issue (hadley/devtools#531) we need to use
## a non-standard temporary file location for the tests.
prepare_temporary <- function(pkg, path="~/tmp") {
  if (!file.exists(path)) {
    dir.create(path)
  }
  pkg_dest <- file.path(path, basename(pkg))
  if (file.exists(pkg_dest)) {
    unlink(pkg_dest, recursive=TRUE)
  }
  file.copy(pkg, path, recursive=TRUE)
  invisible(pkg_dest)
}
