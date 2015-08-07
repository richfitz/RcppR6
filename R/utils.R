package_name <- function(path=".") {
  read.dcf(file.path(path, "DESCRIPTION"), "Package")[[1]]
}

read_file <- function(...) {
  paste(readLines(...), collapse="\n")
}

with_default <- function(x, default=NULL) {
  if (is.null(x)) default else x
}

## Really simple-minded indenting, by a number of spaces:
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

## This is for processing data in the form
##     [key1: val1, key2: val2]
## which is how args are passed in and how concrete template
## parameters are passed in.  Things might not be named and that might
## be OK.
yaml_seq_map <- function(dat, named=TRUE) {
  if (is.null(dat) || length(dat) == 0) {
    return(structure(list(), names=character(0)))
  }
  assert_list(dat)
  ## First, check that everything is length 1:
  if (!all(sapply(dat, length) == 1L)) {
    stop("Expected every element to be length 1")
  }
  dat_contents <- lapply(dat, function(x) x[[1]])
  dat_names <- lapply(dat, names)
  dat_unnamed <- sapply(dat_names, is.null)
  if (named) {
    if (any(dat_unnamed)) {
      stop("All elements must be named")
    }
  } else {
    dat_names[dat_unnamed] <- dat_contents[dat_unnamed]
  }
  dat_names <- vapply(dat_names, identity, character(1L))
  names(dat_contents) <- dat_names
  dat_contents
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
      package %in% parse_dep(data[, field])
    } else {
      FALSE
    }
  }
  any(sapply(field, depend1))
}

parse_dep <- function(x) {
  x <- strsplit(x, "\\s*,\\s*")[[1]]
  sub("[^[:alnum:].].*", "", x)
}

parse_ns_file <- function(path) {
  parseNamespaceFile(basename(path), dirname(path))
}

## Drop blank lines from a string.  Used to work around some
## whisker/mustache inconsistencies.
drop_blank <- function(x) {
  sub("^\n", "", gsub("\n[[:space:]]*\n", "\n", x))
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
  sapply(data, function(x) FUN(x[[key]], ...))
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

join_lists <- function(x) {
  unlist(unname(x), FALSE, TRUE)
}

dput_to_character <- function(x) {
  capture.output(dput(x))
}

first <- function(x) {
  x[[1]]
}

strip_trailing_newline <- function(x) {
  sub("\n$", "", x)
}

read_dcf <- function(filename, ...) {
  d <- data.frame(read.dcf(filename, ...), stringsAsFactors=FALSE,
                  check.names=FALSE)
  ## Seems to be a bug in read.dcf:
  if ("Authors.R" %in% names(d)) {
    names(d)[names(d) == "Authors.R"] <- "Authors@R"
  }
  d
}

## Like strsplit, but only splits at the *first* occurence of the
## pattern.  Return value is a matrix with the first and second column
## being the left and right hand side of the match.  If no match is
## found, the right column will be NA_character_.  Will probably
## behave badly with things like NA values.
strsplit_first <- function(x, split,
                           fixed=FALSE, perl=FALSE, useBytes=FALSE) {
  info <- regexpr(split, x,
                  fixed=fixed, perl=perl, useBytes=useBytes)
  match <- which(info > 0)
  ret <- matrix(NA_character_, length(x), 2)
  ret[-match,1] <- x[-match]
  if (length(match) > 0) {
    pos <- info[match]
    len <- attr(info, "match.length")[match]
    ret[match,1] <- substr(x[match], 1, pos-1)
    ret[match,2] <- substr(x[match], pos + len, nchar(x[match]))
  }
  ret
}

is_scalar_character <- function(x) {
  length(x) == 1L && is.character(x)
}

isFALSE <- function(x) {
  identical(x, FALSE)
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
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

rename <- function(x, from, to) {
  assert_length(to, length(from), "to (arguments to rename)")
  i <- match(from, names(x))
  if (any(is.na(i))) {
    stop(sprintf("Did not find name %s in object",
                 paste(from[i], collapse=", ")))
  }
  names(x)[i] <- to
  x
}
