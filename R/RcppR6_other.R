read_RcppR6_yml <- function(path=".", verbose=TRUE,
                            warn_missing=TRUE) {
  filename <- file.path(path, "inst/RcppR6.yml")
  if (file.exists(filename)) {
    dat <- yaml_read(filename)
  } else {
    if (verbose) {
      message("No configuration found - using default")
    }
    dat <- config_default()
  }

  warn_unknown("RcppR6", dat, "classes")
  assert_character(dat$classes)
  if (length(dat$classes) == 0) {
    stop("Need at least one set of classes")
  }

  filename_classes <- file.path(path, dat$classes)
  classes <- join_lists(lapply(filename_classes,
                               read_RcppR6_classes, verbose, warn_missing))
  list(classes=classes)
}

read_RcppR6_classes <- function(filename, verbose,
                                warn_missing=TRUE) {
  if (verbose) {
    message("Reading classes from ", filename)
  }
  if (file.exists(filename)) {
    yaml_read(filename)
  } else {
    msg <- sprintf("Filename %s does not exist", filename)
    if (warn_missing) {
      warning(msg)
    } else if (verbose) {
      message(msg)
    }
    NULL
  }
}

##' Extract a file from the RcppR6 (this!) package.  Just sets some
##' defaults to \code{\link{system.file}}
##'
##' @title Get RcppR6 File
##' @param ... Passed to \code{\link{system.file}}
RcppR6_file <- function(...) {
  system.file(..., package="RcppR6", mustWork=TRUE)
}

RcppR6_version <- function() {
  as.character(packageVersion("RcppR6"))
}

## Read all templates.  This makes things slightly simpler later.
RcppR6_read_templates <- function() {
  path <- RcppR6_file("templates")
  files <- dir(path, pattern=glob2rx("*.whisker"))
  dat <- lapply(file.path(path, files), read_file)
  names(dat) <- sub("\\.whisker$", "", files)
  dat
}

config_default <- function() {
  list(classes="inst/RcppR6_classes.yml")
}
