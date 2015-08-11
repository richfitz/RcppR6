## First pass: read the files.
##
## For now, the assumption is that files are in <pkgdir>/inst but that
## could change.
RcppR6_read <- function(path, verbose=TRUE) {
  config <- RcppR6_read_config(path)
  filename_classes <- file.path(path, config$classes)
  filename_functions <- file.path(path, config$functions)
  classes <- join_lists(lapply(filename_classes, RcppR6_read_classes,
                               path, verbose))
  functions <- join_lists(lapply(filename_functions, RcppR6_read_functions,
                                 path, verbose))
  list(path=path,
       classes=classes,
       functions=functions,
       hash=digest::digest(list(classes, functions)))
}

RcppR6_read_config <- function(path) {
  filename <- file.path(path, "inst/RcppR6.yml")
  if (file.exists(filename)) {
    dat <- yaml_read(filename)
    if (is.null(dat$functions)) {
      dat$functions <- character(0)
    }
  } else {
    dat <- RcppR6_config_default(path)
  }
  warn_unknown("RcppR6", dat, c("classes", "functions"))
  assert_character(dat$classes)
  assert_character(dat$functions)
  if (length(dat$classes) == 0) {
    stop("Need at least one set of classes")
  }
  dat
}

RcppR6_read_classes <- function(filename, base, verbose) {
  if (verbose) {
    message("Reading classes from ", drop_leading_path(filename, base))
  }
  assert_file_exists(filename)
  yaml_read(filename)
}

RcppR6_read_functions <- function(filename, base, verbose) {
  if (verbose) {
    message("Reading functions from ", drop_leading_path(filename, base))
  }
  assert_file_exists(filename)
  yaml_read(filename)
}


RcppR6_config_default <- function(path) {
  functions <- "inst/RcppR6_functions.yml"
  if (!file.exists(file.path(path, functions))) {
    functions <- character(0)
  }
  list(classes="inst/RcppR6_classes.yml",
       functions=functions)
}

## Read all templates.  This makes things slightly simpler later.
RcppR6_read_templates <- function() {
  path <- RcppR6_file("templates")
  files <- dir(path, pattern=glob2rx("*.whisker"))
  dat <- lapply(file.path(path, files), read_file)
  names(dat) <- sub("\\.whisker$", "", files)
  dat
}

## Extract a file from the RcppR6 (this!) package.  Just sets some
## defaults to \code{\link{system.file}}
RcppR6_file <- function(...) {
  system.file(..., package=.packageName, mustWork=TRUE)
}
