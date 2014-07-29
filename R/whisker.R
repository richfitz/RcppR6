## Support functions for using whisker.

wr <- function(...) {
  res <- whisker::whisker.render(...)
  ## This is overly simple but it will do for now, given that whisker
  ## only outputs a few types:
  ##    whisker::escape --> amp, lt, gt, quot
  ## It obviously misses CDATA entities :)
  re <- "&[#a-zA-Z0-9]+;"
  if (any(grepl(re, res))) {
    stop("HTML entities detected in translated template (use triple '{'")
  }
  res
}

wr_file <- function(filename, ...) {
  wr(get_template(filename), ...)
}

get_template <- function(filename) {
  read_file(rcppr6_file(file.path("templates", filename)))
}

update_template <- function(filename, dest, info, verbose=TRUE,
                            dest_is_directory=TRUE) {
  if (dest_is_directory) {
    dest <- file.path(dest, basename(filename))
  }
  str <- wr_file(filename, info)
  update_file(str, dest, verbose)
}

update_file <- function(str, dest, verbose=TRUE) {
  msg <- function(...) {
    if (verbose) {
      message(...)
    }
  }
  if (file.exists(dest)) {
    old <- read_file(dest)
    if (identical(str, old)) {
      msg(sprintf("%s: skipping (unchanged)", dest))
    } else {
      msg(sprintf("%s: writing (changed)", dest))
      writeLines(str, dest)
    }
  } else {
    message(sprintf("%s: writing (new file)", dest))
    writeLines(str, dest)
  }
}

install_file <- function(filename, dest, verbose=TRUE,
                         dest_is_directory=TRUE) {
  if (dest_is_directory) {
    dest <- file.path(dest, basename(filename))
  }
  if (verbose) {
    message("Installing file ", dest)
  }
  file.copy(rcppr6_file(filename), dest, overwrite=TRUE)
}
