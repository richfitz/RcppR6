## Support functions for using whisker.

## TODO: This is unfortunate, and can be fixed by updating *all*
## templates to use '{{{...}}}' in place of '{{...}}'.  Leaving that
## for now though.

whisker_unescape <- function(x) {
  x <- gsub("&amp;",  "&",  x)
  x <- gsub("&lt;",   "<",  x)
  x <- gsub("&gt;",   ">",  x)
  x <- gsub("&quot;", "\"", x)
  x
}

wr <- function(...) {
  res <- whisker::whisker.render(...)
  ## For now, undoing all HTML escape sequences:
  whisker_unescape(res)
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
