## Utility functions for installation type things.  Mostly
## installing/updating a file and reporting some information about if
## anything changed.
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
      changed <- FALSE
    } else {
      msg(sprintf("%s: writing (changed)", dest))
      writeLines(str, dest)
      changed <- TRUE
    }
  } else {
    message(sprintf("%s: writing (new file)", dest))
    writeLines(str, dest)
    changed <- TRUE
  }
  invisible(changed)
}

install_file <- function(filename, dest_dir, verbose=TRUE,
                         overwrite=FALSE) {
  dest <- file.path(dest_dir, filename)
  file_exists <- file.exists(dest)
  do_copy <- overwrite || !file_exists
  if (verbose) {
    if (file.exists(dest) && overwrite) {
      message(sprintf("Installing file %s (overwriting)", dest))
    }
  } else if (!file_exists) {
    message(sprintf("Installing file %s (new file)", dest))
  }
  if (do_copy) {
    file.copy(rcppr6_file(filename), dest, overwrite=TRUE)
  }
  invisible(do_copy)
}

## Basically just turn down warnings in file.remove to act more like
## shell's 'rm -f'
file_remove_if_exists <- function(..., verbose=FALSE) {
  files <- c(...)
  for (f in files) {
    if (file.exists(f)) {
      if (verbose) {
        message("Removing file ", f)
      }
      file.remove(f)
    }
  }
  invisible(NULL)
}

dir_remove_if_empty <- function(..., verbose=FALSE) {
  dirs <- c(...)
  for (d in dirs) {
    if (file.exists(d) && is_directory(d) &&
        length(dir(d, all.files=TRUE)) == 0) {
      if (verbose) {
        message("Removing empty directory ", d)
      }
      file.remove(d)
    }
  }
}

is_directory <- function(path) {
  file.info(path)[["isdir"]]
}
