## Utility functions for installation type things.  Mostly
## installing/updating a file and reporting some information about if
## anything changed.
update_file <- function(str, dest, base, verbose=TRUE) {
  msg <- function(...) {
    if (verbose) {
      message(...)
    }
  }

  dest_str <- drop_leading_path(dest, base)

  if (file.exists(dest)) {
    old <- read_file(dest)
    if (identical(str, old)) {
      msg(sprintf("%s: skipping (unchanged)", dest_str))
      changed <- FALSE
    } else {
      msg(sprintf("%s: writing (changed)", dest_str))
      writeLines(str, dest)
      changed <- TRUE
    }
  } else {
    message(sprintf("%s: writing (new file)", dest_str))
    writeLines(str, dest)
    changed <- TRUE
  }
  invisible(changed)
}

install_file <- function(filename, dest_dir, base, verbose=TRUE,
                         overwrite=FALSE) {
  dest <- file.path(dest_dir, filename)
  dest_str <- drop_leading_path(dest, base)
  file_exists <- file.exists(dest)
  do_copy <- overwrite || !file_exists
  if (verbose) {
    if (file_exists && overwrite) {
      message(sprintf("Installing file %s (overwriting)", dest_str))
    } else if (!file_exists) {
      message(sprintf("Installing file %s (new file)", dest_str))
    }
  }
  if (do_copy) {
    file.copy(RcppR6_file(filename), dest, overwrite=TRUE)
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

create_directories <- function(paths) {
  for (p in paths) {
    dir.create(p, FALSE, TRUE)
  }
}

drop_leading_path <- function(file, base) {
  ## These might want normalising but that doesn't work if the file
  ## doesn't exist.  We could normalise on the dirname though?
  base <- gsub("/+", "/", base)
  file <- gsub("/+", "/", file)
  n <- nchar(base)
  if (identical(substr(base, 1, n),
                substr(file, 1, n))) {
    file <- sub("^/*", "", substr(file, n + 1L, nchar(file)))
  }
  file
}
