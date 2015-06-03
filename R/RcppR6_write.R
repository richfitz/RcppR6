RcppR6_write <- function(dat, verbose=TRUE) {
  create_directories(dat$package$paths)
  ## There should be five things here; probably move dat around to
  ## make this less fragile (TODO)
  files <- names(dat$contents)
  for (file in files) {
    update_file(dat$contents[[file]], dat$package$files[[file]],
                dat$package$paths$root, verbose)
  }
}

RcppR6_install_files <- function(info, verbose=TRUE) {
  create_directories(info$paths)

  update_DESCRIPTION(info$paths$root, verbose)
  install_file("Makevars", info$paths$src, info$paths$root, verbose)
  if (!file.exists(info$files$package_include)) {
    template <-
      read_file(RcppR6_file("templates/package_include.h.whisker"))
    update_file(wr(template, list(package=info)),
                info$files$package_include, verbose)
    if (verbose) {
      message("\t...you'll need to edit this file a bunch")
    }
  }
  namespace <- file.path(info$paths$root, "NAMESPACE")
  if (!file.exists(namespace)) {
    if (verbose) {
      message("Writing empty NAMESPACE")
    }
    writeLines(character(0), namespace)
  }
}

RcppR6_run_attributes <- function(path, verbose) {
  if (verbose) {
    message("Compiling Rcpp attributes")
  }
  Rcpp::compileAttributes(path)
}

update_DESCRIPTION <- function(path, verbose=TRUE) {
  add_depends_if_missing <- function(package, field, data, verbose) {
    if (!depends(package, field, data)) {
      field <- field[[1]]
      if (verbose) {
        message(sprintf("DESCRIPTION: Adding dependency %s in field %s",
                        package, field))
      }
      if (field %in% names(data)) {
        data[[field]] <- paste(data[[field]], package, sep=", ")
      } else {
        data[[field]] <- package
      }
    }
    data
  }

  filename <- file.path(path, "DESCRIPTION")
  if (!file.exists(filename)) {
    stop("Did not find DESCRIPTION file to modify")
  }

  d <- d_orig <- read_dcf(filename)

  d <- add_depends_if_missing("Rcpp", "LinkingTo", d, verbose)
  d <- add_depends_if_missing("Rcpp", c("Imports", "Depends"), d, verbose)
  d <- add_depends_if_missing("R6",   c("Imports", "Depends"), d, verbose)

  if (isTRUE(all.equal(d, d_orig))) {
    if (verbose) {
      message("DESCRIPTION looks good: leaving alone")
    }
  } else {
    s <- paste(capture.output(write.dcf(d)), collapse="\n")
    update_file(s, filename, path, verbose)
  }
}

uninstall <- function(path=".", verbose=TRUE, attributes=TRUE) {
  info <- RcppR6_package_info(path)

  p <- info$paths
  file_remove_if_exists(file.path(p$include_pkg, "RcppR6_pre.hpp"),
                        file.path(p$include_pkg, "RcppR6_post.hpp"),
                        file.path(p$include_pkg, "RcppR6_support.hpp"),
                        file.path(p$R,           "RcppR6.R"),
                        file.path(p$src,         "RcppR6.cpp"),
                        verbose=verbose)
  ## We leave alone the package include file, Makevars, DESCRIPTION,
  ## even if they look like something that we've modified.
  if (attributes) {
    RcppR6_run_attributes(path, verbose)
  }
  dir_remove_if_empty(info$paths)
}

## Because of the devtools issue (hadley/devtools#531) we need to use
## a non-standard temporary file location for the tests.
prepare_temporary <- function(pkg, path="~/tmp") {
  if (!file.exists(path)) {
    dir.create(path)
  }
  pkg <- normalizePath(pkg)
  pkg_dest <- file.path(path, basename(pkg))
  if (file.exists(pkg_dest)) {
    unlink(pkg_dest, recursive=TRUE)
  }
  file.copy(pkg, path, recursive=TRUE)
  invisible(pkg_dest)
}
