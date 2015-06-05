yaml_load <- RcppR6:::yaml_load

lang_output <- function(x, lang) {
  cat(c(sprintf("```%s", lang), x, "```"), sep="\n")
}
cpp_output <- function(x) lang_output(x, "c++")
r_output <- function(x) lang_output(x, "r")
yaml_output <- function(x) lang_output(x, "yaml")
plain_output <- function(x) lang_output(x, "plain")

tree <- function(path, header=path) {
  paste1 <- function(a, b) {
    paste(rep_len(a, length(b)), b)
  }
  indent <- function(x, files) {
    paste0(if (files) "| " else "  ", x)
  }
  is_directory <- function(x) {
    unname(file.info(x)[, "isdir"])
  }
  prefix_file <- "|--="
  prefix_dir  <- "|-+="

  files <- dir(path)
  files_full <- file.path(path, files)
  isdir <- is_directory(files_full)

  ret <- as.list(c(paste1(prefix_dir, files[isdir]),
                   paste1(prefix_file, files[!isdir])))
  files_full <- c(files_full[isdir], files_full[!isdir])
  isdir <- c(isdir[isdir], isdir[!isdir])

  n <- length(ret)
  ret[[n]] <- sub("|", "\\", ret[[n]], fixed=TRUE)
  tmp <- lapply(which(isdir), function(i)
    c(ret[[i]], indent(tree(files_full[[i]], NULL), !all(isdir))))
  ret[isdir] <- tmp

  c(header, unlist(ret))
}

vignette_prepare <- function(name) {
  path <- system.file(file.path("examples", name), package="RcppR6")
  path <- RcppR6:::prepare_temporary(path, tempfile())
  unlink(file.path(path, "tests"), recursive=TRUE)
  descr <- readLines(file.path(path, "DESCRIPTION"))
  descr <- descr[!grepl("Suggests: testthat", descr, fixed=TRUE)]
  writeLines(descr, file.path(path, "DESCRIPTION"))
  path
}
