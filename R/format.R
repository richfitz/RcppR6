format_class_list <- function(classes, package) {
  rcppr6    <- template_info_rcppr6()
  templates <- rcppr6_templates()
  info <- lapply(classes, format_class, package, rcppr6, templates)
  data <- list(rcppr6=rcppr6, package=package)
  collect <- function(v, x) {
    paste(unlist(sapply(x, "[[", v), use.names=FALSE), collapse="\n\n")
  }
  render <- function(template, v) {
    wr(template, c(data, structure(lapply(v, collect, info), names=v)))
  }

  ## At this point, the contents of these strings are complete files,
  ## ready to write.  So we return nothing but the strings
  ## themselves.
  ret <- list()
  ret$r           <- render(templates$rcppr6.R, "r")
  ret$cpp         <- render(templates$rcppr6.cpp, "cpp")
  ret$rcppr6_pre  <- render(templates$rcppr6_pre.hpp,
                            c("forward_declaration", "rcpp_prototypes"))
  ret$rcppr6_post <- render(templates$rcppr6_post.hpp,
                            "rcpp_definitions")

  ret$support     <- wr(templates$rcppr6_support.hpp, data)
  ret
}

format_class <- function(class, package, rcppr6, templates) {
  data <- list(rcppr6=rcppr6, package=package, class=class)

  ret <- list()

  ## We always have a constructor:
  ret$constructor_r <- wr(templates$constructor_r,
                          c(data, class["constructor"]), templates)
  ret$constructor_cpp <- wr(templates$constructor_cpp,
                          c(data, class["constructor"]), templates)

  ## We always have rcpp stubs:
  ret$forward_declaration <- data$class$forward_declaration
  ret$rcpp_prototypes  <- wr(templates$rcpp_prototypes,  data, templates)
  ret$rcpp_definitions <- wr(templates$rcpp_definitions, data, templates)

  if (length(class$methods) > 0) {
    methods_r <-
      lapply(class$methods, function(x)
             wr(templates$method_r, c(data, list(method=x)), templates))
    methods_cpp <-
      lapply(class$methods, function(m)
             wr(templates$method_cpp, c(data, list(method=m)), templates))
    methods_r   <- paste0(",\n",
                          indent(paste(methods_r, collapse=",\n"), 16))
    methods_cpp <- paste(methods_cpp, collapse="\n")
    ## More formatting tweaks: whisker leaves incorrect blank lines:
    ret$methods_r   <- drop_blank(methods_r)
    ret$methods_cpp <- drop_blank(methods_cpp)
  }

  if (length(class$active) > 0) {
    active_r <-
      lapply(class$active, function(x)
             wr(templates$active_r, c(data, list(active=x)), templates))
    active_cpp <-
      lapply(class$active, function(m)
             wr(templates$active_cpp, c(data, list(active=m)), templates))
    active_r   <- paste0("\n",
                         indent(paste(active_r, collapse=",\n"), 16))
    active_cpp <- paste(active_cpp, collapse="\n")
    ## More formatting tweakss: whisker leaves incorrect blank lines:
    ret$active_r   <- drop_blank(active_r)
    ret$active_cpp <- drop_blank(active_cpp)
  }

  ## Add these to the constructor:
  ret$generator_r <- wr(templates$r6_generator, c(data, ret))
  ret$r <- paste(c(ret$constructor_r, ret$generator_r),
                 collapse="\n")
  ret$cpp <- paste(c(ret$constructor_cpp, ret$methods_cpp, ret$active_cpp),
                   collapse="\n")
  ret
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
