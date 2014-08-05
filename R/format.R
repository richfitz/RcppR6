format_class_list <- function(classes, package) {
  rcppr6    <- template_info_rcppr6()
  templates <- rcppr6_templates()
  info <- lapply(classes, format_class, package, rcppr6, templates)
  data <- list(rcppr6=rcppr6, package=package)
  ## TODO: There is a package version of this to use instead now.
  collect_2nl <- function(v, x) {
    collapse(unlist(collect(v, x)), "\n\n")
  }
  render <- function(template, v) {
    wr(template, c(data, structure(lapply(v, collect_2nl, info), names=v)))
  }

  collect_2nl("forward_declaration", info)
  collect("forward_declaration", info)

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

  if (any(collect("is_generic", classes))) {
    ret$r <- paste(ret$r, wr(templates$rcppr6_support.R, data),
                   sep="\n\n")
  }

  ret$support <- wr(templates$rcppr6_support.hpp, data)
  ret
}

format_class <- function(class, package, rcppr6, templates) {
  data <- list(rcppr6=rcppr6, package=package, class=class)

  ret <- list()
  ret$forward_declaration <- data$class$forward_declaration

  if (class$is_generic) {
    subtypes <- lapply(class$templates, format_class,
                       package, rcppr6, templates)
    collect_nl <- function(...) {
      collapse(collect(..., collapse, "\n"), "\n")
    }
    ret$r <- paste(format_class_r_generic(class, data, templates),
                   collect_nl("r", subtypes), sep="\n")
    ret$cpp <- collect_nl("cpp", subtypes)
    ret$rcpp_prototypes <- collect_nl("rcpp_prototypes", subtypes)
    ret$rcpp_definitions <- collect_nl("rcpp_definitions", subtypes)
  } else {
    ret$r <- format_class_r(class, data, templates)
    ret$cpp <- format_class_cpp(class, data, templates)
    ret$rcpp_prototypes  <- wr(templates$rcpp_prototypes,  data, templates)
    ret$rcpp_definitions <- wr(templates$rcpp_definitions, data, templates)
  }
  ret
}

format_class_r <- function(class, data, templates) {
  data$methods_r   <- format_methods_r(class$methods, data, templates)
  data$active_r    <- format_active_r(class$active, data, templates)
  wr(templates$r6_generator,
     c(data, class["constructor"]), templates)
}

format_class_r_generic <- function(class, data, templates) {
  wr(templates$r6_generator_generic,
     c(data, class["constructor"]), templates)
}

format_class_cpp <- function(class, data, templates) {
  constructor_cpp <- wr(templates$constructor_cpp,
                        c(data, class["constructor"]), templates)
  methods_cpp <- format_methods_cpp(class$methods, data, templates)
  active_cpp  <- format_active_cpp(class$active, data, templates)
  paste(c(constructor_cpp, methods_cpp, active_cpp), collapse="\n")
}

format_methods_r <- function(methods, data, templates) {
  if (length(methods) == 0) {
    NULL
  } else {
    methods_r <-
      lapply(methods, function(x)
             wr(templates$method_r, c(data, list(method=x)), templates))
    methods_r <- indent(paste(methods_r, collapse=",\n"), 16)
    paste(",\n", drop_blank(methods_r))
  }
}

format_active_r <- function(active, data, templates) {
  if (length(active) == 0) {
    NULL
  } else {
    active_r <-
      lapply(active, function(x)
             wr(templates$active_r, c(data, list(active=x)), templates))
    active_r <- indent(paste(active_r, collapse=",\n"), 16)
    paste0("\n", drop_blank(active_r))
  }
}

format_methods_cpp <- function(methods, data, templates) {
  if (length(methods) == 0) {
    NULL
  } else {
    methods_cpp <-
      lapply(methods, function(x)
             wr(templates$method_cpp, c(data, list(method=x)), templates))
    drop_blank(paste(methods_cpp, collapse="\n"))
  }
}

format_active_cpp <- function(active, data, templates) {
  if (length(active) == 0) {
    NULL
  } else {
    active_cpp <-
      lapply(active, function(x)
             wr(templates$active_cpp, c(data, list(active=x)), templates))
    drop_blank(paste(active_cpp, collapse="\n"))
  }
}
