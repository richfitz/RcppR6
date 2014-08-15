## This is my personal project makefile, tweaked for use with
## RcppR6-using projects.

## Detect package name -- used within 'check'.  Very simple minded,
## may fail in odd ways.
PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')

all:

## RcppR6 targets:
RcppR6_install:
	Rscript -e "RcppR6::install()"

RcppR6:
	Rscript -e "RcppR6::RcppR6()"

attributes:
	Rscript -e "Rcpp::compileAttributes()"

roxygen:
	@mkdir -p man
	Rscript -e "library(methods); devtools::document()"

## Other useful targets:
install:
	R CMD INSTALL .

build:
	R CMD build .

check: build
	R CMD check --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

## Only useful if package is set up for use with testthat, which it
## probably should be :)
test:
	make -C tests/testthat

clean:
	cd src && rm -f *.o *.so

reset:
	Rscript -e "RcppR6:::uninstall()"
	make roxygen
