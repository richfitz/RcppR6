PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')

all:

include rcppr6/rcppr6.mk

install:
	R CMD INSTALL .

clean:
	cd src && rm -f *.o *.so

build:
	R CMD build .

check: build
	R CMD check --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

test:
	make -C tests/testthat

run_examples: install
	make -C inst/examples

.PHONY: attributes cog uncog document roxygen staticdocs publish_pages install clean build check
