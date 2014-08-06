PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')

all:

install:
	R CMD INSTALL .

clean:
	make -C src clean

build:
	R CMD build .

check: cleanup build
	R CMD check --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

roxygen:
	@mkdir -p man
	Rscript -e "library(methods); devtools::document()"

test:
	Rscript -e 'library(methods); devtools::test()'

cleanup:
	rm -f `find inst -name '*.o' -or -name '*.so'`

.PHONY: all install clean build check roxygen test
