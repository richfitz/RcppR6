PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

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
	${RSCRIPT} -e "library(methods); devtools::document()"

test:
	${RSCRIPT} -e 'library(methods); devtools::test()'

cleanup:
	rm -f `find inst -name '*.o' -or -name '*.so'`

vignettes/introduction.Rmd: vignettes/src/introduction.R
	${RSCRIPT} -e 'library(sowsear); sowsear("$<", output="$@")'
vignettes/examples.Rmd: vignettes/src/examples.R
	${RSCRIPT} -e 'library(sowsear); sowsear("$<", output="$@")'

vignettes: vignettes/introduction.Rmd vignettes/examples.Rmd
	${RSCRIPT} -e 'library(methods); devtools::build_vignettes()'

.PHONY: all install clean build check roxygen test vignettes
