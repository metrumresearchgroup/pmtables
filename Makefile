SHELL := /bin/bash
PACKAGE=pmtables
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.
all:
	make doc
	make install
	make build
install:
	R CMD install .
build:
	R CMD build .
doc:
	Rscript -e 'devtools::document()'
readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'
test:
	make install
	Rscript -e 'testthat::test_dir("tests/")'
covr:
	Rscript inst/covr/covr.R
check:
	make doc
	make build
	R CMD check $(TARBALL)
pkgdown:
	Rscript -e "options(pkdown.internet = FALSE); pkgdown::build_site()"

data:
	Rscript inst/script/data.R
	Rscript inst/script/data-tabular.R

demo-doc:
	Rscript -e 'rmarkdown::render("inst/demo.Rmd")'

quick:
	make doc
	make build
	R CMD check $(TARBALL) --ignore-vignettes --no-manual --no-tests --no-install

tag-version:
	git tag $(VERSION)

