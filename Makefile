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
	R CMD INSTALL .

build:
	R CMD build .

doc:
	Rscript -e 'devtools::document()'

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

test:
	make install
	Rscript -e 'testthat::test_local()'

covr:
	Rscript inst/covr/covr.R

check:
	make doc
	make build
	R CMD CHECK --no-manual $(TARBALL)

pkgdown:
	Rscript -e "options(pkdown.internet = FALSE); pkgdown::build_site()"

data:
	cd data-raw && Rscript data.R

demo-doc:
	Rscript -e 'rmarkdown::render("inst/demo-table.Rmd", clean=TRUE)'
	Rscript -e 'rmarkdown::render("inst/demo-pmtable.R", clean=TRUE)'
	Rscript -e 'rmarkdown::render("inst/demo-pipe.Rmd", clean=TRUE)'
	Rscript -e 'rmarkdown::render("inst/demo-longtable.Rmd", clean=TRUE)'
	Rscript -e 'rmarkdown::render("inst/demo-sanitize.Rmd", clean=TRUE)'

quick:
	make doc
	make build
	R CMD check $(TARBALL) --ignore-vignettes --no-manual --no-tests --no-install

bump-dev:
	Rscript -e 'usethis::use_version("dev")'

tag-version:
	git tag $(VERSION)
	git push origin $(VERSION)

demo-check:
	Rscript -e 'testthat::test_file("tests/testthat/test-demo-check.R")'

spelling:
	Rscript -e "spelling::spell_check_package('.')"
