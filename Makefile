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
	Rscript -e 'testthat::test_dir("tests/")'
covr:
	Rscript inst/covr/covr.R

pkgdown:
	Rscript -e "options(pkdown.internet = FALSE); pkgdown::build_site()"
