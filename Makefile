doc:
	Rscript -e 'devtools::document()'
readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'
test:
	Rscript -e 'testthat::test_dir("tests/testthat")'
covr:
	Rscript inst/covr/covr.R

