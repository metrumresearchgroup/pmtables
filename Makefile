doc:
	Rscript -e 'devtools::document()'
readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

