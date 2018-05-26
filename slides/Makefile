all: slides.html

%.html: %.Rmd
	Rscript -e "rmarkdown::render('$^')"
