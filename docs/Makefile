all:
	BOOKDOWN_FULL_PDF=false Rscript --quiet _render.R

pdf:
	Rscript --quiet _render.R "bookdown::pdf_book" &&\
	mv _book/blogdown.pdf _book/blogdown-full.pdf

pdf2:
	BOOKDOWN_FULL_PDF=false Rscript --quiet _render.R "bookdown::pdf_book"

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

clean:
	Rscript --quiet -e "bookdown::clean_book(TRUE)"
