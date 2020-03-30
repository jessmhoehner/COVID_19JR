# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License:  2019, EICC, GPL v2 or later
#COVID19/write/Makefile
#----------------------------------------------------

.PHONY:	all clean

all: output/write.pdf

clean: 
				-rm -r output/*

output/write.pdf: \
				src/write.Rmd 
		Rscript --vanilla -e "rmarkdown::render('$<')"
		mv src/write.pdf $@	
# done.
