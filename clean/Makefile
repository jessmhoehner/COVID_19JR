# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License:  2019, EICC, GPL v2 or later
# 
#NCV19/clean/Makefile
#----------------------------------------------------
#

HERE := $(shell git rev-parse --show-toplevel)

clean_data = \
		$(HERE)/clean/output/WHO_sit_rep_clean.csv \
		$(HERE)/clean/output/nga_cv19_clean.csv 

.PHONY:	all clean

all: $(clean_data)

clean:
			-rm -r output/*
		
output/: \
			src/clean.R \
			$(HERE)/clean/input/WHO_sit_rep.csv \
			$(HERE)/clean/input/NGA_COVID_19_JR.csv
	Rscript --vanilla $<
	
# done.
