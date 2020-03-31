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
		$(HERE)/write/input/ecdc_clean.csv \
		$(HERE)/write/input/nyt_clean.csv 

.PHONY:	all clean

all: $(clean_data)

clean:
			-rm -r output/*
		
output/: \
		src/clean.R \
	$(HERE)/owid/covid-19-data/public/data/ecdc/full_data.csv" \
	$(HERE)/NYTimes/covid-19-data/us-counties.csv
	Rscript --vanilla $<
	
# done.
