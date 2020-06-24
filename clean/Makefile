# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License:  2020, EICC, GPL v3 or later
# 
#COVID19/clean/Makefile
#----------------------------------------------------
#

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

clean_data = \
			$(HERE)/graph/input/ecdc_clean.csv \
			$(HERE)/graph/input/nyt_clean.csv
		
.PHONY:	all clean

all: $(clean_data)

clean:

	rm write/input/*
		
$(clean_data): \
			$(HERE)/clean/src/clean.R \
			$(HERE)/owid/covid-19-data/public/data/ecdc/full_data.csv \
			$(HERE)/NYTimes/us-counties.csv 
		Rscript --vanilla $<
	
# done.
