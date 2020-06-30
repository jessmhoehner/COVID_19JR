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
			$(HERE)/graph/input/fl_stlev.csv \
			$(HERE)/graph/input/va_stlev.csv \
			$(HERE)/graph/input/ny_stlev.csv \
			$(HERE)/graph/input/ga_stlev.csv \
			$(HERE)/graph/input/nc_stlev.csv \
			$(HERE)/graph/input/tx_stlev.csv \
			$(HERE)/graph/input/mi_stlev.csv \
			$(HERE)/graph/input/dc_dc.csv \
			$(HERE)/graph/input/va_fairfax.csv \
			$(HERE)/graph/input/va_stafford.csv \
			$(HERE)/graph/input/ga_fulton.csv \
			$(HERE)/graph/input/ga_gwinnett.csv \
			$(HERE)/graph/input/fl_manatee.csv \
			$(HERE)/graph/input/ny_madison.csv \
			$(HERE)/graph/input/tx_taylor.csv \
			$(HERE)/graph/input/nc_randolph.csv \
			$(HERE)/graph/input/mi_genesee.csv \
			$(HERE)/graph/input/va_manassas.csv \
			$(HERE)/graph/input/cvt_filtered.csv
			
.PHONY:	all clean

all: $(clean_data)

clean:
	rm $(HERE)/graph/input/*
		
$(clean_data): \
			$(HERE)/clean/src/clean.R \
			$(HERE)/clean/input/countycases.csv \
			$(HERE)/clean/input/deaths.csv \
			$(HERE)/clean/input/demographics.csv \
			$(HERE)/NYTimes/us-counties.csv \
			$(HERE)/covid-public-api/v1/states/daily.csv
		Rscript --vanilla $<
	
# done.
