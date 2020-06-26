# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License:  2020, EICC, GPL v3 or later
#COVID19/graph/Makefile

#----------------------------------------------------

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

graph_data = \
			$(HERE)/graph/output/plots/Virginia_ppos.png \
			$(HERE)/graph/output/plots/Georgia_ppos.png \
			$(HERE)/graph/output/plots/Texas_ppos.png \
			$(HERE)/graph/output/plots/North_Carolina_ppos.png \
			$(HERE)/graph/output/plots/New_York_ppos.png \
			$(HERE)/graph/output/plots/Florida_ppos.png \
			$(HERE)/graph/output/plots/Michigan_ppos.png \
			$(HERE)/graph/output/plots/multistate_ppos.png \
			$(HERE)/graph/output/plots/Fairfax_VA_casesbyday.png \
			$(HERE)/graph/output/plots/Fulton_GA_casesbyday.png \
			$(HERE)/graph/output/plots/Genesee_MI_casesbyday.png \
			$(HERE)/graph/output/plots/Gwinnett_GA_casesbyday.png \
			$(HERE)/graph/output/plots/Madison_NY_casesbyday.png \
			$(HERE)/graph/output/plots/Manatee_FL_casesbyday.png \
			$(HERE)/graph/output/plots/Randolph_NC_casesbyday.png \
			$(HERE)/graph/output/plots/Stafford_VA_casesbyday.png \
			$(HERE)/graph/output/plots/Taylor_TX_casesbyday.png \
			$(HERE)/graph/output/plots/WashingtonDC_casesbyday.png 
			
.PHONY:	all clean

all: $(graph_data)

clean:
	rm $(HERE)/graph/output/plots/*
		
$(graph_data): \
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
			$(HERE)/graph/input/cvt_filtered.csv
		Rscript --vanilla $<
	
# done.
