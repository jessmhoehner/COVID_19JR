# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License:  2020, EICC, GPL v3 or later
#COVID19/graph/Makefile

#----------------------------------------------------

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

graph_data = \
			$(HERE)/graph/output/datasets/WashingtonDC_obscases.csv \
			$(HERE)/graph/output/datasets/Fairfax_VA_obscases.csv \
			$(HERE)/graph/output/datasets/Fulton_GA_obscases.csv \
			$(HERE)/graph/output/datasets/Genesee_MI_obscases \
			$(HERE)/graph/output/datasets/Gwinnett_GA_obscases.csv \
			$(HERE)/graph/output/datasets/Madison_NY_obscases.csv \
			$(HERE)/graph/output/datasets/Manatee_FL_obscases.csv \
			$(HERE)/graph/output/datasets/Randolph_NC_obscases.csv \
			$(HERE)/graph/output/datasets/Stafford_VA_obscases.csv \
			$(HERE)/graph/output/datasets/Taylor_TX_obscases.csv \
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

	rm graph/output/*
		
$(graph_data): \
			$(HERE)/graph/src/graph.R \
			$(HERE)/graph/input/ecdc_clean.csv \
			$(HERE)/graph/input/nyt_clean.csv
		Rscript --vanilla $<
	
# done.
