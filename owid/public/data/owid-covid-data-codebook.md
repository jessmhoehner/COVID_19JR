# Codebook for the complete Our World in Data COVID-19 dataset

Column|Description|Source
------|-----------|------
`iso_code`|ISO 3166-1 alpha-3 – three-letter country codes|International Organization for Standardization
`continent`|Continent of the geographical location|Our World in Data
`location`|Geographical location|Our World in Data
`date`|Date of observation|Our World in Data
`total_cases`|Total confirmed cases of COVID-19|European Centre for Disease Prevention and Control
`new_cases`|New confirmed cases of COVID-19|European Centre for Disease Prevention and Control
`total_deaths`|Total deaths attributed to COVID-19|European Centre for Disease Prevention and Control
`new_deaths`|New deaths attributed to COVID-19|European Centre for Disease Prevention and Control
`total_cases_per_million`|Total confirmed cases of COVID-19 per 1,000,000 people|European Centre for Disease Prevention and Control
`new_cases_per_million`|New confirmed cases of COVID-19 per 1,000,000 people|European Centre for Disease Prevention and Control
`total_deaths_per_million`|Total deaths attributed to COVID-19 per 1,000,000 people|European Centre for Disease Prevention and Control
`new_deaths_per_million`|New deaths attributed to COVID-19 per 1,000,000 people|European Centre for Disease Prevention and Control
`total_tests`|Total tests for COVID-19|National government reports
`new_tests`|New tests for COVID-19|National government reports
`new_tests_smoothed`|New tests for COVID-19 (7-day smoothed). For countries that don't report testing data on a daily basis, we assume that testing changed equally on a daily basis over any periods in which no data was reported. This produces a complete series of daily figures, which is then averaged over a rolling 7-day window|National government reports
`total_tests_per_thousand`|Total tests for COVID-19 per 1,000 people|National government reports
`new_tests_per_thousand`|New tests for COVID-19 per 1,000 people|National government reports
`new_tests_smoothed_per_thousand`|New tests for COVID-19 (7-day smoothed) per 1,000 people|National government reports
`tests_units`|Units used by the location to report its testing data|National government reports
`stringency_index`|Government Response Stringency Index: composite measure based on 9 response indicators including school closures, workplace closures, and travel bans, rescaled to a value from 0 to 100 (100 = strictest response)|Oxford COVID-19 Government Response Tracker, Blavatnik School of Government
`population`|Population in 2020|United Nations, Department of Economic and Social Affairs, Population Division, World Population Prospects: The 2019 Revision
`population_density`|Number of people divided by land area, measured in square kilometers, most recent year available|World Bank – World Development Indicators, sourced from Food and Agriculture Organization and World Bank estimates
`median_age`|Median age of the population, UN projection for 2020|UN Population Division, World Population Prospects, 2017 Revision
`aged_65_older`|Share of the population that is 65 years and older, most recent year available|World Bank – World Development Indicators, based on age/sex distributions of United Nations Population Division's World Population Prospects: 2017 Revision
`aged_70_older`|Share of the population that is 70 years and older in 2015|United Nations, Department of Economic and Social Affairs, Population Division (2017), World Population Prospects: The 2017 Revision
`gdp_per_capita`|Gross domestic product at purchasing power parity (constant 2011 international dollars), most recent year available|World Bank – World Development Indicators, source from World Bank, International Comparison Program database
`extreme_poverty`|Share of the population living in extreme poverty, most recent year available since 2010|World Bank – World Development Indicators, sourced from World Bank Development Research Group
`cvd_death_rate`|Death rate from cardiovascular disease in 2017|Global Burden of Disease Collaborative Network, Global Burden of Disease Study 2017 Results
`diabetes_prevalence`|Diabetes prevalence (% of population aged 20 to 79) in 2017|World Bank – World Development Indicators, sourced from International Diabetes Federation, Diabetes Atlas
`female_smokers`|Share of women who smoke, most recent year available|World Bank – World Development Indicators, sourced from World Health Organization, Global Health Observatory Data Repository
`male_smokers`|Share of men who smoke, most recent year available|World Bank – World Development Indicators, sourced from World Health Organization, Global Health Observatory Data Repository
`handwashing_facilities`|Share of the population with basic handwashing facilities on premises, most recent year available|United Nations Statistics Division
`hospital_beds_per_thousand`|Hospital beds per 1,000 people, most recent year available since 2010|OECD, Eurostat, World Bank, national government records and other sources
`life_expectancy`|Life expectancy at birth in 2019|James C. Riley, Clio Infra, United Nations Population Division
