# Paper-pandemic-births
 R scripts used on the paper "The Impact of the COVID-19 pandemic on birth counts in selected Latin American Countries"


This repository has the following files:
 - Data folder: data used for the calculations on this paper; The file Variation_of_births.csv show the percentual variation of births by month. (A more detailed explanation is founded on the methodology section of the study). The file wcde_data.csv has the TFR projections for each country given by the Wittgenstein Centre. 
- Images folder: Graph of the time series made by the script Time-Series-by-country.R  
 - Time-Series-by-Country.R: R file with code to do an ARIMA process for each country, grouped by age and years of education.
 - Total_Fecundity_Rate.R: This script uses the projections of the Total Fecundity Rate for the period 2020-2025 given by the Wittgenstein Centre for Demography and Global Human Capital, and apply the effects of the variation of births on July 2022, in order to compare the effect of the pandemic in the TFR for the countries involved in the study.  

	The projection of the Wittgenstein Centre are available and can be found on https://dataexplorer.wittgensteincentre.org/wcde-v3/ 
