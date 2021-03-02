# GA-COVID
Code to accompany "The interplay of policy, behavior, and socioeconomic conditions in early COVID-19 epidemiology in Georgia"

Repository contains the following:

DATA
ga_tidy_df:  dataframe used in all four parts of analysis, giving county-level features
mobility_df: daily mobility for Georgia counties with available data between March 1 and April 30, modified from Descartes Lab, used in part 2 (event study)
ga_county_df: .csv version of ga_tidy_df
Counties_Georgia: shapefile of Georgia counties used to generate figure 1

CODE
make_df: code to extract and clean all covariates used in the analyses. Data used are publicly available through sources noted in manuscript and can be provided upon request
fig1_map: code to generate maps for figure 1
regressions_134: code to conduct regressions (part 1, 3, and 4 of the analysis) and generate corresopnding figures (3, 5, and 6)
eventstudy_pt2: code to conduct event study (part 2) and generate corresponding figure (2)
sup file 1: code to generate figure used in Supplementary File 1

