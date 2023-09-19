# Correspondence among multiple methods provides confidence when measuring Marine Protected Area effects for species and assemblages
This repository houses data and code used for anaylses in the publication "Correspondence among multiple methods provides confidence when measuring Marine Protected Area effects for species and assemblages"

1. Data - raw data used by the included R scripts
    - density_data.csv - Density data from 3 MPA monitoring programs compared in the analyses
    - Clean_BRUV_Range_standardized_density_data.csv - Range standardized density data for nMDS and diversity anaylses
    - CLEANBRUV_Summary_by_Island_by_MPA_Year.csv - density and biomass data summarized by MPA-Year and used to calculate response ratios
    - 2_Clean_BRUV_All_Spp_Fish_Biomass_RR.csv - Biomass response ratios used to create a forest plot
    - 2_Clean_BRUV_All_Spp_Fish_Density_RR.csv - Density response ratios used to create a forest plot
  
2. R Scripts for Analyses
    - Standardization for diversity stats.R - Code used to range standardize density_data.csv for diversity and nMDS analyses
    - standardized_spp_div_and_richness_code.R - Code used to perform diversity and richenss analyses
    - Forest Plot.R - Creates forest plots from biomass and density response ratios
    - Presence_and_absence_Frequency_of_occurence.R - creates a presence/absence dataset from density_data.csv and calculates the frequency of occurence per island per method for clarification of forest plots
    - MPA_Comp_NMDS.R - nMDS, SIMPER, and ANOSIM analyses on range standardized density data
    - Response_Ratio_calculations.R - Calculates response ratios from island MPA year summaries
