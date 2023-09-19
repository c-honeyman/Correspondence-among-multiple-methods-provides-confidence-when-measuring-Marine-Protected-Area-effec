### THIS CODE WILL CREATE MPA RESPONSE RATIOS For all species by target status
### ORIGNAL CODE CREATED:  MAY 2021, CHRIS HONEYMAN AND BARBARA SPIECKER

##############################################################################
# Fish Biomass by Target Status
##############################################################################

#setwd on pc

# setwd("//FILES/pisco/ucsb/data/MPA methodology comparison/Data_Exports")

#mac
setwd("/Volumes/pisco/ucsb/data/MPA methodology comparison/Data_Exports")

# Load dataset
# This code uses the ISlandMPAYear Summary Tables as it's starting point

mpa.dat <- read.csv("Summary Tables/CLEANBRUV_Summary_by_Island_MPA_Year.csv")

# Split data into two df: density and biomass
fish_density_target <- mpa.dat[, c(2:7, 10:190)]
fish_biomass_target <- mpa.dat[, c(2:5, 8, 9, 191:371)]

# Transpose biomass df mean by target status
library(tidyr)
fish_biomass_target.1 <- gather(fish_biomass_target, species, biomass, 5:187, factor_key=TRUE)

## Check the dataset if the biomass column has zeros
## If yes, we will need to add a constant to all of the biomasses to avoid undefined ratios (constant = 10% of the grand mean)
# Calculate the biomass grand mean
m_bm <- mean(fish_biomass_target.1$biomass, na.rm = T)

# Calculate 10% of the grand mean
m_bm.1 <- 0.1*m_bm

# Add the constant to all of the biomasses
fish_biomass_target.1$biomass <- m_bm.1+fish_biomass_target.1$biomass


# Run a quick summary of the data structure
library(Rmisc)
bm_data_summary <- summarySE(fish_biomass_target.1, measurevar = "biomass", groupvars = c("method", "survey_year", "island", "mpa_status", "species"), na.rm = T)
bm_data_summary.1 <- bm_data_summary[c("method", "survey_year", "island", "mpa_status", "species", "biomass")]


# Identify missing MPA/reference pairs
bm_data_wide <- spread(bm_data_summary.1, mpa_status, biomass)

# see below for code removed from here #

#TEST Calc rr from data wide
fish_biomass_rr <- bm_data_wide

# Calculate log ratio
# Make sure it is written as log10 not log because "log" is actually natural logarithm (ln) in R
fish_biomass_rr$log_ratio <- log10(fish_biomass_rr$MPA/fish_biomass_rr$REF)

#Save file for Analysis
setwd("//FILES/pisco/ucsb/data/MPA methodology comparison/Data_Exports/Response Ratios")

write.csv(fish_biomass_rr, file = "2_Clean_BRUV_All_Spp_Fish_Biomass_RR.csv")




##############################################################################
# Fish Density by Target Status
##############################################################################

#Perform RR Calculations for Fish Density by Target Status


# Transpose mean by target status
library(tidyr)
fish_density_target.1 <- gather(fish_density_target, species, density, 5:187, factor_key=TRUE)

## Check the dataset if the density column has zeros
## If yes, we will need to add a constant to all of the density to avoid undefined ratios (constant = 10% of the grand mean)
# Calculate the density grand mean
m_den <- mean(fish_density_target.1$density, na.rm = T)

# Calculate 10% of the grand mean
m_den.1 <- 0.1*m_den

# Add the constant to all of the biomasses
fish_density_target.1$density <- m_den.1+fish_density_target.1$density


# Run a quick summary of the data structure
library(Rmisc)
den_data_summary <- summarySE(fish_density_target.1, measurevar = "density", groupvars = c("method", "survey_year", "island", "mpa_status", "species"), na.rm = T)
den_data_summary.1 <- den_data_summary[c("method", "survey_year", "island", "mpa_status", "species", "density")]


# Identify missing MPA/reference pairs
den_data_wide <- spread(den_data_summary.1, mpa_status, density)

# If missing pairs, see below for code removed from here #

#TEST Calc rr from data wide
fish_density_rr <- den_data_wide

# Calculate log ratio
# Make sure it is written as log10 not log because "log" is actually natural logarithm (ln) in R
fish_density_rr$log_ratio <- log10(fish_density_rr$MPA/fish_density_rr$REF)

#Save file for Analysis
setwd("//FILES/pisco/ucsb/data/MPA methodology comparison/Data_Exports/Response Ratios")

write.csv(fish_density_rr, file = "2_Clean_BRUV_All_spp_Fish_Density_RR.csv")


######## END RR Calculations ########
