#### MPA Methodology Comp Paper ####
### 5/2021 by Chris Honeyman ###
#Updated w/ Freq of Occurence 5/22
#Diversity Index on PRESENCE ABSENCE DATA (Original Code by Peter Carlson and Conner Jainese)#

# Will begin with merged MPA dataset and convert to presence-absence for this first pass #
# create zero populated wide data to use to calculate frequency of occurrence
# FoO data will be used to inform RR forest plots
# CODE Graveyard: Shannon div and richness script for pres-abs data

library(tidyverse)
library(plyr)
library(reshape2)
library(readxl)
library(tibble)
library(gt)
library(gtsummary)
library(gtExtras)

#setwd on mac
setwd("/Volumes/pisco/ucsb/data/MPA methodology comparison")

#on pc
#setwd("//FILES/pisco/ucsb/data/MPA methodology comparison")


#load density data
den.dat <- read.csv("Data_Exports/density_data.csv")

#to prepare for analysis we'll remove targeted, untargeted, and total columns

pa.dat <- den.dat[, c(1:8, 11:178, 180:191)]

#zero populate

pa.dat[is.na(pa.dat)] = 0

header <- pa.dat[, c(1:8)]


#convert to presence-absence
#remove header
pa.dat <- pa.dat[, c(9:187)]

pa.dat[pa.dat > 0] <- 1 #converts from abundance to P/A across entire dataframe

pa.dat$X <- seq.int(nrow(pa.dat))

pa.dat <- pa.dat[, c(180, 1:179)]
 
#Rejoin header with presence absence dataset

full.pa.dat <- merge(header, pa.dat, by = "X")
full.pa.dat$X <- NULL


#Cast into long form

pa.long <- gather(full.pa.dat, PISCO_code, value, 8:186)

# Trying something here - frequency of occurence by summary table

# pa.wide.test.tab <- pa.dat %>%
  #tbl_cross(row = method,
#            col = island)


# pa.wide.test.tab

#We want to calculate frequency of occurence per tool, so we'll create a new column that is "survey ID"
# combine Date - Year - Site Block Drop columns into 1

#remove MPA designations

full.pa.dat$mpa_designation <- NULL
full.pa.dat$mpa_status <- NULL

#concatenate Date - Site columns

freq.pa.dat <- full.pa.dat %>%
    unite("Survey ID", site_block_drop:survey_date, remove = FALSE)
    
#Split dataframe based on method

ccfrp.pa <- filter(freq.pa.dat, method == "CCFRP")
bruv.pa <- filter(freq.pa.dat, method == "BRUV")
scuba.pa <- filter(freq.pa.dat, method == "SCUBA")

#Now Split each method based by Island

ccfrp.ana<- filter(ccfrp.pa, island == "Anacapa")
ccfrp.sri<- filter(ccfrp.pa, island == "Santa Rosa")


bruv.ana <- filter(bruv.pa, island == "Anacapa")
bruv.sri <- filter(bruv.pa, island == "Santa Rosa")


scuba.ana <- filter(scuba.pa, island == "Anacapa")
scuba.sri <- filter(scuba.pa, island == "Santa Rosa")


#______________________ Calculating Frequency of Occurence___________________#

#_______________________ First CCFRP at Anacapa _______________________#  

#removeheaders
 ccfrp.test.ana <- ccfrp.ana[, c(7:185)]
 
 #test
 
 ccfrp.ana.test <- ccfrp.ana %>%
   tbl_summary()

# obtain Frequency of occurrence for each spp.
# For each species, divide the number of surveys with an observation for that species 
# by the number of surveys at that island

ccfrp.freq.ana <- colMeans(ccfrp.test.ana, na.rm = TRUE)
ccfrp.freq.ana <- as.data.frame(ccfrp.freq.ana)

#add method and island columns for combination
ccfrp.freq.ana$method <- 'CCFRP'
ccfrp.freq.ana$island <- 'Anacapa'


#change column names to match among dataframes

colnames(ccfrp.freq.ana)[which(names(ccfrp.freq.ana) == 'ccfrp.freq.ana')] <- 'freq.dat'

#convert rownames to first column

ccfrp.freq.ana <- tibble::rownames_to_column(ccfrp.freq.ana, "species")

#cast data into wide form 

ccfrp.a <- ccfrp.freq.ana %>%
  spread(species, freq.dat)

#view(ccfrp.a)



#_______________________ CCFRP at Santa Rosa _______________________# 

#removeheaders
ccfrp.test.sri <- ccfrp.sri[, c(7:185)]

# obtain Frequency of occurrence for each spp.

ccfrp.freq.sri <- colMeans(ccfrp.test.sri, na.rm = TRUE)
ccfrp.freq.sri <- as.data.frame(ccfrp.freq.sri)

#add method and island columns for combination
ccfrp.freq.sri$method <- 'CCFRP'
ccfrp.freq.sri$island <- 'Santa Rosa'


#change column names to match among dataframes

colnames(ccfrp.freq.sri)[which(names(ccfrp.freq.sri) == 'ccfrp.freq.sri')] <- 'freq.dat'

#convert rownames to first column

ccfrp.freq.sri <- tibble::rownames_to_column(ccfrp.freq.sri, "species")

#cast data into wide form 

ccfrp.s <- ccfrp.freq.sri %>%
  spread(species, freq.dat)

#view(ccfrp.s)







#_______________________ Next BRUV at Anacapa _______________________#  

#removeheaders
bruv.test.ana <- bruv.ana[, c(7:185)]

# obtain Frequency of occurrence for each spp.

bruv.freq.ana <- colMeans(bruv.test.ana, na.rm = TRUE)
bruv.freq.ana <- as.data.frame(bruv.freq.ana)

#add method and island columns for combination
bruv.freq.ana$method <- 'BRUV'
bruv.freq.ana$island <- 'Anacapa'


#change column names to match among dataframes

colnames(bruv.freq.ana)[which(names(bruv.freq.ana) == 'bruv.freq.ana')] <- 'freq.dat'

#convert rownames to first column

bruv.freq.ana <- tibble::rownames_to_column(bruv.freq.ana, "species")

#cast data into wide form 

bruv.a <- bruv.freq.ana %>%
  spread(species, freq.dat)

#view(bruv.a)





#_______________________ BRUV at Santa Rosa _______________________#  

#removeheaders
bruv.test.sri <- bruv.sri[, c(7:185)]

# obtain Frequency of occurrence for each spp.

bruv.freq.sri <- colMeans(bruv.test.sri, na.rm = TRUE)
bruv.freq.sri <- as.data.frame(bruv.freq.sri)

#add method and island columns for combination
bruv.freq.sri$method <- 'BRUV'
bruv.freq.sri$island <- 'Santa Rosa'


#change column names to match among dataframes

colnames(bruv.freq.sri)[which(names(bruv.freq.sri) == 'bruv.freq.sri')] <- 'freq.dat'

#convert rownames to first column

bruv.freq.sri <- tibble::rownames_to_column(bruv.freq.sri, "species")

#cast data into wide form 

bruv.s <- bruv.freq.sri %>%
  spread(species, freq.dat)

#view(bruv.s)



#_______________________ SCUBA at Anacapa _______________________#  

#removeheaders
scuba.test.ana <- scuba.ana[, c(7:185)]

# obtain Frequency of occurrence for each spp.

scuba.freq.ana <- colMeans(scuba.test.ana, na.rm = TRUE)
scuba.freq.ana <- as.data.frame(scuba.freq.ana)

#add method and island columns for combination
scuba.freq.ana$method <- 'SCUBA'
scuba.freq.ana$island <- 'Anacapa'


#change column names to match among dataframes

colnames(scuba.freq.ana)[which(names(scuba.freq.ana) == 'scuba.freq.ana')] <- 'freq.dat'

#convert rownames to first column

scuba.freq.ana <- tibble::rownames_to_column(scuba.freq.ana, "species")

#cast data into wide form 

scuba.a <- scuba.freq.ana %>%
  spread(species, freq.dat)

#view(scuba.a)






#_______________________ SCUBA at Santa Rosa _______________________#  

#removeheaders
scuba.test.sri <- scuba.sri[, c(7:185)]

# obtain Frequency of occurrence for each spp.

scuba.freq.sri <- colMeans(scuba.test.sri, na.rm = TRUE)
scuba.freq.sri <- as.data.frame(scuba.freq.sri)

#add method and island columns for combination
scuba.freq.sri$method <- 'SCUBA'
scuba.freq.sri$island <- 'Santa Rosa'


#change column names to match among dataframes

colnames(scuba.freq.sri)[which(names(scuba.freq.sri) == 'scuba.freq.sri')] <- 'freq.dat'

#convert rownames to first column

scuba.freq.sri <- tibble::rownames_to_column(scuba.freq.sri, "species")

#cast data into wide form 

scuba.s <- scuba.freq.sri %>%
  spread(species, freq.dat)

#view(scuba.s)

#Done with individual df Freq. occ calculations


#trying to combine by island first for summary table

ana.freq.dat.frames <- list(ccfrp.a, scuba.a, bruv.a)

ana.freq.dat <- rbind.fill(ana.freq.dat.frames, fill = TRUE) 

#subset out spp of interest from SIMPER

# Subset out Species from SIMPER Analysis/RR Plotting

simper.freq.dat.ana <- subset(ana.freq.dat, select = c("method", "den_CPRI", "den_CPUN", "den_EJAC", "den_ELAT", "den_GNIG", 
                                               "den_HROS", "den_HRUB", "den_HSEM", 
                                               "den_OCAL", "den_OPIC", "den_OYT", "den_PCLA", "den_RVAC", "den_SCAU", "den_SMIN",
                                               "den_SMYS", "den_SPAU", "den_SPUL", "den_SROS", "den_SSEM"))

# nevermind Combine all three back into one dataframe
#make all dataframes into a list

freq.dat.frames <- list(ccfrp.a, ccfrp.s, scuba.a, scuba.s, bruv.a, bruv.s)

freq.dat <- rbind.fill(freq.dat.frames, fill = TRUE)

#save as .csv

write_csv(freq.dat, "Data_Exports/Frequency of Occurence/Freq_data.csv")

# Subset out Species from SIMPER Analysis/RR Plotting

simper.freq.dat <- subset(freq.dat, select = c("method", "island", "den_CPRI", "den_CPUN", "den_EJAC", "den_ELAT", "den_GNIG", 
                                               "den_HROS", "den_HRUB", "den_HSEM", 
                                               "den_OCAL", "den_OPIC", "den_OYT", "den_PCLA", "den_RVAC", "den_SCAU", "den_SMIN",
                                               "den_SMYS", "den_SPAU", "den_SPUL", "den_SROS", "den_SSEM"))

#melt into long form for making a table

simp.freq.dat.long <- gather(simper.freq.dat, species, freq.occ, 3:22)

#cast this into wide format

simp.freq.dat.wide <- spread(simp.freq.dat.long, method, freq.occ)



# Let's make this a fancy ol' table


simper.freq.tab <- simp.freq.dat.wide %>%
  group_by(island) %>%
  gt(rowname_col = "species")%>%
  cols_label(
    BRUV = md("**BRUV**"),
    CCFRP = md("**CCFRP**"),
    SCUBA = md("**SCUBA**"))%>%
  tab_stubhead(label = " ")%>%
  tab_header(
    title = md("**Species Frequency of Occurence by Island by Method**")
  )


simper.freq.tab #still need to figure out 2 column formatting

































































##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
#__________ Code Graveyard - presence absence diversity code ____________#
##########################################################################
##########################################################################
##########################################################################
##########################################################################

#Start Diversity Code

all_fish_wider <- pa.long %>% 
  dplyr:: select("PISCO_code", "survey_year", "survey_date", "island", "site_block_drop", "mpa_designation", "value" ) %>%
  
  #change data to intergers
  mutate(value = as.integer(value)) %>%
  
  #make it wide
  pivot_wider(names_from = PISCO_code, values_from = value)

#isolate row names to add back later
all_fish_wider$UniqueID <- paste(all_fish_wider$site_block_drop, all_fish_wider$survey_year, all_fish_wider$survey_date, all_fish_wider$island, all_fish_wider$mpa_designation)

rownames <- all_fish_wider$UniqueID

#remove row names for diversity calculation
all_fish_wider$UniqueID <- NULL
all_fish_wider$survey_date <- NULL
all_fish_wider$site_block_drop <- NULL
all_fish_wider$island <- NULL
all_fish_wider$mpa_designation <- NULL
all_fish_wider$survey_year <-NULL

#all columns need to be numeric for code to work
all_fish_wider <- as.data.frame(all_fish_wider)
all_fish_wider[] <- lapply(all_fish_wider, function(x) as.numeric(as.character(x)))


#Shannon diversity index for all_fish_wider
H_all_fish <-  diversity(all_fish_wider, index = "shannon", MARGIN = 1, 
                         base = exp(1)) 

#create dataframe from H output
dat_H_all_fish <- data.frame("H_all" = H_all_fish)

#reattach rownames (renamed as Drop_ID) for joining to metadata
dat_H_all_fish$UniqueID <- rownames



# Pielou's evenness Carrington out *NOT WORKING*
#J_car_out <- H_car_OUT/log(specnumber(car_OUT_fish_wider))


#----------Species Richness--------#

#spp richness (per drop) by depth
dat_sp_rich<- pa.long %>% 
  filter(value > 0) %>% 
  dplyr::group_by(site_block_drop, mpa_designation, island, survey_year, survey_date, method) %>% 
  dplyr::summarise(count = length(unique(PISCO_code))) %>% 
  dplyr::rename(sp_rich = count)
#___________________________________________________#

#isolate row names for dat_sp_rich to join H and richness

dat_sp_rich$UniqueID <- paste(dat_sp_rich$site_block_drop, dat_sp_rich$survey_year, dat_sp_rich$survey_date, dat_sp_rich$island, dat_sp_rich$mpa_designation)

rownames <- all_fish_wider$UniqueID





#_______join div, richness and meta data______#

#joining H_all to metadata
dat_div <- left_join(dat_H_all_fish, dat_sp_rich )#join by "UniqueID"

dat_div <- dat_div[, c(8, 2:7, 9, 1)]

# dat_div <- left_join(dat_div, meta_data)

dat_div$H_all <- as.numeric(dat_div$H_all)

#Joining, by = c("Drop_ID", "MPA_Status", "Reserve_Name", "Depth", "island_MPA")

#export diversity data

dat_div$island_MPA <- paste(dat_div$island, dat_div$mpa_designation)

write.csv(dat_div, "Data_Exports/MPA_Comp_presence_absence_diversity_data.csv")

#____________________ Stop Div Calculations _____________________#



#_____________________ Start Div Plotting _____________________#

### In Progress 6/10 ##

#Add Island - MPA designation column for plotting

dat_div$island_MPA <- paste(dat_div$island, dat_div$mpa_designation)


#plot shannon diversity by species richness
ggplot(dat_div, aes(x = sp_rich, y = H_all)) +
  geom_point()

#mean species richness by site/MPA
mean_sp_rich <- dat_div %>% 
  group_by(method, island_MPA, island, mpa_designation, survey_year) %>% 
  dplyr::summarise(mean = mean(sp_rich),
                   sd = sd(sp_rich))



#plot mean_sp_rich data
ggplot(data = mean_sp_rich) + 
  geom_col(mapping = aes(x = island_MPA, y = mean, fill = method), position = "dodge")



#Island MPA diversity plot 



div_island_plot <- ggplot(dat_div, aes(method, H_all))+
  geom_boxplot(position = "dodge", aes(fill=mpa_designation), show.legend = FALSE)+
  geom_jitter(width = .15, color = "black", size = 3)+
  facet_wrap(~island_MPA, ncol = 2)+
  xlab("")+
  ylab("Shannon Diversity Index")+
  scale_fill_manual(values = alpha(c("red","blue", "yellow", "green"), .5))+
  theme_solarized_2(
    base_size = 20)

div_island_plot




dat_div %>%
  dplyr:: group_by(island_MPA)%>%
  dplyr:: summarise(stat = shapiro.test(H_all)$statistic,
                    pval = shapiro.test(H_all)$p.value)

#Diversity data is not normally distributed 


hist_plot <- ggplot(data = dat_div, aes(x = H_all, color = island_MPA, fill = method))+
  geom_histogram(binwidth = .5)+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  facet_wrap(~island_MPA)

hist_plot


#wilcox pairwise comparisons 

sr <- subset(dat_div, island == "Santa Rosa")
an <- subset(dat_div, island == "Anacapa")

div_compare <- pairwise.wilcox.test(sr$H_all, sr$mpa_designation,
                                    paired = FALSE, p.adjust.method="none")
div_compare



#linear modeling 
fit_div <- lm(H_all ~ Grid_Hab_H_percent , data = dat_div_hab)
summary(fit_div)
