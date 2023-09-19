# MPA Methodology Comparisons NMDS
# Pulled from PC NMDS Code 3/2021
# Updated by Chris Honeyman 2/2023

### Can be Run on Standardized or non-standardized density data ###


library(lubridate)
library(ggplot2)
library(plyr)
library(reshape2)
library(tidyverse)
library(vegan)
library(grid)
library(readxl)
library(gt)
library(hrbrthemes)
library(gridExtra)
library(cowplot)
library(DataCombine)
library(ggpubr)
library(gt)
library(webshot)

rm(list=ls())

#set wd on pc
# setwd("//FILES/pisco/ucsb/data/MPA methodology comparison")

#on mac
setwd("/Volumes/pisco/ucsb/data/MPA methodology comparison")



#_______________________________________________________________________#
#_______________________________________________________________________#
#__________________________NMDS by Island_______________________________#
#_______________________________________________________________________#
#_______________________________________________________________________#
# ANACAPA FIRST #

#clear environment and reimport density data

#Standardized dataset
den_dat <- read.csv("Data_Exports/Standardized Datasets/Clean_BRUV_Range_standardized_density_data.csv")

#Non standardized dataset 
#den_dat <- read.csv("Data_Exports/density_data.csv")

den_dat$X <- NULL
den_dat$site_block_drop <- NULL
den_dat$survey_date <- NULL
den_dat$mpa_designation <- NULL


# We need to summarize by Mpa_sttatus and remove all spp not seen at all

#summarize by mpa_status

dat_wide = den_dat %>% 
  group_by(method, island, survey_year, mpa_status) %>% 
  summarise_all(.funs = mean)

#remove any spp not seen by any method

dat_wide <- dat_wide[, colSums(dat_wide != 0) > 0]

#also remove YOY, schooling silver fishes, and grouped categories

dat_wide$den_RYOY <- NULL
dat_wide$den_SMYSYOY <- NULL
dat_wide$den_BFREYOY <- NULL
dat_wide$den_CPUNYOY <- NULL
dat_wide$den_PCLAYOY <- NULL
dat_wide$den_SPAUYOY <- NULL
dat_wide$den_SPULYOY <- NULL
dat_wide$den_CITHSPP <- NULL
dat_wide$den_KGB <- NULL
dat_wide$den_OYB <- NULL
dat_wide$den_RFSP <- NULL
dat_wide$den_OYT <- NULL
dat_wide$den_CITHSPP <- NULL
dat_wide$den_SEBSPP <- NULL
dat_wide$den_SCOMSPP <- NULL
dat_wide$den_UnID.SMYS <- NULL
dat_wide$den_UNK <- NULL
dat_wide$den_ATHE <- NULL
dat_wide$den_TSYM <- NULL
dat_wide$den_CSOR <- NULL
dat_wide$den_SSAG <- NULL
dat_wide$den_COTSP <- NULL
dat_wide$den_CITH <- NULL
dat_wide$den_RNIC <- NULL
dat_wide$den_RONSP <- NULL
dat_wide$den_SJAP <- NULL
dat_wide$den_Total <- NULL
dat_wide$den_targeted <- NULL
dat_wide$den_nontargeted <- NULL
dat_wide$den_MMOL <- NULL



# Remove San Miguel Data and Santa Rosa Data to make a dataframe for Anacapa

dat_wide <- subset(dat_wide, dat_wide$island != "San Miguel")
ana_dat <- subset(dat_wide, dat_wide$island != "Santa Rosa")


#remove spp not seen at Anacapa

ana_dat <- ana_dat[, colSums(ana_dat != 0) > 0]

#if working with non standardized dataset
#Zero Populate dataset if not already
#ana_dat[is.na(ana_dat)] = 0


#Create Site Lookup table
ana_dat$ID <- as.character(1:nrow(ana_dat))
nmds_sites_ana <- ana_dat[ ,c("method", "survey_year", "island", "mpa_status", "ID")]# select ID variables for site table 
fish_dat_wide_ana <- ana_dat[ ,-which(names(ana_dat) %in%c("method", "survey_year", "island", "site_block_drop", "mpa_status", "ID"))] #select spp columns only

colnames(fish_dat_wide_ana)




#NMDS
NMDS_raw_ana <- metaMDS(fish_dat_wide_ana, 
                    distance = "bray",
                    k=2,
                    autotransform = FALSE,
                    trymax = 9999,
                    noshare = 0.1)


plot(NMDS_raw_ana, type = 't')
stressplot(NMDS_raw_ana)
NMDS_raw_ana$stress #0.09

#____________Get the data out of the model____________#

data.scores.ana <- as.data.frame(scores(NMDS_raw_ana, "sites"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores.ana$ID <- rownames(data.scores.ana)  # create a column of site names, from the rownames of data.scores
head(data.scores.ana) 

#look at the data

species.scores.ana <- as.data.frame(scores(NMDS_raw_ana, "species")) 
species.scores.ana$species <- rownames(species.scores.ana)  # create a column of species, from the rownames of species.scores
head(species.scores.ana)  #look at the data


#join nmds Data with Meta data
data.scores.ana_2 <- left_join(nmds_sites_ana,data.scores.ana) #join by "ID"



#Convex hulls
chull_dat_ana_2 <- data.scores.ana_2 %>%
  group_by(method) %>%
  slice(chull(NMDS1, NMDS2))

method.cols.ana <- c("#ff00ff", "#ccbfa5", "#088163")

#_______________ PLOT NMDS Methods ___________________#

nmds_fig_ana <- ggplot()+
  geom_point(data=data.scores.ana_2,
             aes(x=NMDS1,y=NMDS2,shape=mpa_status, fill=NA),size=5.0, 
             alpha = .75, show.legend = TRUE)+ 
  scale_shape_manual(values=c(15, 16, 17, 8))+
  geom_polygon(data = chull_dat_ana_2,
               aes(x= NMDS1, y= NMDS2, group = method, fill=method),
               alpha = 0.50, 
               size = 0.1)+
  # scale_color_manual(values = cbbPalette)+
  # scale_fill_manual(values = cbbPalette)+

  
  #BE SURE TO EDIT TEXT BASED ON DATA AND RESULTS
  geom_text(aes(x=1.2, y=0.75, label="2D stress 0.09"))+
  guides(fill = "none")+
  # scale_color_brewer(palette = "Accent")+
  ylim(-1.0, 0.8)+ #-1.0, 1.0
  xlim(-1.5, 1.5)+ #-1.5, 1.5
  theme_bw()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=20),
        legend.title = element_blank())+
  guides(fill = guide_legend(override.aes = list(colour = method.cols.ana, pch=0))) # add

nmds_fig_ana

#___________ Plotting Anacapa Species Scores ___________________#

#We want to clean this plot up so run SIMPER first

#Top ten from each of the three SIMPER Comparisons

#den_CPRI
#den_SGUT
#den_CPUN
#den_SGIG
#den_HFRA
#den_PCLA
#den_SPUL
#den_HROS
#den_HSEM
#den_SROS
#den_HRUB
#den_GNIG
#den_EJAC
#den_STRE
#den_BRAY
#den_SCON
#den_MCAL

#Filter out top 10 species from each tool comparison ANACAPA SIMPER results for plotting

species.scores.ana.edit <- species.scores.ana %>%
  filter(species %in% c("den_CPRI",
                        "den_SGUT",
                        "den_CPUN",
                        "den_SGIG",
                        "den_HFRA",
                        "den_PCLA",
                        "den_SPUL",
                        "den_HROS",
                        "den_HSEM",
                        "den_SROS",
                        "den_HRUB",
                        "den_GNIG",
                        "den_EJAC",
                        "den_STRE",
                        "den_BRAY",
                        "den_SCON",
                        "den_MCAL")) 

#Replace codes with Species names

species.scores.ana.edit$species[species.scores.ana.edit$species=='den_CPRI']<- 'C. princeps'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_CPUN']<- 'C. punctipinnis'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_EJAC']<- 'E. jacksoni'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_GNIG']<- 'G. nigricans'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_HRUB']<- 'H. rubicundus'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_HSEM']<- 'H. semicinctus'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_SGUT']<- 'S. guttata'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_PCLA']<- 'P. clathratus'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_HFRA']<- 'H. francisci'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_STRE']<- 'S. serriceps'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_SPUL']<- 'S. pulcher'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_SROS']<- 'S. rosaceus'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_MCAL']<- 'M. californiensis'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_SCON']<- 'S. constellatus'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_HROS']<- 'H. rostratus'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_BRAY']<- 'M. californica'
species.scores.ana.edit$species[species.scores.ana.edit$species=='den_SGIG']<- 'S. gigas'

#export spp score to edit values for clarity

write_csv(species.scores.ana.edit, "Data_Exports/Diversity Data/NMDS_spp_scores_ana.csv")

#Open in Excel and edit x-y coords to improve clarity; may require trial and error
#Reimport edited data before plotting PAY ATTENTION TO NAMING

ana_spp_scores <- read_csv("Data_Exports/Diversity Data/NMDS_spp_scores_ana_edited.csv")


#Plot Spp Scores


nmds_fig_spp_ana <- ggplot()+
  
  geom_text(data=ana_spp_scores,
            aes(x=(NMDS1),y=(NMDS2),
                label=species,
                fontface = "bold"), size = 6) + 
  
  geom_segment(data = ana_spp_scores, aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),colour="black",alpha = 0.2, inherit_aes=TRUE)+
  # scale_color_manual(values = cbbPalette)+
  # scale_fill_manual(values = cbbPalette)+
  
  guides(fill = "none")+
  # scale_color_brewer(palette = "Accent")+
  ylim(-0.8, 0.6)+ #-, 1.4
  xlim(-1.5, 1.75)+ #-98  1.5
  theme_bw()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank())+
  guides(fill = guide_legend(override.aes = list(colour = method.cols.ana, pch=0))) # add

nmds_fig_spp_ana


##_____________________ Mush Plots together _____________________ #

#we will separate out the legend from the ndms_fig_site and add it into grid as it's own plot

legend <- get_legend(nmds_fig_ana)

#remove legend from nmds_fig_site

nmds_fig_ana <- nmds_fig_ana + 
  theme(legend.position = "none")

#create a blank plot

bp.ana <- ggplot() + geom_blank(aes(1, 1)) +
  cowplot::theme_nothing()

#arrange into multi panel plot

grid.arrange(nmds_fig_ana, nmds_fig_spp_ana, legend, bp.ana,
             ncol=2, nrow=2,
             widths = c(2.5, 2.5), heights = c(2.5,.75))

#SAVE PLOT; WIDTH 1300 px

#_______________________________________________________________#
#______________ Analysis of Similarity ANA ONLY_________________#
#_______________________________________________________________#
##### https://jkzorz.github.io/2019/06/11/ANOSIM-test.html #####


#make community matrix - extract columns with abundance information, turn data frame into matrix
# MUST MAKE MATRIX NUMERIC

ana_com = ana_dat[,5:58] #removing "ID" column
ana_m_com = as.matrix(ana_com)


ana_m_com <- matrix(as.numeric(ana_m_com),    # Convert to numeric matrix
                ncol = ncol(ana_m_com))
ana_m_com


#Run ANOSIM 

ana_ano = anosim(x = ana_m_com, grouping = ana_dat$method, distance = "bray", permutations = 9999)
ana_ano

plot(ana_ano)

# ANOSIM statistic R: 0.872 
# Significance: 1e-04 

# Add column names in for SIMPER 

species.names <- colnames(ana_dat)
species.names <- species.names[5:58]

colnames(ana_m_com) <- species.names

#Run SIMPER

ana.similarity <- simper(ana_m_com, ana_dat$method, permutations = 999)


#coerce to list of dataframes
ana.sim.sum <- summary(ana.similarity)

ana.sim.sum

#pull individual dataframes from list

list2env(ana.sim.sum, envir = .GlobalEnv)



#write.csv(BRUV_CCFRP, file = "Data_Exports/Final Datasets/SIMPER Comparisons/ANA/Anacapa_SIMPER_BRUV_CCFRP.csv")
#write.csv(BRUV_SCUBA, file = "Data_Exports/Final Datasets/SIMPER Comparisons/ANA/Anacapa_SIMPER_BRUV_SCUBA.csv")
#write.csv(CCFRP_SCUBA, file = "Data_Exports/Final Datasets/SIMPER Comparisons/ANA/Anacapa_SIMPER_CCFRP_SCUBA.csv")



# Create Table for each SIMPER Comparison

#____________ First BRUV - CCFRP ___________________ #
# Chagne Rownames from den_ to species names for top 10
#BEFORE Running this, double check species codes are still in the correct order

row.names(BRUV_CCFRP)[1] <- "C. princeps"
row.names(BRUV_CCFRP)[2] <- "P. clathratus"
row.names(BRUV_CCFRP)[3] <- "S. pulcher"
row.names(BRUV_CCFRP)[4] <- "S. guttata"
row.names(BRUV_CCFRP)[5] <- "S. serriceps"
row.names(BRUV_CCFRP)[6] <- "S. gigas"
row.names(BRUV_CCFRP)[7] <- "S. rosaceus"
row.names(BRUV_CCFRP)[8] <- "C. punctipinnis"
row.names(BRUV_CCFRP)[9] <- "M. californica"
row.names(BRUV_CCFRP)[10] <- "S. constellatus"


#RUN THIS before changing any names

BRUV_CCFRP_gt <- BRUV_CCFRP %>%
  
  head(10) %>%
  
  gt(rownames_to_stub = TRUE) %>%
  
  cols_hide(columns = c(average, sd, ratio, ava, avb, p)) %>%
  
  cols_label(cumsum = "Cumulative Contribution") %>%
  
  cols_align(align = "center",
             columns = cumsum) %>%
  
  tab_header(title = "SIMPER Comparison BRUV - CCFRP at Anacapa Island",
             subtitle = "Top ten drivers ordered by cumulative contribution to dissimilarity") #%>%
  
#  tab_footnote(
#    footnote = "p-values <0.05 are significant",
#    locations = cells_body(
#      columns = "p",
#      rows = p <0.05))

  

BRUV_CCFRP_gt

#Save table


# _______________ Now BRUV_SCUBA _________________ #

# Change Rownames from den_ to species names
#BEFORE Running this, double check species codes are still in the correct order

row.names(BRUV_SCUBA)[1] <- "H. rubicundus"
row.names(BRUV_SCUBA)[2] <- "P. clathratus"
row.names(BRUV_SCUBA)[3] <- "S. pulcher"
row.names(BRUV_SCUBA)[4] <- "G. nigricans"
row.names(BRUV_SCUBA)[5] <- "C. punctipinnis"
row.names(BRUV_SCUBA)[6] <- "H. semicinctus"
row.names(BRUV_SCUBA)[7] <- "E. jacksoni"
row.names(BRUV_SCUBA)[8] <- "M. californiensis"
row.names(BRUV_SCUBA)[9] <- "H. rostratus"
row.names(BRUV_SCUBA)[10] <- "H. francisci"

#RUN THIS before changing any names

BRUV_SCUBA_gt <- BRUV_SCUBA %>%
  
  head(10) %>%
  
  gt(rownames_to_stub = TRUE) %>%
  
  cols_hide(columns = c(average, sd, ratio, ava, avb, p)) %>%
  
  cols_label(cumsum = "Cumulative Contribution") %>%
  
  cols_align(align = "center",
             columns = cumsum) %>%
  
  tab_header(title = "SIMPER Comparison BRUV - SCUBA at Anacapa Island",
             subtitle = "Top ten drivers ordered by cumulative contribution to dissimilarity") #%>%



BRUV_SCUBA_gt

# Save Table



#_______________________ CCFRP - SCUBA _______________________________ #
# Chagne Rownames from den_ to species names
#BEFORE RUNNING THIS Run the table with codes to make sure species order is the same

row.names(CCFRP_SCUBA)[1] <- "H. rubicundus"
row.names(CCFRP_SCUBA)[2] <- "P. clathratus"
row.names(CCFRP_SCUBA)[3] <- "S. pulcher"
row.names(CCFRP_SCUBA)[4] <- "C. princeps"
row.names(CCFRP_SCUBA)[5] <- "G. nigricans"
row.names(CCFRP_SCUBA)[6] <- "H. semicinctus"
row.names(CCFRP_SCUBA)[7] <- "C. punctipinnis"
row.names(CCFRP_SCUBA)[8] <- "E. jacksoni"
row.names(CCFRP_SCUBA)[9] <- "H. rosaceus"
row.names(CCFRP_SCUBA)[10] <- "M. californiensis"

# RUN THIS before changing names above 

CCFRP_SCUBA_gt <- CCFRP_SCUBA %>%
  
  head(10) %>%
  
  gt(rownames_to_stub = TRUE) %>%
  
  cols_hide(columns = c(average, sd, ratio, ava, avb, p)) %>%
  
  cols_label(cumsum = "Cumulative Contribution") %>%
  
  cols_align(align = "center",
             columns = cumsum) %>%
  
  tab_header(title = "SIMPER Comparison CCFRP - SCUBA at Anacapa Island",
             subtitle = "Top ten drivers ordered by cumulative contribution to dissimilarity") #%>%


CCFRP_SCUBA_gt

# Save Table



#Top ten drivers for each comparison compiled into one list - 17 TOTAL 8/21/22

#den_CPRI
#den_SGUT
#den_CPUN
#den_SGIG
#den_HFRA
#den_PCLA
#den_SPUL
#den_HROS
#den_HSEM
#den_SROS
#den_HRUB
#den_GNIG
#den_EJAC
#den_STRE
#den_BRAY
#den_SCON
#den_MCAL




#_______________________________________________________________#
#_______________________________________________________________#
#_______________________________________________________________#

































#_______________________________________________________________#
#_______________________________________________________________#
#_______________________________________________________________#

# Now Carrington #

#clear environment and reimport density data

#Standardized dataset
#den_dat <- read.csv("Data_Exports/Standardized Datasets/Clean_BRUV_Range_standardized_density_data.csv")

#Non standardized dataset 
#den_dat <- read.csv("Data_Exports/density_data.csv")

#den_dat$X <- NULL
#den_dat$site_block_drop <- NULL
#den_dat$survey_date <- NULL
#den_dat$mpa_designation <- NULL


# We need to summarize by Mpa_sttatus and remove all spp not seen at all

#summarize by mpa_status

#dat_wide = den_dat %>% 
#  group_by(method, island, survey_year, mpa_status) %>% 
#  summarise_all(.funs = mean)

#remove any spp not seen by any method

#dat_wide <- dat_wide[, colSums(dat_wide != 0) > 0]

#also remove YOY, schooling silver fishes, and grouped categories

#dat_wide$den_RYOY <- NULL
#dat_wide$den_SMYSYOY <- NULL
#dat_wide$den_BFREYOY <- NULL
#dat_wide$den_CPUNYOY <- NULL
#dat_wide$den_PCLAYOY <- NULL
#dat_wide$den_SPAUYOY <- NULL
#dat_wide$den_SPULYOY <- NULL
#dat_wide$den_CITHSPP <- NULL
#dat_wide$den_KGB <- NULL
#dat_wide$den_OYB <- NULL
#dat_wide$den_RFSP <- NULL
#dat_wide$den_OYT <- NULL
#dat_wide$den_CITHSPP <- NULL
#dat_wide$den_SEBSPP <- NULL
#dat_wide$den_SCOMSPP <- NULL
#dat_wide$den_UnID.SMYS <- NULL
#dat_wide$den_UNK <- NULL
#dat_wide$den_ATHE <- NULL
#dat_wide$den_TSYM <- NULL
#dat_wide$den_CSOR <- NULL
#dat_wide$den_SSAG <- NULL
#dat_wide$den_COTSP <- NULL
#dat_wide$den_CITH <- NULL
#dat_wide$den_RNIC <- NULL
#dat_wide$den_RONSP <- NULL
#dat_wide$den_SJAP <- NULL
#dat_wide$den_Total <- NULL
#dat_wide$den_targeted <- NULL
#dat_wide$den_nontargeted <- NULL
#dat_wide$den_MMOL <- NULL
#dat_wide$den_SYNG <- NULL



# Remove San Miguel Data and Santa Rosa Data to make a dataframe for Anacapa

#dat_wide <- subset(dat_wide, dat_wide$island != "San Miguel")

rosa_dat <- subset(dat_wide, dat_wide$island != "Anacapa")


#if working with non-standardized datase need to zero Populate dataset

#rosa_dat[is.na(rosa_dat)] = 0
#rosa_dat$den_targeted <- NULL
#rosa_dat$den_nontargeted <- NULL
#rosa_dat$den_total <- NULL
#rosa_dat$den_Total <- NULL

#remove spp not seen at SRI

rosa_dat <- rosa_dat[, colSums(rosa_dat != 0) > 0]


#Create Site Lookup table
rosa_dat$ID <- as.character(1:nrow(rosa_dat))
nmds_sites_rosa <- rosa_dat[ ,c("method", "survey_year", "island", "mpa_status", "ID")]# select ID variables for site table 
fish_dat_wide_rosa <- rosa_dat[ ,-which(names(rosa_dat) %in%c("method", "survey_year", "island", "site_block_drop", "mpa_status", "ID"))] #select spp columns only

colnames(fish_dat_wide_rosa)




#NMDS
NMDS_raw_rosa <- metaMDS(fish_dat_wide_rosa, 
                        distance = "bray",
                        k=2,
                        autotransform = FALSE,
                        trymax = 9999,
                        noshare = 0.1)


plot(NMDS_raw_rosa, type = 't')
stressplot(NMDS_raw_rosa)
NMDS_raw_rosa$stress #0.076

#____________Get the data out of the model____________#

data.scores.rosa <- as.data.frame(scores(NMDS_raw_rosa, "sites"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores.rosa$ID <- rownames(data.scores.rosa)  # create a column of site names, from the rownames of data.scores
head(data.scores.rosa) 

#look at the data

species.scores.rosa <- as.data.frame(scores(NMDS_raw_rosa, "species")) 
species.scores.rosa$species <- rownames(species.scores.rosa)  # create a column of species, from the rownames of species.scores
head(species.scores.rosa)  #look at the data


#join nmds Data with Meta data
data.scores.rosa_2 <- left_join(nmds_sites_rosa,data.scores.rosa) #join by "ID"



#Convex hulls
chull_dat_rosa_2 <- data.scores.rosa_2 %>%
  group_by(method) %>%
  slice(chull(NMDS1, NMDS2))

method.cols.rosa <- c("#ff00ff", "#ccbfa5", "#088163")

# Plot NMDS

nmds_fig_rosa <- ggplot()+
  geom_point(data=data.scores.rosa_2,
             aes(x=NMDS1,y=NMDS2,shape=mpa_status, fill=NA),size=5.0, 
             alpha = .75, show.legend = TRUE)+ 
  scale_shape_manual(values=c(15, 16, 17, 8))+
  geom_polygon(data = chull_dat_rosa_2,
               aes(x= NMDS1, y= NMDS2, group = method, fill=method),
               alpha = 0.50, 
               size = 0.1)+
  # scale_color_manual(values = cbbPalette)+
  # scale_fill_manual(values = cbbPalette)+
  geom_text(aes(x=0.95, y=0.65, label="2D stress 0.076"))+
  guides(fill = "none")+
  # scale_color_brewer(palette = "Accent")+
  ylim(-0.75, 0.75)+ #-0.75, 0.75
  xlim(-0.75, 1.1)+ #-0.75  1.1
  theme_bw()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=20),
        legend.title = element_blank())+
  guides(fill = guide_legend(override.aes = list(colour = method.cols.rosa, pch=0))) # add

nmds_fig_rosa

#___________ Plotting Santa Rosa Island Species Scores ___________________#

#We want to clean this plot up using top ten from each SIMPER comparison

#den_CPUN
#den_SMYS
#den_SCAU
#den_CPRI
#den_SMIN
#den_OCAL
#den_SPUL
#den_RVAC
#den_OPIC
#den_OYT
#den_ELAT
#den_PCLA
#den_EJAC

#Filter out top 10 species from each tool comparison ROSA SIMPER results for plotting

species.scores.rosa.edit <- species.scores.rosa %>%
  filter(species %in% c("den_SCAU",
    "den_SMYS",
    "den_CPRI",
    "den_OPIC",
    "den_SATR",
    "den_SCAR",
    "den_OELO",
    "den_SPUL",
    "den_SMAR",
    "den_SMIN",
    "den_ELAT",
    "den_RVAC",
    "den_RTOX",
    "den_SCHR")) 

#Replace codes with Species names

species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_SCAU']<- 'S. caurinus'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_SMYS']<- 'S. mystinus'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_CPRI']<- 'C. princeps'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_OPIC']<- 'O. pictus'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_SATR']<- 'S. atrovirens'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_SCAR']<- 'S. carnatus'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_OELO']<- 'O. elongatus'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_SPUL']<- 'S. pulcher'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_SMAR']<- 'S. marmoratus'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_SMIN']<- 'S. miniatus'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_ELAT']<- 'E. lateralis'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_RVAC']<- 'R. vacca'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_RTOX']<- 'R. toxotes'
species.scores.rosa.edit$species[species.scores.rosa.edit$species=='den_SCHR']<- 'S. chrysomelas'

#export spp score to edit values for clarity

write_csv(species.scores.rosa.edit, "Data_Exports/Diversity Data/NMDS_spp_scores_rosa.csv")

#Open in Excel and edit x-y coords to improve clarity; may require trial and error
#Reimport edited data before plotting PAY ATTENTION TO NAMING

rosa_spp_scores <- read_csv("Data_Exports/Diversity Data/NMDS_spp_scores_rosa_edited.csv")



#Plot spp scores

nmds_fig_spp_rosa <- ggplot()+
  geom_text(data=rosa_spp_scores,
            aes(x=(NMDS1),y=(NMDS2),
                label=species,
                fontface = "bold"), size = 6)+
  
  geom_segment(data = rosa_spp_scores, aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),colour="black", alpha = 0.2, inherit_aes=TRUE)+
  # scale_color_manual(values = cbbPalette)+
  # scale_fill_manual(values = cbbPalette)+

  guides(fill = "none")+
  # scale_color_brewer(palette = "Accent")+
  ylim(-0.75, 0.75)+ #-1.6, 1.4
  xlim(-0.85, 1.1)+ #-98  1.5
  theme_bw()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank())+
  guides(fill = guide_legend(override.aes = list(colour = method.cols.rosa, pch=0))) # add

nmds_fig_spp_rosa


##_____________________ Mush Plots together _____________________ #

#we will separate out the legend from the nmds_fig_site and add it into grid as it's own plot
# Not working 4/26

legend <- get_legend(nmds_fig_rosa)

#remove legend from nmds_fig_site

nmds_fig_rosa <- nmds_fig_rosa + 
  theme(legend.position = "none")

#create a blank plot

bp.rosa <- ggplot() + geom_blank(aes(1, 1)) +
  cowplot::theme_nothing()

#arrange into multi panel plot

rosa.plot.all <- grid.arrange(nmds_fig_rosa, nmds_fig_spp_rosa, legend, bp.rosa,
             ncol=2, nrow=2, 
             widths = c(2.5, 2.5), heights = c(2.5, 0.75))

rosa.plot.all

#save plot! 1300 x 700 for now


#_______________________________________________________________#
#______________ Analysis of Similarity Rosa ONLY_________________#
#_______________________________________________________________#
##### https://jkzorz.github.io/2019/06/11/ANOSIM-test.html #####


#make community matrix - extract columns with abundance information, turn data frame into matrix
# MUST MAKE MATRIX NUMERIC

sri_com = rosa_dat[,5:56] # Remove "ID" column
sri_m_com = as.matrix(sri_com)


sri_m_com <- matrix(as.numeric(sri_m_com),    # Convert to numeric matrix
                    ncol = ncol(sri_m_com))



sri_m_com


#Remove Empty Rows 
## m_com <- m_com[rowSums(m_com[])>0,]



sri_ano = anosim(x = sri_m_com, grouping = rosa_dat$method, distance = "bray", permutations = 9999)
sri_ano

plot(sri_ano)


#ANOSIM statistic R: 0.92
#Significance: 1e-04 

#Permutation: free
#Number of permutations: 9999


# Add column names in for SIMPER 

species.names <- colnames(rosa_dat)
species.names <- species.names[5:56]

colnames(sri_m_com) <- species.names

#Run SIMPER

sri.similarity <- simper(sri_m_com, rosa_dat$method, permutations = 999)



sri.sim.sum <- summary(sri.similarity)
sri.sim.sum

#Top ten drivers for each comparison

#pull individual dataframes from list

list2env(sri.sim.sum, envir = .GlobalEnv)



#write.csv(BRUV_CCFRP, file = "Data_Exports/Final Datasets/SIMPER Comparisons/SRI/Rosa_SIMPER_BRUV_CCFRP.csv")
#write.csv(BRUV_SCUBA, file = "Data_Exports/Final Datasets/SIMPER Comparisons/SRI/Rosa_SIMPER_BRUV_SCUBA.csv")
#write.csv(CCFRP_SCUBA, file = "Data_Exports/Final Datasets/SIMPER Comparisons/SRI/Rosa_SIMPER_CCFRP_SCUBA.csv")

# Turn each into a table for publication

#______________________ BRUV_SCUBA first________________________ #
# Change codes to scientific names for top ten drivers
#BE SURE TO RUN the table with codes first and edit name changes as needed if running with new data

row.names(BRUV_CCFRP)[1] <- "S. caurinus"
row.names(BRUV_CCFRP)[2] <- "S. mystinus"
row.names(BRUV_CCFRP)[3] <- "C. princeps"
row.names(BRUV_CCFRP)[4] <- "O. pictus"
row.names(BRUV_CCFRP)[5] <- "S. atrovirens"
row.names(BRUV_CCFRP)[6] <- "S. carnatus"
row.names(BRUV_CCFRP)[7] <- "O. elongatus"
row.names(BRUV_CCFRP)[8] <- "S. pulcher"
row.names(BRUV_CCFRP)[9] <- "S. marmoratus"
row.names(BRUV_CCFRP)[10] <- "S. miniatus"

#RUN THIS BEFORE CHANGING ANY NAMES

SRI_BRUV_CCFRP_gt <- BRUV_CCFRP %>%
  
  head(10) %>%
  
  gt(rownames_to_stub = TRUE) %>%
  
  cols_label(ava = "average BRUV",
             avb = "average CCFRP",
             cumsum = "Cumulative Contribution",
             p = "p-value") %>%
  
  cols_hide(columns = c(average, sd, ratio, ava, avb, p)) %>%
  
  cols_align(align = "center",
             columns = c(average, sd, ratio, ava, avb, cumsum, p)) %>%
  
  tab_header(title = "SIMPER Comparison BRUV - CCFRP Santa Rosa Island",
             subtitle = "Top ten drivers ordered by cumulative contribution") 

SRI_BRUV_CCFRP_gt

# Save Table


#__________________________ BRUV_SCUBA ___________________________#
row.names(BRUV_SCUBA)[1] <- "E. lateralis"
row.names(BRUV_SCUBA)[2] <- "S. atrovirens"
row.names(BRUV_SCUBA)[3] <- "O. elongatus"
row.names(BRUV_SCUBA)[4] <- "S. pulcher"
row.names(BRUV_SCUBA)[5] <- "R. vacca"
row.names(BRUV_SCUBA)[6] <- "O. pictus"
row.names(BRUV_SCUBA)[7] <- "O. Californica"
row.names(BRUV_SCUBA)[8] <- "R. toxotes"
row.names(BRUV_SCUBA)[9] <- "S. chrysomelas"
row.names(BRUV_SCUBA)[10] <- "S. carnatus"

#BE SURE TO RUN THIS AND CHECK FOR CODE CHANGES BEFORE EDITING SPP NAMES

SRI_BRUV_SCUBA_gt <- BRUV_SCUBA %>%
  
  head(10) %>%
  
  gt(rownames_to_stub = TRUE) %>%
  
  cols_label(ava = "average BRUV",
             avb = "average CCFRP",
             cumsum = "Cumulative Contribution",
             p = "p-value") %>%
  
  cols_hide(columns = c(average, sd, ratio, ava, avb, p)) %>%
  
  cols_align(align = "center",
             columns = c(average, sd, ratio, ava, avb, cumsum, p)) %>%
  
  tab_header(title = "SIMPER Comparison BRUV - SCUBA Santa Rosa Island",
             subtitle = "Top ten drivers ordered by cumulative contribution") 
  



SRI_BRUV_SCUBA_gt

#Save Table

#_____________________ CCFRP_SCUBA _______________________#
rownames(CCFRP_SCUBA)[1] <- "O. pictus"
rownames(CCFRP_SCUBA)[2] <- "E. lateralis"
rownames(CCFRP_SCUBA)[3] <- "S. caurinus"  
rownames(CCFRP_SCUBA)[4] <- "R. vacca"
rownames(CCFRP_SCUBA)[5] <- "S. pulcher"
rownames(CCFRP_SCUBA)[6] <- "O. elongatus"
rownames(CCFRP_SCUBA)[7] <- "S. atrovirens"
rownames(CCFRP_SCUBA)[8] <- "O. californica"
rownames(CCFRP_SCUBA)[9] <- "C. princeps"
rownames(CCFRP_SCUBA)[10] <- "R. toxotes"
  
#BE sure to run before editing spp names

SRI_CCFRP_SCUBA_gt <- CCFRP_SCUBA %>%
  
  head(10) %>%
  
  gt(rownames_to_stub = TRUE) %>%
  
  cols_label(cumsum = "Cumulative Contribution") %>%
  
  cols_hide(columns = c(average, sd, ratio, ava, avb, p)) %>%
  
  cols_align(align = "center",
             columns = c(cumsum)) %>%
  
  tab_header(title = "SIMPER Comparison CCFRP - SCUBA Santa Rosa Island",
             subtitle = "Top ten drivers ordered by cumulative contribution") 




SRI_CCFRP_SCUBA_gt



#Top ten drivers for each comparison COMPILED INTO 1 LIST - 8/21/22

#UPDATE IF RERUNNING ANALYSIS

#den_SCAU
#den_SMYS
#den_CPRI
#den_OPIC
#den_SATR
#den_SCAR
#den_OELO
#den_SPUL
#den_SMAR
#den_SMIN
#den_ELAT
#den_RVAC
#den_RTOX
#den_SCHR

### ________________________________________________________________________ ###
### ____________________________ Combine ANA and SRI into Plot!! ___________________________ ###
### ________________________________________________________________________ ###


#we will separate out the legend from the nmds_fig_site and add it into grid as it's own plot
# Not working 4/26

#remove legend from nmds_fig_site

nmds_fig_rosa <- nmds_fig_rosa + 
  theme(legend.position = "none")

nmds_fig_ana <- nmds_fig_ana + 
  theme(legend.position = "none")

#create a blank plot

bp.all <- ggplot() + geom_blank(aes(2, 3)) +
  cowplot::theme_nothing()

#arrange into multi panel plot

grid.arrange(nmds_fig_ana, nmds_fig_spp_ana, nmds_fig_rosa, nmds_fig_spp_rosa, legend, bp.all, 
             ncol = 2, nrow = 3,
             widths = c(2.5, 2.5), heights = c(2.5, 2.5, 0.75))

#save plot! 1300 x 700 for now




### ________________________________________________________________________ ###
### ____________________________ END NMDS Script ___________________________ ###
### ________________________________________________________________________ ###




































####________________________________________________________________________####
####________________________________________________________________________####
####____________________________ CODE GRAVEYARD_____________________________####
####________________________________________________________________________####
####________________________________________________________________________####




####_____________________________ NMDS BOTH SITES___________________________####

#pick data in 

#Standardized Density Data
den_dat <- read.csv("Data_Exports/Standardized Datasets/Clean_BRUV_Range_standardized_density_data.csv")

#Non-Standardized Density Data
# den_dat <- read.csv("Data_Exports/density_data.csv")

den_dat$X <- NULL
den_dat$site_block_drop <- NULL
den_dat$survey_date <- NULL

#Change Old_SMR to Old SMR

den_dat$mpa_designation<-gsub("OLD_SMR","Old SMR",as.character(den_dat$mpa_designation))


# We need to summarize by Mpa_designation_year and remove all spp not seen at all

#summarize by mpa_des

dat_wide = den_dat %>% 
  group_by(method, island, survey_year, mpa_status, mpa_designation) %>% 
  summarise_all(.funs = mean)

#remove any spp not seen by any method

dat_wide <- dat_wide[, colSums(dat_wide != 0) > 0]

#also remove YOY, Biomass busters, and grouped categories

dat_wide$den_RYOY <- NULL
dat_wide$den_SMYSYOY <- NULL
dat_wide$den_BFREYOY <- NULL
dat_wide$den_CPUNYOY <- NULL
dat_wide$den_PCLAYOY <- NULL
dat_wide$den_SPAUYOY <- NULL
dat_wide$den_SPULYOY <- NULL
dat_wide$den_KGB <- NULL
dat_wide$den_OYB <- NULL

#dat_wide$den_OYT <- NULL
dat_wide$OYB <- NULL

dat_wide$den_SEBSPP <- NULL
dat_wide$den_SCOMSPP <- NULL
dat_wide$den_CITHSPP <- NULL
dat_wide$den_COTSP <- NULL
dat_wide$den_RFSP <- NULL
dat_wide$den_RONSP <- NULL
dat_wide$den_XLIO <- NULL

dat_wide$den_AHOL <- NULL
dat_wide$den_MMOL <- NULL



# Remove San Miguel Data

dat_wide <- subset(dat_wide, dat_wide$island != "San Miguel")


#if working with non standardized dataset
#Zero Populate dataset if not already

#dat_wide[is.na(dat_wide)] = 0
#dat_wide$den_targeted <- NULL
#dat_wide$den_nontargeted <- NULL
#dat_wide$den_total <- NULL
#dat_wide$den_Total <- NULL

# Remove fish not seen

dat_wide <- dat_wide[, colSums(dat_wide != 0) > 0]



#Create Site Lookup table
dat_wide$ID <- as.character(1:nrow(dat_wide))
nmds_sites <- dat_wide[ ,c("method", "survey_year", "island", "mpa_status", "mpa_designation", "ID")]# select ID variables for site table 
fish_dat_wide <- dat_wide[ ,-which(names(dat_wide) %in%c("method", "survey_year", "island", "site_block_drop", "mpa_designation", "mpa_status", "ID"))] #select spp columns only

colnames(fish_dat_wide)




#NMDS
NMDS_raw <- metaMDS(fish_dat_wide, 
                    distance = "bray",
                    k=2,
                    autotransform = FALSE,
                    trymax = 9999,
                    noshare = 0.1)


plot(NMDS_raw, type = 't')
stressplot(NMDS_raw)
NMDS_raw$stress #0.14

#____________Get the data out of the model____________#

data.scores <- as.data.frame(scores(NMDS_raw))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$ID <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
head(data.scores) 

#look at the data

species.scores <- as.data.frame(scores(NMDS_raw, "species")) 
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


#join nmds Data with Meta data
data.scores_2 <- left_join(nmds_sites,data.scores) #join by "ID"



#Convex hulls
chull_dat_2 <- data.scores_2 %>%
  group_by(method) %>%
  slice(chull(NMDS1, NMDS2))

method.cols <- c("#ff00ff", "#ccbfa5", "#088163")

#____________________ Plotting NMDS________________________#

nmds_fig_sites <- ggplot()+
  geom_point(data=data.scores_2,
             aes(x=NMDS1,y=NMDS2,shape=mpa_designation, fill=NA),size=3.0, 
             alpha = .85, show.legend = TRUE)+ 
  scale_shape_manual(values=c(15, 16, 17, 8))+
  geom_polygon(data = chull_dat_2,
               aes(x= NMDS1, y= NMDS2, group = method, fill= method), 
               alpha = 0.4, 
               size = 0.1)+
  # scale_color_manual(values = cbbPalette)+
  # scale_fill_manual(values = cbbPalette)+
  geom_text(aes(x=1.25, y=1.45, label="2D stress 0.14 \n data standardized \n yoy removed"))+
  guides(fill = FALSE)+
  # scale_color_brewer(palette = "Accent")+
  ylim(-1.6, 1.6)+ #-1.6, 1.4
  xlim(-1.6, 1.6)+ #-98  1.5
  theme_bw()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks = element_blank())+
  guides(fill = guide_legend(override.aes = list(colour = method.cols, pch=0))) # add

nmds_fig_sites

#________________ Plotting Species Scores__________________#

#We want to clean this plot up, so we'll export the species scores csv and manually adjust them

write.csv(species.scores, file = "Data_Exports/Diversity Data/NMDS_spp_scores.csv")

#edit values in excel, then reimport

species.scores.edit <- read.csv("Data_Exports/Diversity Data/NMDS_spp_scores_edited.csv")


nmds_fig_spp <- ggplot()+
  
  geom_segment(data = species.scores.edit, aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),colour="black", alpha = 0.2, inherit_aes=TRUE)+
  
  geom_text(data=species.scores.edit,
            aes(x=(NMDS1),y=(NMDS2),
                label=species,
                fontface = "bold")) +
  # scale_color_manual(values = cbbPalette)+
  # scale_fill_manual(values = cbbPalette)+
  geom_text(aes(x=1.35, y=1.5, label="2D stress 0.14"))+
  guides(fill = FALSE)+
  # scale_color_brewer(palette = "Accent")+ 
  ylim(-1.6, 1.6)+ #-1.6, 1.4
  xlim(-1.6, 1.6)+ #-98  1.5
  theme_bw()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  guides(fill = guide_legend(override.aes = list(colour = method.cols, pch=0))) # add

nmds_fig_spp




##_____________________ Mush Plots together _____________________ #

#we will separate out the legend from the ndms_fig_site and add it into grid as it's own plot

legend <- get_legend(nmds_fig_sites)

#remove legend from nmds_fig_site

nmds_fig_sites <- nmds_fig_sites + 
  theme(legend.position = "none")

#create a blank plot

bp <- ggplot() + geom_blank(aes(1, 1)) +
  cowplot::theme_nothing()

#arrange into multi panel plot

grid.arrange(nmds_fig_sites, nmds_fig_spp, legend, bp,
             ncol=2, nrow = 2, top= "MPA Comp NMDS",
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))



#mlet for zeropopulated data in long form
# dat_all_long_zero <- melt(dat_wide, id.vars = c("method", "survey_year", "island", "mpa_status", "mpa_designation"),
#                          variable.name = "PISCO_code")



#_______________________________________________________________#
#___________________ Analysis of Similarity ____________________#
#_______________________________________________________________#
##### https://jkzorz.github.io/2019/06/11/ANOSIM-test.html #####


#make community matrix - extract columns with abundance information, turn data frame into matrix
# MUST MAKE MATRIX NUMERIC

com = dat_wide[,6:ncol(dat_wide)]
m_com = as.matrix(com)


m_com <- matrix(as.numeric(m_com),    # Convert to numeric matrix
                ncol = ncol(m_com))
#be sure to remove "ID column"

m_com <- m_com[,1:68]


#Remove Empty Rows 
m_com <- m_com[rowSums(m_com[])>0,]



ano_both = anosim(x = m_com, grouping = dat_wide$method, distance = "bray", permutations = 9999)
summary(ano_both)

plot(ano_both)

# ANOSIM statistic R: 0.6353 
# Significance: 1e-04 


species.names <- colnames(dat_wide)
species.names <- species.names[6:73]

colnames(m_com) <- species.names


similarity <- simper(m_com, dat_wide$method, permutations = 999)

#view(similarity)



sim.sum <- summary(similarity)
sim.sum


# create a table with Top ten drivers for each comparison?

#dist for fun

#mrp <- mrpp(m_com, dat_wide$method, permutations = 999, distance = "bray")
#mrp

#_______________________________________________________________________#
#_______________________________________________________________________#
#______________________END OF BOTH SITES NMDS___________________________#
#_______________________________________________________________________#
#_______________________________________________________________________#


#Species Method histogram plot - Not Working 
x <- "den_CPUN"

dat_spp <- subset(dat_all_long_zero, PISCO_code == x )

p <- dat_spp %>%
  ggplot( aes(x=value, fill=method)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 50) +
  scale_fill_manual(values=c("#69b3a2", "#404080", "red")) +
  theme_ipsum() +
  labs(fill="")+ 
  ggtitle(x)

p


#__________________________________________________________________________#
#___________________________ ADONIS IN VEGAN ______________________________#
#__________________________________________________________________________#
# https://www.youtube.com/watch?v=1ETBgbXl-BM&ab_channel=RiffomonasProject #
#__________________________________________________________________________#

#run ADONIS using method and island as interaction terms


all_test <- adonis(m_com~dat_wide$method*dat_wide$island)

# check string if desired 
#str(all_test)

#call out results

print(all_test)

# _________________________ PERMANOVA _________________________________ #
# _____________ https://rpubs.com/collnell/manova _____________________ #

fish.mat <- sqrt(m_com)

fish.dist <- vegdist(fish.mat, method = 'bray')

set.seed(36)

fish.div <-adonis2(fish.dist~method, data = dat_wide, permutations = 999, method = 'bray')
fish.div

#try with islands


fish.div.2 <-adonis2(fish.dist~method + island + island:method, data = dat_wide, permutations = 999, method = 'bray')
fish.div.2

#multivariate dispersion

dispersion <- betadisper(fish.dist, group = dat_wide$method)
permutest(dispersion)

plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

#multivariate dispersion with islands

dispersion.2 <- betadisper(fish.dist, group = dat_wide$island)
permutest(dispersion.2)

plot(dispersion.2, hull=FALSE, ellipse=TRUE)


