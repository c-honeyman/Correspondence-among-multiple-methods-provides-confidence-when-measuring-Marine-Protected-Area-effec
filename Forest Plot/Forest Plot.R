# Species Response Ratio Clean Up and Forest Plotting #
# 6-22-21 by Chris Honeyman

library(plyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(stringr)

#be sure to clear environment before starting new session

#setwd on mac
setwd("/Volumes/pisco/ucsb/data/MPA methodology comparison/Data_Exports/Response Ratios")

#setwd on pc
#setwd("//FILES/pisco/ucsb/data/MPA methodology comparison/Data_Exports/Response Ratios")

#load in RR data for all spp

den.rr.dat <- read_csv("2_Clean_BRUV_All_spp_Fish_Density_RR.csv")
den.rr.dat$X1 <- NULL
#remove "den" from spp names
den.rr.dat$species<-gsub("den_","",as.character(den.rr.dat$species))

bm.rr.dat <- read_csv("2_Clean_BRUV_All_Spp_Fish_Biomass_RR.csv")
bm.rr.dat$X1 <- NULL
#remove "bm" from spp names
bm.rr.dat$species<-gsub("bm_","",as.character(bm.rr.dat$species))

#Load in Frequency of Occurence Density Data

den.freq.dat <- read_csv("Freq_data.csv")

#melt data into long form

den.freq.long <- gather(den.freq.dat, species, freq.occ, 3:181)

#remove "den_" from Species codes


den.freq.long <- den.freq.long %>%
  mutate_at("species", str_replace, "den_", "")

#____________________ Density Ratio Clean Up and Plotting ____________________#

#Remove San Miguel
den.rr.dat <- subset(den.rr.dat, den.rr.dat$island != "San Miguel")


#Summarize Data

den.rr.sum <- ddply(den.rr.dat, c("method", "island", "species"), summarise,
               N_rr    = length(log_ratio),
               mean_rr = mean(log_ratio),
               sd_rr   = sd(log_ratio),
               se_rr   = sd_rr / sqrt(N_rr))

#filter out a smaller set of species of interest in both density and frequency data 
#right now these are selected from a subset of the top drivers of dissimilarity from SIMPER Comparisons 


species.list <- c("SPUL", "PCLA", "SCAU", "SCAR", "EJAC", "SMIN", 
            "CPUN", "CPRI", "OELO", "targeted", "nontargeted")

spp.den.dat <- filter(den.rr.sum, species %in% species.list)

spp.freq.dat <- filter(den.freq.long, species %in% species.list)


#add in a "pos.neg" column for easy colorizing

spp.den.dat <- spp.den.dat %>% rowwise() %>%
  mutate(pos.neg = case_when(mean_rr>0 ~ "positive",
                             mean_rr<0 ~ "negative",
                             mean_rr==0 ~ "zero"))

#merge dataframes 

spp.den.dat <- spp.den.dat %>% full_join(spp.freq.dat)

#replace NA's with 1

spp.den.dat[is.na(spp.den.dat)] <- 1

#set frequency column as numeric

spp.den.dat$freq.occ <- as.numeric(spp.den.dat$freq.occ)

#replace codes with scientific names

rep_str = c('CPRI'='C. princeps','OELO'='O. elongatus','PCLA'='P. clathratus', 'SCAR'='S. caurinus', 'SCAU' = 'S. carnatus',
            'SMIN' = 'S. miniatus', 'EJAC' = 'E. jacksoni', 'CPUN' = 'C. punctipinnis', 'SPUL' = 'S. pulcher')
            
spp.den.dat$species <- str_replace_all(spp.den.dat$species, rep_str)

#reorder factor levels for plotting
spp.den.dat$species <- factor(spp.den.dat$species, levels=c("C. princeps", "O. elongatus", "P. clathratus","S. pulcher", "S. carnatus",
                                                            "S. caurinus", "S. miniatus", "E. jacksoni", "C. punctipinnis",
                                                            "nontargeted", "targeted"))


#Density Forest Plot by Island by Method

den.plot <- ggplot(spp.den.dat, aes(x=mean_rr, y=species, xmin= (mean_rr-se_rr), xmax = (mean_rr+se_rr))) +
  geom_pointrange(shape = 21, colour="black",
                  aes(size = freq.occ, fill = factor(pos.neg)))
 

  den.plot +
    
    scale_size(range = c(0.5, 1.5)) +
    
    scale_fill_manual(guide="legend", name = "Legend", values = c("positive" = "red", "negative" = "blue", "zero" = "black")) +
    
    facet_wrap(island + method ~., nrow=1) +
    
    geom_vline(xintercept=0, lty=2) +
    
    scale_x_continuous(breaks=c(-1.5, -0.75, 0, 0.75, 1.5), limits=c(-2, 2)) +
    
    labs(title = "Species Density Response Ratios by Island by Method",
         x = "Mean log(Response Ratio) Across Years",
         y = "Species") +
    
    theme_bw(base_size = 20) +
    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          strip.text.x = element_text(size = 12),
          axis.text.y = element_text(face = c('bold', 'bold', 'bold', 'bold', 'bold', 'bold', 'bold', 'plain', 'plain','plain', 'bold')),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(0, "lines"),
          panel.background = element_blank(),
          legend.title.align=0.5,
          plot.title = element_text(hjust = 0.5))

  
#Save: file name "Density RR by Island by Method with Frequency" Size 1300 x  (maintain aspect ratio)  
# ___________________________________________________________________________________________ #


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


#___________________________ Begin Biomass RR Forest Plot ____________________________________#

  
  bm.rr.dat <- read_csv("2_Clean_BRUV_All_Spp_Fish_Biomass_RR.csv")
  bm.rr.dat$X1 <- NULL
  #remove "bm" from spp names
  bm.rr.dat$species<-gsub("bm_","",as.character(bm.rr.dat$species))
  
  #Load in Frequency of Occurence Density Data - We are using density FoO even thought this is Biomass
  
  den.freq.dat <- read_csv("Freq_data.csv")
  
  #melt data into long form
  
  den.freq.long <- gather(den.freq.dat, species, freq.occ, 3:181)
  
  #remove "den_" from Species codes
  
  library(stringr)
  den.freq.long <- den.freq.long %>%
    mutate_at("species", str_replace, "den_", "")

#Remove San Miguel
bm.rr.dat <- subset(bm.rr.dat, bm.rr.dat$island != "San Miguel")


#Summarize Data

bm.rr.sum <- ddply(bm.rr.dat, c("method", "island", "species"), summarise,
                    N_rr    = length(log_ratio),
                    mean_rr = mean(log_ratio),
                    sd_rr   = sd(log_ratio),
                    se_rr   = sd_rr / sqrt(N_rr))

#filter out a smaller set of species of interest (right now this is arbitrary)
species.list <- c("SPUL", "PCLA", "SCAU", "SCAR", "EJAC", "SMIN", 
                  "CPUN", "CPRI", "OELO", "targeted", "nontargeted")

spp.bm.dat <- filter(bm.rr.sum, species %in% species.list)


#add in a "pos.neg" column for easy colorizing

spp.bm.dat <- spp.bm.dat %>% rowwise() %>%
  mutate(pos.neg = case_when(mean_rr>0 ~ "positive",
                             mean_rr<0 ~ "negative",
                             mean_rr==0 ~ "zero"))

#merge dataframes 

spp.bm.dat <- spp.bm.dat %>% full_join(spp.freq.dat)

#replace NA's with 1

spp.bm.dat[is.na(spp.bm.dat)] <- 1

#set frequency column as numeric

spp.bm.dat$freq.occ <- as.numeric(spp.bm.dat$freq.occ)

#Replace codes with scientific names


rep_str= c('CPRI'='C. princeps','OELO'='O. elongatus','PCLA'='P. clathratus', 'SCAR'='S. caurinus', 'SCAU' = 'S. carnatus',
            'SMIN' = 'S. miniatus', 'EJAC' = 'E. jacksoni', 'CPUN' = 'C. punctipinnis', 'SPUL' = 'S. pulcher')

spp.bm.dat$species <- str_replace_all(spp.bm.dat$species, rep_str)

#reorder factor levels for plotting
spp.bm.dat$species <- factor(spp.bm.dat$species, levels=c("C. princeps", "O. elongatus", "P. clathratus","S. pulcher", "S. carnatus",
                                                            "S. caurinus", "S. miniatus", "E. jacksoni", "C. punctipinnis",
                                                            "nontargeted", "targeted"))


#Biomass Forest Plot by Island by Method

bm.plot <- ggplot(spp.bm.dat, aes(x=mean_rr, y=species, xmin= (mean_rr-se_rr), xmax = (mean_rr+se_rr))) +
  geom_pointrange(shape = 21, colour="black",
                                  aes(size = freq.occ, fill = factor(pos.neg)))


bm.plot +
  
  scale_size(range = c(0.5, 1.5)) +
  
  scale_fill_manual(guide="legend", name = "Legend", values = c("positive" = "red", "negative" = "blue", "zero" = "black")) +
  
  facet_wrap(island + method ~., nrow=1) +
  
  geom_vline(xintercept=0, lty=2) +
  
  #xlim(-1, 1) +
  
  scale_x_continuous(breaks=c(-1.5, -0.75, 0, 0.75, 1.5), limits=c(-2, 2)) +
  
  
  labs(title = "Species Biomass Response Ratios by Island by Method",
       x = "Mean log(Response Ratio) Across Years",
       y = "Species") +
  
  theme_bw(20) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    strip.text.x = element_text(size = 12),
    axis.text.y = element_text(face = c('bold', 'bold', 'bold', 'bold', 'bold', 'bold', 'bold', 'plain', 'plain','plain', 'bold')),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(0, "lines"),
    panel.background = element_blank(),
    legend.title.align=0.5,
    plot.title = element_text(hjust = 0.5))


#Save: file name "Biomass RR by Island by Method with Frequency" Size 1300 x  (maintain aspect ratio

























