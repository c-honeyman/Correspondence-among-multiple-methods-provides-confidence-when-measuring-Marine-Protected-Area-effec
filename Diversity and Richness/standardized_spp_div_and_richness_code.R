 #### MPA Methodology Comp Paper ####
### Updated 2_2023 by Chris Honeyman and Peter Carlson###
#Diversity Index on STANDARDIZED DATA (Original Code by Peter Carlson and Conner Jainese)#

# Will begin with merged MPA dataset and convert to presence-absence for this first pass #
# create zero populated wide data to 
# be used in shannon diversity index

library(tidyverse)
#library(lubridate)
library(ggplot2)
library(plyr)
library(reshape2)
library(vegan)
library(readxl)
library(gt)
library(ggthemes)
library(viridis)
library(plotrix)
library(fitdistrplus)
library(gridExtra)
library(car) 
library(cowplot)
 rm(list=ls()) #clear the enviroment 
 
#setwd on mac
 setwd("/Volumes/pisco/ucsb/data/MPA methodology comparison")

#on pc
# setwd("C:/Users/Caselle Lab/Desktop/MPA_methods_comp")
#setwd("E:/Peter/Peter_Computer_backup/MPA_methods_comp")

#load density data
den.dat <- read.csv("Data_Exports/Clean_BRUV_Range_standardized_density_data.csv")


#Cast into long form

pa.long <- gather(den.dat, PISCO_code, value, 9:176)


#Start Diversity Code

all_fish_wider <- pa.long %>% 
  dplyr:: select("PISCO_code", "method", "survey_year", "survey_date", "island", "site_block_drop", "mpa_status", "value" ) %>%
  
  #change data to intergers
  # mutate(value = as.integer(value)) %>%
  
  #make it wide
  pivot_wider(names_from = PISCO_code, values_from = value)

#isolate row names to add back later
all_fish_wider$UniqueID <- paste(all_fish_wider$site_block_drop, all_fish_wider$survey_year, all_fish_wider$survey_date, all_fish_wider$island, all_fish_wider$mpa_status)

rownames <- all_fish_wider$UniqueID

#remove row names for diversity calculation
all_fish_wider$UniqueID <- NULL
all_fish_wider$survey_date <- NULL
all_fish_wider$site_block_drop <- NULL
all_fish_wider$island <- NULL
all_fish_wider$mpa_status <- NULL
all_fish_wider$mpa_designation <- NULL
all_fish_wider$survey_year <-NULL
all_fish_wider$method <- NULL

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

#spp richness (per site/block/drop) by depth
dat_sp_rich<- pa.long %>% 
  filter(value > 0) %>% 
  dplyr::group_by(site_block_drop,mpa_status, island, survey_year, survey_date, method) %>% 
  dplyr::summarise(count = length(unique(PISCO_code))) %>% 
  dplyr::rename(sp_rich = count)
#___________________________________________________#

#isolate row names for dat_sp_rich to join H and richness

dat_sp_rich$UniqueID <- paste(dat_sp_rich$site_block_drop, dat_sp_rich$survey_year, dat_sp_rich$survey_date, dat_sp_rich$island, dat_sp_rich$mpa_status)

rownames <- all_fish_wider$UniqueID





#_______join div, richness and meta data______#

#joining H_all to metadata
dat_div <- left_join(dat_H_all_fish, dat_sp_rich )#join by "UniqueID"

dat_div <- dat_div[, c(8, 2:7, 9, 1)]

# dat_div <- left_join(dat_div, meta_data)

dat_div$H_all <- as.numeric(dat_div$H_all)

#Joining, by = c("Drop_ID", "MPA_Status", "Reserve_Name", "Depth", "island_MPA")

#export diversity data

dat_div$island_MPA <- paste(dat_div$island, dat_div$mpa_status)

#save data; be sure to edit date!

# write.csv(dat_div, "Data_Exports/Clean_BRUV_MPA_Comp_status_range_standardized_diversity_data_8_9_22.csv")

#____________________ Stop Div Calculations _____________________#

# remove all observations where H = 0
# these are drops on BRUV & 1 CCFRP Cell where only sanddabs were caught due to not being on the porper habitat

test_dat_div <- subset(dat_div, H_all!= 0)

dat_div <- test_dat_div

#_____________________ Start Div Plotting _____________________#

### In Progress 6/10 ##

#Remove San Miguel
dat_div <- subset(dat_div, dat_div$island != "San Miguel")

#Add Island - MPA designation column for plotting

dat_div$island_MPA <- paste(dat_div$island, dat_div$mpa_status)


#plot shannon diversity by method
div.plot <- ggplot(dat_div, aes(x = method, y = H_all)) +
            geom_violin() +
            geom_jitter(width = .1)+

  facet_wrap(island~.) +
  
  labs(title = "Shannon Diversity Index by Method",
       x = "Method",
       y = "Shannon Diversity Index") +
  
  theme_bw() +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

div.plot

#mean species richness by site/MPA
mean_sp_rich <- dat_div %>% 
  group_by(method, island_MPA, island, mpa_status, survey_year) %>% 
  dplyr::summarise(mean = mean(sp_rich),
                   sd = sd(sp_rich))

#mean diversity index by site/MPA
mean_shan_div <- dat_div %>% 
  group_by(method, island_MPA, island, mpa_status, survey_year) %>% 
  dplyr::summarise(mean = mean(H_all),
                   sd = sd(H_all))



#plot mean_sp_rich data,   Table?
mean.rich.plot <- ggplot(data = mean_sp_rich) + 
                      geom_col(mapping = aes(x = island_MPA, y = mean, fill = method), position = "dodge")

mean.rich.plot


#plot mean_shan_div data
mean.shan.plot <- ggplot(data = mean_shan_div) + 
                    geom_col(mapping = aes(x = island_MPA, y = mean, fill = method), position = "dodge")

mean.shan.plot


#Island MPA designation diversity plot 

div_island_plot <- ggplot(dat_div, aes(method, H_all))+
  geom_boxplot(position = "dodge", aes(fill=mpa_status), show.legend = TRUE)+
  #geom_jitter(width = .15, color = "black", size = 3)+
  facet_wrap(island~.)+
  labs( y = "Shannon Diversity Index") +
  scale_fill_manual(values = alpha(c("red", "blue"), .75))+
  guides(fill = guide_legend(title = "MPA Status")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = 'none')

div_island_plot

###_______________________ Mean Spp_Rich by MPA/REF ______________________________________________#

rich_island_mpa_plot <- ggplot(mean_sp_rich, aes(x=method, y=mean, xmin= (mean-sd), xmax = (mean+sd)))+
  
  geom_boxplot(position = "dodge", aes(fill=mpa_status), show.legend = TRUE)+
  
  #geom_jitter(width = .15, color = "black", size = 3)+
  
  facet_wrap(island~.)+
  
  labs(title = "Species Richness by Island by Method",
       x = "Method",
       y = "Species Richness") +
  
  scale_fill_manual(values = alpha(c("red", "blue"), .75))+
  
  guides(fill = guide_legend(title = "MPA Designation")) +
  
  theme_bw() +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

rich_island_mpa_plot

#_______________________ Combine the two into one panel _____________________________ #

#we will separate out the legend from the ndms_fig_site and add it into grid as it's own plot

legend <- get_legend(div_island_plot)
legend <- as_grob(legend)

#remove legend from nmds_fig_site

div_island_plot <- div_island_plot + 
  theme(legend.position = "none")

rich_island_mpa_plot <- rich_island_mpa_plot + 
  theme(legend.position = "none")

#create a blank plot

#bp <- ggplot() + geom_blank(aes(1, 1)) +
  cowplot::theme_nothing()

#arrange into multi panel plot


#We're going to visualize Richness as a violin plot 


spp_rich <- ggplot(dat_div, aes(method, sp_rich, fill = mpa_status, color = mpa_status))+
  geom_violin(alpha = .1, lwd = .5)+
  geom_point(position=position_jitterdodge(jitter.width = .2, jitter.height = .5), shape = 21)+
  scale_color_manual(values = alpha(c("red", "blue"), .75))+
  xlab("Method")+
  ylab("Species Richness")+
  facet_wrap(island~.)+
  theme_classic()+
  theme(
      legend.position = 'none'
  )
spp_rich

#New multi-panel with violin

 grid.arrange(div_island_plot, spp_rich,
                 ncol=1, nrow = 2,
                 widths = c(1.0))




             
#______________________ End Diversity Plotting ________________________________________#














rm(list=setdiff(ls(), c("dat_div", "div_island_plot", "rich_island_mpa_plot")))

#______________________ Statsy Stuff for diversity Data using H-index_________________________ #
dat_div$island_MPA_method <- paste(dat_div$island_MPA, dat_div$method, sep = "_")


dat_div %>%
  dplyr:: group_by(island_MPA_method)%>%
  dplyr:: summarise(stat = shapiro.test(H_all)$statistic,
                    pval = shapiro.test(H_all)$p.value)

#Diversity data is  normally distributed 


hist_plot <- ggplot(data = dat_div, aes(x = H_all, color = island_MPA, fill = method))+
  geom_histogram(binwidth = .1)+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  facet_wrap(~island_MPA)

hist_plot


#wilcox pairwise comparisons 


#both islands together

#div_compare <- pairwise.wilcox.test(dat_div$H_all, dat_div$mpa_status,
#                                    paired = FALSE, p.adjust.method = "none")

#div_compare

#sr <- subset(dat_div, island == "Santa Rosa")
#an <- subset(dat_div, island == "Anacapa")

#Rosa

#sr_div_compare <- pairwise.wilcox.test(sr$H_all, sr$mpa_status,
#                                    paired = FALSE, p.adjust.method="none")
#sr_div_compare

#Anacapa

#ana_div_compare <- pairwise.wilcox.test(an$H_all, an$mpa_status,
#                                      paired = FALSE, p.adjust.method="none")
#ana_div_compare



#linear modeling both sites (in progress 6/22)
fit_div <- lm(H_all ~ method, data = dat_div)

summary(fit_div)

hist(fit_div$residuals)

qqnorm(fit_div$residuals)


shapiro.test(fit_div$residuals)    #data not normally distributed



#_________________ Stats by Island ______________________#

#subset mean diversity data for stats by individual island

dat_div$method <- factor(dat_div$method)


sr <- subset(dat_div, island == "Santa Rosa")
an <- subset(dat_div, island == "Anacapa")
#_____________________________________________#

#_____________________________________#
#ANA


ana_fit_div_2 <- aov(H_all ~ method, data = an)
summary(ana_fit_div_2)

test <- TukeyHSD(ana_fit_div_2)

hist(ana_fit_div_2$residuals)
plot(ana_fit_div_2, 1)

leveneTest(H_all ~ method, data = an)  #Variance between methods are not equal,  cannot do ANOVA



pt_an <- pairwise.t.test(an$H_all, an$method,
                           p.adjust.method = 'bonferroni', pool.sd = FALSE, var.eq = F)

pt_an

# Pairwise comparisons using t tests with non-pooled SD 
# 
# data:  an$H_all and an$method 
# 
#            BRUV    CCFRP  
#   CCFRP  0.00052   -      
#   SCUBA  < 2e-16   < 2e-16
# 
# P value adjustment method: bonferroni 


#run with each method "SCUBA", "CCFRP", "BRUV"

method_an <- subset(an, method == "SCUBA")

ttest_method_an <- t.test(H_all ~ mpa_status, data = method_an)
ttest_method_an


#BRUV.... MPA vs REF    p = 0.75
#CCFRP... MPA vs REF    p = 0.8661
#SCUBA... MPA vs REF    p= 0.20

#________________________Santa Rosa only______________________________#
#testing the same models as Anacapa, but with santa rosa


sr_fit_div_1 <- aov(H_all ~ method + mpa_status, data = sr) 
summary(sr_fit_div_1)

sr_tuky <- TukeyHSD(sr_fit_div_1)
sr_tuky


#fitted vs residulas
plot(sr_fit_div_1, 1)

leveneTest(H_all ~ method*mpa_status, data = sr)  #same as ANA data, variance of groups is not equal 


#pairwise test between methods 
pt_sr <- pairwise.t.test(sr$H_all, sr$method,
                         p.adjust.method = 'bonferroni' , pool.sd = FALSE)

pt_sr

# Pairwise comparisons using t tests with non-pooled SD 
# 
# data:  sr$H_all and sr$method 
# 
#           BRUV    CCFRP  
#   CCFRP < 2e-16 -      
#   SCUBA < 2e-16 3.1e-11
# 
# P value adjustment method: bonferroni 


#run with each method: "BRUV", "CCFRP", "SCUBA"
method_sr <- subset(sr, method == "SCUBA")

t_test_method_sr <- t.test(H_all ~ mpa_status, data = method_sr)
t_test_method_sr



#BRUV.... MPA vs REF    p = 0.02655
#CCFRP... MPA vs REF    p = 0.497
#SCUBA... MPA vs REF    p= 0.07


leveneTest(H_all ~ mpa_status, data = method_sr)





#________________ Now repeat for Species Richness_______________#

dat_div %>%
  dplyr:: group_by(island_MPA_method)%>%
  dplyr:: summarise(stat = shapiro.test(sp_rich)$statistic,
                    pval = shapiro.test(sp_rich)$p.value)



# Data is super skewed
hist(an$sp_rich)


hist_plot <- ggplot(data = dat_div, aes(x = sp_rich, color = island_MPA, fill = method))+
  geom_histogram(binwidth = 1)+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  facet_wrap(~island_MPA)

hist_plot





#______________
#ANA
leveneTest(sp_rich ~ method, data = an)
#data is not normal, and varainces are different.....  well isnt that greaaaat 


#betweeen methods 
pairwise.wilcox.test(an$sp_rich, an$method, p.adjust.method = 'bonferroni')

#           BRUV    CCFRP  
#   CCFRP 0.096   -      
#   SCUBA <2e-16 <2e-16



#within Method, testing for difference between MPA and REF
#run each method: "BRUV", "CCFRP", and "SCUBA"
an_method <- subset(an, method == "SCUBA")
wilcox.method.an <- wilcox.test(sp_rich ~ mpa_status, data = an_method)
wilcox.method.an

an_method_plot <- ggplot(an_method, aes(mpa_status, sp_rich))+
                    geom_jitter(width = .2, size = 3)
an_method_plot

#BRUV = 0.75
#CCFRP = 0.4529
#SCUBA = 0.02**

#___________________________________________________#
#Santa Rosa 



#betweeen methods 
pairwise.wilcox.test(sr$sp_rich, sr$method, p.adjust.method = 'bonferroni')

#           BRUV    CCFRP  
#   CCFRP 3.3e-11   -      
#   SCUBA 3.7e-14   6.2e-13



#within Method, testing for difference between MPA and REF
#again run all methods: SCUBA, BRUV, CCFRP
sr_method <- subset(sr, method == "SCUBA")
wilcox.method.sr <- wilcox.test(sp_rich ~ mpa_status, data = sr_method)
wilcox.method.sr


#BRUV = 0.07
#CCFRP = 0.826
#SCUBA = 0.05




#Species richness plot


ana_spp_rich <- ggplot(dat_div, aes(method, sp_rich, fill = mpa_status, color = mpa_status))+
                      geom_violin(alpha = .1, lwd = 1)+
                      geom_point(position=position_jitterdodge(jitter.width = .2, jitter.height = .5), shape = 21)+
  scale_color_manual(values = alpha(c("red", "blue"), .75))+
  xlab("Method")+
  ylab("Species Richness")+

  facet_wrap(island~.)+
  
  theme_bw(base_size = 15)

ana_spp_rich














ana_fit_rich <- lm(sp_rich ~ method + mpa_status + mpa_status*method , data = an)
summary(ana_fit_rich) #interaction term not sig

ana_fit_rich_1 <- lm(sp_rich ~ method + mpa_status  , data = an)
summary(ana_fit_rich_1) #mpa status not sig

ana_fit_rich_2 <- lm(sp_rich ~ mpa_status, data = an)
summary(ana_fit_rich_2)


ana.av.rich <- anova( ana_fit_rich_1, ana_fit_rich_2)

ana.av.rich #seems like the model with mpa_status but no interaction term explains it the best

#testing residuals 

hist(ana_fit_rich_1$residuals)

qqnorm(ana_fit_rich_1$residuals)

shapiro.test(ana_fit_rich_1$residuals) #Data is NOT normally distributed




#________________________Santa Rosa only______________________________#

hist(sr$sp_rich)

#testing the same models as Anacapa, but with santa rosa


sr_fit_rich <- lm(sp_rich ~ method + mpa_status + mpa_status*method, data = sr)
summary(sr_fit_rich)  #Interaction term not significant

sr_fit_rich_1 <- lm(sp_rich ~ method + mpa_status, data = sr) 
summary(sr_fit_rich_1)

sr_fit_rich_2 <- lm(sp_rich ~ method, data = sr)
summary(sr_fit_rich_2)

sri.an.rich <- anova(sr_fit_rich_2, sr_fit_rich_1, sr_fit_rich)

sri.an.rich

hist(sr_fit_rich_1$residuals)

qqnorm(sr_fit_rich_1$residuals)

shapiro.test(sr_fit_rich_1$residuals) # Data is NOT normally distributed




#__________________________ End Diversity and Richness Scirpt________________________#