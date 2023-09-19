### MPA methodology comparison Standardization ###
# by Chris Honeyman 6/10/21

# This script will take density data, split into 3 projects, standardize each project,
# and recombine the standardized data into one big dataset

library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)
library(reshape2)
library(vegan)

#set wd on pc
#setwd("//FILES/pisco/ucsb/data/MPA methodology comparison")


#on mac
setwd("/Volumes/pisco/ucsb/data/MPA methodology comparison")

#load data

den.dat <- read.csv("Data_Exports/archived/density_data.csv")

#remove targeted, nontargeted, and total columns
#remove grouped unknown and generalized catagories

den.dat$den_targeted <- NULL
den.dat$den_nontargeted <- NULL
den.dat$den_Total <- NULL
den.dat$X <- NULL
den.dat$den_BAITBALL <-NULL
den.dat$den_RFSP <- NULL
den.dat$den_RYOY <- NULL
den.dat$den_RFYOY <- NULL
den.dat$den_SEBASTESSPP <- NULL
den.dat$den_SEBASTESSPP <- NULL
den.dat$den_UnID.SMYS <- NULL
den.dat$den_UNK <- NULL

#split data by method

ccfrp <- subset(den.dat, method == "CCFRP")
scuba <- subset(den.dat, method == "SCUBA")
bruv <- subset(den.dat, method == "BRUV")

#standardize data by column for each project

#_____ CCFRP _____ #
#remove header
ccfrp.header <- ccfrp[, c(1:7)]
ccfrp.header$X <- seq.int(nrow(ccfrp.header))

ccfrp <- ccfrp[, c(8:180)]

#replace Nas with 0
ccfrp[is.na(ccfrp)] = 0

#standardize

std.ccfrp <- decostand(ccfrp, "range")

#create numeric key to remerge
std.ccfrp$X <- seq.int(nrow(std.ccfrp))
std.ccfrp <- std.ccfrp[, c(174, 1:173)]

std.ccfrp.full <- merge(ccfrp.header, std.ccfrp, by = "X")
std.ccfrp.full$X <- NULL


#_____ BRUV _____ #
#remove header
bruv.header <- bruv[, c(1:7)]
bruv.header$X <- seq.int(nrow(bruv.header))

bruv <- bruv[, c(8:180)]

#replace Nas with 0
bruv[is.na(bruv)] = 0

#standardize

std.bruv <- decostand(bruv, "range")

#create numeric key to remerge
std.bruv$X <- seq.int(nrow(std.bruv))
std.bruv <- std.bruv[, c(174, 1:173)]

std.bruv.full <- merge(bruv.header, std.bruv, by = "X")
std.bruv.full$X <- NULL


#_____ SCUBA ______ #


#remove header and save for later
scuba.header <- scuba[, c(1:7)]
scuba.header$X <- seq.int(nrow(scuba.header))

scuba <- scuba[, c(8:180)]

#replace Nas with 0
scuba[is.na(scuba)] = 0

#standardize

std.scuba <- decostand(scuba, "range")

#create numeric key to remerge
std.scuba$X <- seq.int(nrow(std.scuba))
std.scuba <- std.scuba[, c(174, 1:173)]

std.scuba.full <- merge(scuba.header, std.scuba, by = "X")
std.scuba.full$X <- NULL

#_______ Merge Standardized Data Together _______#

#format each as dataframe and make a list

std.ccfrp.full <- as.data.frame(std.ccfrp.full)
std.scuba.full <- as.data.frame(std.scuba.full)
std.bruv.full <- as.data.frame(std.bruv.full)


# Export Standardized Data

write.csv(mpa.dat.clean, "Data_Exports/Standardized Datasets/Clean_BRUV_Range_standardized_density_data.csv")