rm(list = ls())

# set working directory to file location
# for Travis:
setwd("D:/GitHub/RPA-Water")

library(tidyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(data.table)

#set models
#-----------------------------------------------------------------------------------

#IMPORTANT: need to set the carbon assumption and global climate model
#-----------------------------------------------------------------------------------
#carbon=45 or carbon=85 depending on which carbon climate model
carbon <- 45

#set the global climate model. options: "cnrm_c5", "hadgem","ipsl_cm5a","mri_cgcm3","noresm", "base"
gcm <- "cnrm_c5"

#this is messy, but brings in the corresponding climate data based on setting the model above
if(carbon==45 & gcm=="cnrm_c5"){
precip <-read.csv("ClimateData/p_cnrm_c5_45.csv", check.names=F)
pet <-read.csv("ClimateData/pet_cnrm_c5_45.csv", check.names=F)
}
if(carbon==85 & gcm=="cnrm_c5"){
  precip <-read.csv("ClimateData/p_cnrm_c5_85.csv", check.names=F)
  pet <-read.csv("ClimateData/pet_cnrm_c5_85.csv", check.names=F)
}
if(carbon==45 & gcm=="hadgem"){
  precip <-read.csv("ClimateData/p_hadgem_45.csv", check.names=F)
  pet <-read.csv("ClimateData/pet_hadgem_45.csv", check.names=F)
}
if(carbon==85 & gcm=="hadgem"){
  precip <-read.csv("ClimateData/p_hadgem_85.csv", check.names=F)
  pet <-read.csv("ClimateData/pet_hadgem_85.csv", check.names=F)
}
if(carbon==45 & gcm=="ipsl_cm5a"){
  precip <-read.csv("ClimateData/p_ipsl_cm5a_45.csv", check.names=F)
  pet <-read.csv("ClimateData/pet_ipsl_cm5a_45.csv", check.names=F)
}
if(carbon==85 & gcm=="ipsl_cm5a"){
  precip <-read.csv("ClimateData/p_ipsl_cm5a_85.csv", check.names=F)
  pet <-read.csv("ClimateData/pet_ipsl_cm5a_85.csv", check.names=F)
}
if(carbon==45 & gcm=="mri_cgcm3"){
  precip <-read.csv("ClimateData/p_mri_cgcm3_45.csv", check.names=F)
  pet <-read.csv("ClimateData/pet_mri_cgcm3_45.csv", check.names=F)
}
if(carbon==85 & gcm=="mri_cgcm3"){
  precip <-read.csv("ClimateData/p_mri_cgcm3_85.csv", check.names=F)
  pet <-read.csv("ClimateData/pet_mri_cgcm3_85.csv", check.names=F)
}
if(carbon==45 & gcm=="noresm"){
  precip <-read.csv("ClimateData/p_noresm_45.csv", check.names=F)
  pet <-read.csv("ClimateData/pet_noresm_45.csv", check.names=F)
}
if(carbon==85 & gcm=="noresm"){
  precip <-read.csv("ClimateData/p_noresm_85.csv", check.names=F)
  pet <-read.csv("ClimateData/pet_noresm_85.csv", check.names=F)
}
if(gcm=="base"){
  precip <-read.csv("ClimateData/baseline.csv", check.names=F)
  pet <-read.csv("ClimateData/baselinepet.csv", check.names=F)
}

#takes output from WaterUsedatacleanup.R, population and income projections and creates input for WEAP
#WaterUsedatacleanup.R takes USGS water data and converts it into a usable format. See that file for additional assumptions in our modeling.

#this function fixes FIPS differences

fipsfix <- function (inc) 
{
inc[,c(1)] <- ifelse(inc[,c(1)] == 51003, 51901, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51540, 51901, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51015, 51907, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51790, 51907, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51820, 51907, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51031, 51911, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51680, 51911, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51035, 51913, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51640, 51913, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51053, 51918, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51570, 51918, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51730, 51918, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51059, 51919, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51600, 51919, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51610, 51919, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51069, 51921, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51840, 51921, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51081, 51923, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51595, 51923, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51089, 51929, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51690, 51929, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51095, 51931, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51830, 51931, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51121, 51933, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51750, 51933, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51143, 51939, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51590, 51939, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51143, 51941, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51670, 51941, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51153, 51942, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51683, 51942, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51685, 51942, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51161, 51944, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51775, 51944, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51163, 51945, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51530, 51945, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51687, 51945, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51165, 51947, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51660, 51947, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51175, 51949, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51620, 51949, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51177, 51951, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51630, 51951, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51191, 51953, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51520, 51953, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51195, 51955, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51720, 51955, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51199, 51958, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51735, 51958, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51515, 51019, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51560, 51005, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51005, 51903, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51580, 51903, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51780, 51925, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51083, 51925, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51019, 51909, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51149, 51941, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 51678, 51945, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 12025, 12086, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 46113, 46102, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 55078, 55901, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 55115, 55901, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 08014, 08013, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 04012, 04027, inc[,c(1)])
inc[,c(1)] <- ifelse(inc[,c(1)] == 35006, 35007, inc[,c(1)])
inc <- inc %>% group_by(fips) %>% summarise_all(funs(sum))
}

#fix fips codes for energy regions
fipsfixenergy <- function (inc) 
{
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51003, 51901, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51540, 51901, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51015, 51907, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51790, 51907, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51820, 51907, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51031, 51911, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51680, 51911, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51035, 51913, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51640, 51913, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51053, 51918, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51570, 51918, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51730, 51918, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51059, 51919, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51600, 51919, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51610, 51919, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51069, 51921, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51840, 51921, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51081, 51923, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51595, 51923, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51089, 51929, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51690, 51929, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51095, 51931, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51830, 51931, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51121, 51933, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51750, 51933, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51143, 51939, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51590, 51939, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51143, 51941, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51670, 51941, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51153, 51942, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51683, 51942, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51685, 51942, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51161, 51944, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51775, 51944, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51163, 51945, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51530, 51945, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51687, 51945, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51165, 51947, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51660, 51947, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51175, 51949, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51620, 51949, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51177, 51951, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51630, 51951, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51191, 51953, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51520, 51953, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51195, 51955, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51720, 51955, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51199, 51958, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51735, 51958, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51515, 51019, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51560, 51005, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51005, 51903, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51580, 51903, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51780, 51925, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51083, 51925, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51019, 51909, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51149, 51941, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 51678, 51945, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 12025, 12086, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 46113, 46102, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 55078, 55901, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 55115, 55901, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 08014, 08013, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 04012, 04027, inc[,c(1)])
  inc[,c(1)] <- ifelse(inc[,c(1)] == 35006, 35007, inc[,c(1)])
  inc <- inc %>% group_by(fips) %>% summarise_all(funs(sum,mean))
}

#fixes fip codes for County Watershed Percentages
cntypercent<-read.csv("CountyWatershedPct.csv")
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51003, 51901, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51540, 51901, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51015, 51907, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51790, 51907, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51820, 51907, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51031, 51911, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51680, 51911, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51035, 51913, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51640, 51913, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51053, 51918, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51570, 51918, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51730, 51918, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51059, 51919, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51600, 51919, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51610, 51919, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51069, 51921, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51840, 51921, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51081, 51923, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51595, 51923, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51089, 51929, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51690, 51929, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51095, 51931, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51830, 51931, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51121, 51933, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51750, 51933, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51143, 51939, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51590, 51939, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51143, 51941, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51670, 51941, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51153, 51942, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51683, 51942, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51685, 51942, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51161, 51944, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51775, 51944, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51163, 51945, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51530, 51945, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51687, 51945, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51165, 51947, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51660, 51947, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51175, 51949, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51620, 51949, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51177, 51951, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51630, 51951, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51191, 51953, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51520, 51953, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51195, 51955, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51720, 51955, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51199, 51958, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51735, 51958, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51515, 51019, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51560, 51005, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51005, 51903, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51580, 51903, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51780, 51925, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51083, 51925, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51019, 51909, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51149, 51941, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 51678, 51945, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 12025, 12086, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 46113, 46102, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 55078, 55901, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 55115, 55901, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 08014, 08013, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 04012, 04027, cntypercent[,c(1)])
  cntypercent[,c(1)] <- ifelse(cntypercent[,c(1)] == 35006, 35007, cntypercent[,c(1)])
  cntypercent[,c(5)] <- 0
cntypercent <- cntypercent %>% group_by(FIPS, HUC_10) %>% summarise(Shape_Area=sum(Shape_Area), CountyArea=sum(CountyArea), AreaHuc10=mean(AreaHuc10), CountyAreaPct=sum(CountyAreaPct), HU_10_NAME=sum(HU_10_NAME))
cntypercent <- cntypercent[,c(1,4,3,2,7,5,6)]
cntypercent$CountyAreaPct <- cntypercent$Shape_Area/cntypercent$CountyArea

#This function is used to create the linear approximations for years in between the 5 year intervals.
linapprox <- function (DF)
{
  DF$Y2016 <- DF$Y2015 + 1*(DF$Y2020 - DF$Y2015)/5
  DF$Y2017 <- DF$Y2015 + 2*(DF$Y2020 - DF$Y2015)/5
  DF$Y2018 <- DF$Y2015 + 3*(DF$Y2020 - DF$Y2015)/5
  DF$Y2019 <- DF$Y2015 + 4*(DF$Y2020 - DF$Y2015)/5
  
  DF$Y2021 <- DF$Y2020 + 1*(DF$Y2025 - DF$Y2020)/5
  DF$Y2022 <- DF$Y2020 + 2*(DF$Y2025 - DF$Y2020)/5
  DF$Y2023 <- DF$Y2020 + 3*(DF$Y2025 - DF$Y2020)/5
  DF$Y2024 <- DF$Y2020 + 4*(DF$Y2025 - DF$Y2020)/5
  
  DF$Y2026 <- DF$Y2025 + 1*(DF$Y2030 - DF$Y2025)/5
  DF$Y2027 <- DF$Y2025 + 2*(DF$Y2030 - DF$Y2025)/5
  DF$Y2028 <- DF$Y2025 + 3*(DF$Y2030 - DF$Y2025)/5
  DF$Y2029 <- DF$Y2025 + 4*(DF$Y2030 - DF$Y2025)/5
  
  DF$Y2031 <- DF$Y2030 + 1*(DF$Y2035 - DF$Y2030)/5
  DF$Y2032 <- DF$Y2030 + 2*(DF$Y2035 - DF$Y2030)/5
  DF$Y2033 <- DF$Y2030 + 3*(DF$Y2035 - DF$Y2030)/5
  DF$Y2034 <- DF$Y2030 + 4*(DF$Y2035 - DF$Y2030)/5
  
  DF$Y2036 <- DF$Y2035 + 1*(DF$Y2040 - DF$Y2035)/5
  DF$Y2037 <- DF$Y2035 + 2*(DF$Y2040 - DF$Y2035)/5
  DF$Y2038 <- DF$Y2035 + 3*(DF$Y2040 - DF$Y2035)/5
  DF$Y2039 <- DF$Y2035 + 4*(DF$Y2040 - DF$Y2035)/5
  
  DF$Y2041 <- DF$Y2040 + 1*(DF$Y2045 - DF$Y2040)/5
  DF$Y2042 <- DF$Y2040 + 2*(DF$Y2045 - DF$Y2040)/5
  DF$Y2043 <- DF$Y2040 + 3*(DF$Y2045 - DF$Y2040)/5
  DF$Y2044 <- DF$Y2040 + 4*(DF$Y2045 - DF$Y2040)/5
  
  DF$Y2046 <- DF$Y2045 + 1*(DF$Y2050 - DF$Y2045)/5
  DF$Y2047 <- DF$Y2045 + 2*(DF$Y2050 - DF$Y2045)/5
  DF$Y2048 <- DF$Y2045 + 3*(DF$Y2050 - DF$Y2045)/5
  DF$Y2049 <- DF$Y2045 + 4*(DF$Y2050 - DF$Y2045)/5
  
  DF$Y2051 <- DF$Y2050 + 1*(DF$Y2055 - DF$Y2050)/5
  DF$Y2052 <- DF$Y2050 + 2*(DF$Y2055 - DF$Y2050)/5
  DF$Y2053 <- DF$Y2050 + 3*(DF$Y2055 - DF$Y2050)/5
  DF$Y2054 <- DF$Y2050 + 4*(DF$Y2055 - DF$Y2050)/5
  
  DF$Y2056 <- DF$Y2055 + 1*(DF$Y2060 - DF$Y2055)/5
  DF$Y2057 <- DF$Y2055 + 2*(DF$Y2060 - DF$Y2055)/5
  DF$Y2058 <- DF$Y2055 + 3*(DF$Y2060 - DF$Y2055)/5
  DF$Y2059 <- DF$Y2055 + 4*(DF$Y2060 - DF$Y2055)/5
  
  DF$Y2061 <- DF$Y2060 + 1*(DF$Y2065 - DF$Y2060)/5
  DF$Y2062 <- DF$Y2060 + 2*(DF$Y2065 - DF$Y2060)/5
  DF$Y2063 <- DF$Y2060 + 3*(DF$Y2065 - DF$Y2060)/5
  DF$Y2064 <- DF$Y2060 + 4*(DF$Y2065 - DF$Y2060)/5
  
  DF$Y2066 <- DF$Y2065 + 1*(DF$Y2070 - DF$Y2065)/5
  DF$Y2067 <- DF$Y2065 + 2*(DF$Y2070 - DF$Y2065)/5
  DF$Y2068 <- DF$Y2065 + 3*(DF$Y2070 - DF$Y2065)/5
  DF$Y2069 <- DF$Y2065 + 4*(DF$Y2070 - DF$Y2065)/5
  
  DF <- DF[,c(1,2,3,15:18,4,19:22,5,23:26,6,27:30,7,31:34,8,35:38,9,39:42,10,43:46,11,47:50,12,51:54,13,55:58,14)]
}


# ## DP -- Travis' attampt
# 
# demand.ssp1 <- read.csv('inputs_ssp1.csv')
# demand.ssp1$dpu <- NA
# 
# demand.ssp1 <- merge(demand.ssp1, pop1, by='fips')
# 
# demand.2020 <- demand.ssp1
# demand.2020$year = 2020
# demand.2020$pop <- demand.2020$pr_pop_2020
# demand.2020$inc.in.thousands <- NA
# demand.2020$AcresIrrig <- NA
# 
# demand.2025 <- demand.2020
# demand.2030 <- demand.2020
# demand.2035 <- demand.2020
# demand.2040 <- demand.2020
# demand.2045 <- demand.2020
# demand.2050 <- demand.2020
# demand.2055 <- demand.2020
# demand.2060 <- demand.2020
# demand.2065 <- demand.2020
# demand.2070 <- demand.2020
# 
# demand.2025$year <- 2025
# demand.2030$year <- 2030
# demand.2035$year <- 2035
# demand.2040$year <- 2040
# demand.2045$year <- 2045
# demand.2050$year <- 2050
# demand.2055$year <- 2055
# demand.2060$year <- 2060
# demand.2065$year <- 2065
# demand.2070$year <- 2070
# 
# demand.2025$pop <- demand.2025$pr_pop_2025
# demand.2030$pop <- demand.2030$pr_pop_2030
# demand.2035$pop <- demand.2035$pr_pop_2035
# demand.2040$pop <- demand.2040$pr_pop_2040
# demand.2045$pop <- demand.2045$pr_pop_2045
# demand.2050$pop <- demand.2050$pr_pop_2050
# demand.2055$pop <- demand.2055$pr_pop_2055
# demand.2060$pop <- demand.2060$pr_pop_2060
# demand.2065$pop <- demand.2065$pr_pop_2065
# demand.2070$pop <- demand.2070$pr_pop_2070
# 
# demand.ssp1 <- rbind(demand.ssp1, demand.2020, demand.2025, demand.2030,
#                      demand.2035, demand.2040, demand.2045, demand.2050,
#                      demand.2055, demand.2060, demand.2065, demand.2070)
# 
# # this function is not quite right
# demand.ssp1$dpu <- demand.ssp1$DP.perUnit * 
#   (1+demand.ssp1$DP.growth*(1+demand.ssp1$DP.decay)^(demand.ssp1$year-2015))^5
# 
# demand.ssp1$dpwd <- demand.ssp1$pop * demand.ssp1$DP.perUnit
# demand.ssp1$dpcu <- demand.ssp1$dpwd * demand.ssp1$DP.cu.ratio
# 
# ## NEED to add climate effects
# 
# demand.east <- subset(demand.ssp1, EastWest == "East")
# demand.west <- subset(demand.ssp1, EastWest == "West")
# dpu.summary <- aggregate(demand.ssp1[, c('dpu')], list(demand.ssp1$year), mean)
# dpu.east <- aggregate(demand.east[, c('dpu')], list(demand.east$year), mean)
# dpu.west <- aggregate(demand.west[, c('dpu')], list(demand.west$year), mean)
# 
# plot(dpu.east)
# plot(dpu.west)
# plot(dpu.summary)
# 
# ### ---- end Travis' attempt

#This function calculates dp projections out to 2070 using function from Tom's paper (Foti, Ramirez, Brown, 2010 FS RPA Assessment)

dp.project <- function(water1, pop1,climate)
{
water1 <- water1[,c(2,5,10,11,8)]


water1<-water1 %>% mutate(dp.wpu.2020 = DP.perUnit*(1+DP.growth*(1+DP.decay)^(2020-2015))^5 )
water1<-water1 %>% mutate(dp.wpu.2025 = dp.wpu.2020*(1+DP.growth*(1+DP.decay)^(2025-2015))^5 )
water1<-water1 %>% mutate(dp.wpu.2030 = dp.wpu.2025*(1+DP.growth*(1+DP.decay)^(2030-2015))^5 )
water1<-water1 %>% mutate(dp.wpu.2035 = dp.wpu.2030*(1+DP.growth*(1+DP.decay)^(2035-2015))^5 )
water1<-water1 %>% mutate(dp.wpu.2040 = dp.wpu.2035*(1+DP.growth*(1+DP.decay)^(2040-2015))^5 )
water1<-water1 %>% mutate(dp.wpu.2045 = dp.wpu.2040*(1+DP.growth*(1+DP.decay)^(2045-2015))^5 )
water1<-water1 %>% mutate(dp.wpu.2050 = dp.wpu.2045*(1+DP.growth*(1+DP.decay)^(2050-2015))^5 )
water1<-water1 %>% mutate(dp.wpu.2055 = dp.wpu.2050*(1+DP.growth*(1+DP.decay)^(2055-2015))^5 )
water1<-water1 %>% mutate(dp.wpu.2060 = dp.wpu.2055*(1+DP.growth*(1+DP.decay)^(2060-2015))^5 )
water1<-water1 %>% mutate(dp.wpu.2065 = dp.wpu.2060*(1+DP.growth*(1+DP.decay)^(2065-2015))^5 )
water1<-water1 %>% mutate(dp.wpu.2070 = dp.wpu.2065*(1+DP.growth*(1+DP.decay)^(2070-2015))^5 )
water1$sector <- "dp"

water1 <- water1[,c(1,17,5:16)]
colnames(water1) <- c("fips","sector", "Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
water1 <- linapprox(water1)

#fixing population fips codes and then merging with Domestic water use
pop1 <- fipsfix(pop1)

water1 <- merge(water1, pop1, by="fips")
# water1 <- water1[,c(1,5:16,2,18:28)]
water1 <- merge(water1, climate, by="fips")

for (n in 3:58){
  j <- n + 56
  k <- j +56
  m <- k +56
  water1[,n] <- as.numeric(water1[,n])
  water1[,j] <- as.numeric(water1[,j])
  water1[,n] <- (water1[,n] + water1[,k]- (water1[,m]*water1[,n] ))*water1[,j]
}
water1<-water1[,c(1:58)]
# colnames(water1) <- c("fips", "sector","Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
return(water1)
}

#-----------------------------------------
#CLIMATE EFFECTS 
#-----------------------------------------
# Definitions from function used in Tom's paper
# plantwater is "a factor for the change due to the direct effect of rising atmospheric
# CO2 levels on plant water use"
# precip is "change in effective precipitation, the portion of total growing 
# season precipitation that is useable by the plant, from 2005 to the future year, 
# in cm/y, computed at the basin scale"
# mup is "the change in DP gallons per capita per day withdrawn for a 1 cm increase in P'  from 2005 to a future year, a constant equal to ???1.415"
# et is "the change potential evapotranspiration during the growing season, from 2005 
# to the future year, in cm/y, computed at the basin scale"
# muet is "the change in DP gallons per capita per day withdrawn for a 1 cm increase in ETp from 2005 to a future year, a constant equal to 0.778"


#-----------------------------------------
#Bring in CO2 estimates, outdoor water use proportion from Tom
#

inputs<- read.csv("inputs_ssp1.csv")
inputs$Region <- "World"
inputs <- inputs[,c(2,28)]
co2.45 <-read.csv("R45_bulk.csv")
co2.85 <-read.csv("R85_bulk.csv")

co2.45 <- co2.45[co2.45$Region=="World",]
co2.45 <- co2.45[co2.45$Variable=="Concentration - CO2",]
co2.45 <- co2.45[,-c(2:6,17)]
climate.45 <- merge(inputs, co2.45, by="Region")
climate.45 <- climate.45[,-1]

co2.85 <- co2.85[co2.85$Region=="World",]
co2.85 <- co2.85[co2.85$Variable=="Concentration - CO2",]
co2.85 <- co2.85[,-c(2:6,17)]
climate.85 <- merge(inputs, co2.85, by="Region")
climate.85 <- climate.85[,-1]

#linear approximations of annual rather than 10 year intervals
fixclimate <- function(df){
  for (i in 1:6){
    for (n in 1:10){
    k <- (i-1)*10 +11 +n
    j <- 1+i
    m <- j + 1
    df[,k] <- df[,j]+(n*(df[,m]-df[,j])/10)
    }
  }
  df <- df[,c(1,16:71)]
  colnames(df) <- c("fips","plantwater15","plantwater16","plantwater17","plantwater18","plantwater19","plantwater20","plantwater21","plantwater22","plantwater23","plantwater24","plantwater25","plantwater26","plantwater27","plantwater28","plantwater29","plantwater30","plantwater31","plantwater32","plantwater33","plantwater34","plantwater35","plantwater36","plantwater37","plantwater38","plantwater39","plantwater40","plantwater41","plantwater42","plantwater43","plantwater44","plantwater45","plantwater46","plantwater47","plantwater48","plantwater49","plantwater50","plantwater51","plantwater52","plantwater53","plantwater54","plantwater55","plantwater56","plantwater57","plantwater58","plantwater59","plantwater60","plantwater61","plantwater62","plantwater63","plantwater64","plantwater65","plantwater66","plantwater67","plantwater68","plantwater69","plantwater70")
  for (i in 3:57){
    df[i] <- (df[,i]-df[,2])/1900
  }
  df[,2] <- 0
  return(df)
}
climate.45 <- fixclimate(climate.45)
climate.85 <- fixclimate(climate.85)

#climate.45 and climate.85 refer to RCP 45/85. This replaces the "plantwater" placeholder in the climate data frame below
#Note that each climate scenario from Hadi is run for both of these RCP scenarios.

outdoor <- cntypercent
outdoor$HUC_10 <- floor(outdoor$HUC_10/100000000)
outdoor <- outdoor %>% group_by(FIPS, HUC_10) %>% summarise(Shape_Area=sum(Shape_Area), CountyArea=mean(CountyArea), AreaHuc10=mean(AreaHuc10), CountyAreaPct=sum(CountyAreaPct), HU_10_NAME=sum(HU_10_NAME))
outdoor$CountyAreaPct <- outdoor$Shape_Area/outdoor$CountyArea
norm <- outdoor %>% group_by(FIPS) %>% summarise_all(sum)
norm <- norm[,c(1,3)]
outdoor <- merge(outdoor, norm, by="FIPS")
outdoor$Shape_Area.y <- outdoor$Shape_Area.y/outdoor$CountyArea
outdoor$CountyAreaPct <- outdoor$CountyAreaPct/outdoor$Shape_Area.y
outdoor <- outdoor[,c(1,2,6)]
outdoor$outprop <- ifelse(outdoor$HUC_10==1, 0.07,ifelse(outdoor$HUC_10==2, 0.03,ifelse(outdoor$HUC_10==3, .12,ifelse(outdoor$HUC_10==4, .1, ifelse(outdoor$HUC_10==5, .08,ifelse(outdoor$HUC_10==6, .08,ifelse(outdoor$HUC_10==7, .17,ifelse(outdoor$HUC_10==8, .14,ifelse(outdoor$HUC_10==9, .14,ifelse(outdoor$HUC_10==10, .33,ifelse(outdoor$HUC_10==11, .28,ifelse(outdoor$HUC_10==12, .21,ifelse(outdoor$HUC_10==13, .26,ifelse(outdoor$HUC_10==14, .44,ifelse(outdoor$HUC_10==15, .32,ifelse(outdoor$HUC_10==16, .48,ifelse(outdoor$HUC_10==17, .34,0.44 ) ) ) ) ) ) ) ) ) ) ) ) )) ) ) )

outdoor$outprop <- outdoor$outprop * outdoor$CountyAreaPct
outdoor <- outdoor[,c(1,4)] %>% group_by(FIPS) %>% summarise_all(sum)

check <- as.list(outdoor$FIPS)
check3 <- as.list(inputs$fips)
#climate data has but county does not
check1 <- check3[! check3 %in% check]
#county data has but climate does not
check2 <- check[!  check %in% check3]

outdoor <- outdoor[!outdoor$FIPS %in% check2,]
colnames(outdoor) <- c("fips","outprop")

#-----------------------------------------
#bringing in effective precipitation
#-----------------------------------------
#functions for effective precipitation
#converts to effective precipitation based on the function in Tom's paper. Note that  
# input data is mm/month and the output needs to be in cm/month, hence the dividing by 10.
effectiveprecip <- function(df){
  for (i in 2:length(colnames(df))){
    for (j in 1:length(rownames(df))){
      df[j,i] <- df[j,i] /10
      df[j,i] <- ifelse(df[j,i] >= 25, 12.5+0.1*df[j,i] , (df[j,i]*(12.5-0.2*df[j,i]))/12.5   )
    }
  }
  return(df)
}

county <- function(climate,cnty){
  forlater <- cnty
  #first check that both datasets have the same HUC8s 
  check <- as.list(colnames(cnty))
  check3 <- as.list(colnames(climate))
  #climate data has but county does not
  check1 <- check3[! check3 %in% check]
  #county data has but climate does not
  check2 <- check[!  check %in% as.list(colnames(climate))]
  # make sure dataframes are ordered correctly
  cnty <- cnty[,order(as.numeric(as.character(colnames(cnty))))]
  climate <- climate[,order(as.numeric(as.character(colnames(climate))))]
  #removing bad HUC8s
  fixed <- climate[,!(names(climate)%in% check1)]
  cnty <- cnty[,!(names(cnty)%in% check2)]
  
  
  #make dataframes into matrices
  fixed <- t(as.matrix(fixed))
  cnty <- as.matrix(cnty)
  
  fixed <- cnty %*% fixed
  fixed <- as.data.frame(t(fixed))
  fixed[,(length(colnames(fixed))+1)] <- climate[,c(length(colnames(climate)))]
  colnames(fixed) <- c(forlater$FIPS,"year")
  
  fixed <- fixed[,c(length(colnames(fixed)),1:(length(colnames(fixed))-1))]
  
  return(fixed)
}
#end functions
#------------------------------

#converting HUC to county and then checking against fips codes in data

converthuc <- cntypercent
converthuc$HUC_10 <-floor(signif(converthuc$HUC_10,8)/100)
converthuc <- converthuc[,c(1,4,7)] %>% group_by(FIPS, HUC_10) %>% summarise_all(sum)
converthuc <- spread(converthuc, "HUC_10", "CountyAreaPct")
converthuc[is.na(converthuc)] <- 0
converthuc$norm <- rowSums(converthuc[,c(2:2110)], na.rm=T)

converthuc[,c(2:2111)] <- sapply(converthuc[,c(2:2111)], function(x, y) x/(y), y=converthuc[,2111])

check <- as.list(converthuc$FIPS)
check3 <- as.list(inputs$fips)
check1 <- check3[! check3 %in% check]
check2 <- check[!  check %in% check3]

converthuc <- converthuc[!converthuc$FIPS %in% check2,]

converthuc <- converthuc[,c(1:2110)]
converthuc <- converthuc[,order(as.numeric(as.character(colnames(converthuc))))]

#converts precipitation to effective precipitation
converteffective <- function(df,converthuc){
  df <- subset(df, month >=4 & month <= 9) %>% group_by(year) %>% summarise_all(mean)
  df <- df[,-2]
  
  df <- county(df,converthuc)
  df <- as.data.frame(t(df))
  colnames(df) <- df[1,]
  df$fips <- rownames(df)
  df <- df[-1,]
  df <- df[,c(151,66:121)]
  
  df <- effectiveprecip(df)
  
  for(i in 3:length(colnames(df))){
    df[,i] <- df[,i] - df[,2]
  }
  df[,2] <- 0
  return(df)
}

#converts pet to the correct units
convertpet <- function(df,converthuc){
  df <- county(df,converthuc)
  df <- as.data.frame(t(df))
  colnames(df) <- df[1,]
  df$fips <- rownames(df)
  df <- df[-1,]
  df <- df[,c(151,66:121)]
  
  for (i in 2:length(colnames(df))){
    for (j in 1:length(rownames(df))){
      df[j,i] <- df[j,i]/10
    }
  }
  
  
  for(i in 3:length(colnames(df))){
    df[,i] <- df[,i] - df[,2]
  }
  df[,2] <- 0
  return(df)
}

precip <- converteffective(precip, converthuc)
colnames(precip) <- c("fips","precip15","precip16","precip17","precip18","precip19","precip20","precip21","precip22","precip23","precip24","precip25","precip26","precip27","precip28","precip29","precip30","precip31","precip32","precip33","precip34","precip35","precip36","precip37","precip38","precip39","precip40","precip41","precip42","precip43","precip44","precip45","precip46","precip47","precip48","precip49","precip50","precip51","precip52","precip53","precip54","precip55","precip56","precip57","precip58","precip59","precip60","precip61","precip62","precip63","precip64","precip65","precip66","precip67","precip68","precip69","precip70")

pet <- convertpet(pet,converthuc)
colnames(pet) <- c("fips","et15","et16","et17","et18","et19","et20","et21","et22","et23","et24","et25","et26","et27","et28","et29","et30","et31","et32","et33","et34","et35","et36","et37","et38","et39","et40","et41","et42","et43","et44","et45","et46","et47","et48","et49","et50","et51","et52","et53","et54","et55","et56","et57","et58","et59","et60","et61","et62","et63","et64","et65","et66","et67","et68","et69","et70")

#=====================================================================================================
#Below is an old section that was a place holder for climate effects

# placeholderdp <- function(climate1)
# {#this creates the placeholder dataframe that has FIPS, Precipitation data, Evapotranspiration data, and plantwater data.
#   climate1 <- climate1[,c(2,3)]
#   climate1$precip15 <-0 
#   climate1$precip20 <-0 
#   climate1$precip25 <-0 
#   climate1$precip30 <-0 
#   climate1$precip35 <-0 
#   climate1$precip40 <-0 
#   climate1$precip45 <-0 
#   climate1$precip50 <-0 
#   climate1$precip55 <-0 
#   climate1$precip60 <-0 
#   climate1$precip65 <-0 
#   climate1$precip70 <-0 
#   climate1$et15 <-0 
#   climate1$et20 <-0 
#   climate1$et25 <-0 
#   climate1$et30 <-0 
#   climate1$et35 <-0 
#   climate1$et40 <-0 
#   climate1$et45 <-0 
#   climate1$et50 <-0 
#   climate1$et55 <-0 
#   climate1$et60 <-0 
#   climate1$et65 <-0 
#   climate1$et70 <-0 
#   climate1$plantwater15 <-0 
#   climate1$plantwater20 <-0 
#   climate1$plantwater25 <-0 
#   climate1$plantwater30 <-0 
#   climate1$plantwater35 <-0 
#   climate1$plantwater40 <-0 
#   climate1$plantwater45 <-0 
#   climate1$plantwater50 <-0 
#   climate1$plantwater55 <-0 
#   climate1$plantwater60 <-0 
#   climate1$plantwater65 <-0 
#   climate1$plantwater70 <-0 
#   climate1 <-climate1[,c(1,3:38)]
#   return(climate1)
# }
#   
# climate <- read.csv("inputs_ssp1.csv")
# climate <- placeholderdp(climate)


#=====================================================================================================

#incorporate carbon effects into climate dataframe

climate <- merge(precip, pet, by="fips")
if (carbon==45){
climate <- merge(climate,climate.45, by="fips")
  }
if (carbon==85) {
climate <- merge(climate,climate.85, by="fips")
}

if (gcm=="base"){
  climate[,c(114:169)] <- 0
}

#NOTE: since withdrawal per unit for DP is per 1000 people, we divide by 1000 in our calculation
#this function differs slightly from the paper but produces the same results. We multiply by population and
#consumptive use proportion later.
climatedp <- function(climate1,outdoor1)
{
  for (n in 2:57){
    j <- n + 56
    climate1[,n] <- (climate1[,n]*(-1.415) +climate1[,j]*(0.778))/1000
  }
  
  climate1 <- climate1[,c(1:57,114:169)]
  colnames(climate1) <- c("fips","climate15","climate16","climate17","climate18","climate19","climate20","climate21","climate22","climate23","climate24","climate25","climate26","climate27","climate28","climate29","climate30","climate31","climate32","climate33","climate34","climate35","climate36","climate37","climate38","climate39","climate40","climate41","climate42","climate43","climate44","climate45","climate46","climate47","climate48","climate49","climate50","climate51","climate52","climate53","climate54","climate55","climate56","climate57","climate58","climate59","climate60","climate61","climate62","climate63","climate64","climate65","climate66","climate67","climate68","climate69","climate70",
                          "plantwater15","plantwater16","plantwater17","plantwater18","plantwater19","plantwater20","plantwater21","plantwater22","plantwater23","plantwater24","plantwater25","plantwater26","plantwater27","plantwater28","plantwater29","plantwater30","plantwater31","plantwater32","plantwater33","plantwater34","plantwater35","plantwater36","plantwater37","plantwater38","plantwater39","plantwater40","plantwater41","plantwater42","plantwater43","plantwater44","plantwater45","plantwater46","plantwater47","plantwater48","plantwater49","plantwater50","plantwater51","plantwater52","plantwater53","plantwater54","plantwater55","plantwater56","plantwater57","plantwater58","plantwater59","plantwater60","plantwater61","plantwater62","plantwater63","plantwater64","plantwater65","plantwater66","plantwater67","plantwater68","plantwater69","plantwater70")
  climate1 <-merge(climate1, outdoor1, by="fips")
  for (i in 58:113) {
    climate1[,i] <- climate1[,i] * climate1[,114]
  }
  return(climate1[,-114])
}
climate.dp <- climatedp(climate, outdoor)

#There is no explicit effect of climate on IC, however we include "change in consumption attributable to meeting renewable fuel standard goals" or "change in consumption needed for processing biofuels and producing liquid fuel from coal (CTL)"
#in order to do this, we need the annual volume of fuel produced and the level of water consumption per unit of fuel produced.
#the dataframes fuel1-5 below have the FIPS code and the amount of fuel produced in each year. For now this is zero.
#fuelcons.year is the amount of water consumption per unit of fuel for the year.

fuelcons.2015 <- 0
fuelcons.2020 <- 0
fuelcons.2025 <- 0
fuelcons.2030 <- 0
fuelcons.2035 <- 0
fuelcons.2040 <- 0
fuelcons.2045 <- 0
fuelcons.2050 <- 0
fuelcons.2055 <- 0
fuelcons.2060 <- 0
fuelcons.2065 <- 0
fuelcons.2070 <- 0

placeholder <- function(fuel1)
{
  fuel1 <- fuel1[,c(2,3)]
  fuel1$fuel2015 <-0 * fuelcons.2015
  fuel1$fuel2020 <-0 * fuelcons.2020
  fuel1$fuel2025 <-0 * fuelcons.2025
  fuel1$fuel2030 <-0 * fuelcons.2030
  fuel1$fuel2035 <-0 * fuelcons.2035
  fuel1$fuel2040 <-0 * fuelcons.2040
  fuel1$fuel2045 <-0 * fuelcons.2045
  fuel1$fuel2050 <-0 * fuelcons.2050
  fuel1$fuel2055 <-0 * fuelcons.2055
  fuel1$fuel2060 <-0 * fuelcons.2060
  fuel1$fuel2065 <-0 * fuelcons.2065
  fuel1$fuel2070 <-0 * fuelcons.2070

  fuel1 <- fuel1[,c(1,3:14)]
  return(fuel1)
}

fuel <- read.csv("inputs_ssp1.csv")
fuel <- placeholder(fuel)

#----------------------------------------------------------------------------------------------------

#bring in water inputs, population, and income

water1 <- read.csv("inputs_ssp1.csv")
pop1 <- read.csv("pop_ssp1.csv")
dp.ssp1 <- dp.project(water1,pop1,climate.dp)

water2 <- read.csv("inputs_ssp2.csv")
pop2 <- read.csv("pop_ssp2.csv")
dp.ssp2 <- dp.project(water2,pop2,climate.dp)

water3 <- read.csv("inputs_ssp3.csv")
pop3 <- read.csv("pop_ssp3.csv")
dp.ssp3 <- dp.project(water3,pop3,climate.dp)

water4 <- read.csv("inputs_ssp4.csv")
pop4 <- read.csv("pop_ssp4.csv")
dp.ssp4 <- dp.project(water4,pop4,climate.dp)

water5 <- read.csv("inputs_ssp5.csv")
pop5 <- read.csv("pop_ssp5.csv")
dp.ssp5 <- dp.project(water5,pop5,climate.dp)

inc1 <- read.csv("pipc_ssp1.csv")
inc2 <- read.csv("pipc_ssp2.csv")
inc3 <- read.csv("pipc_ssp3.csv")
inc4 <- read.csv("pipc_ssp4.csv")
inc5 <- read.csv("pipc_ssp5.csv")

# this function changes the units for income from income per capita 
# to total income in the county (income per capita * population)

incomeunit <- function(inc1, pop1)
{
  pop1 <- pop1[,c(1,2,7,12,17,22,27,32,37,42,47,52,57)]
  inc1 <- inc1[,c(1,2,7,12,17,22,27,32,37,42,47,52,57)]
  
  inc1 <- merge(inc1, pop1, by="fips")
  for (n in 2:13){
    j <- n + 12
    inc1[,n] <- as.numeric(inc1[,n])
    inc1[,j] <- as.numeric(inc1[,j])
    inc1[,n] <- inc1[,n]*inc1[,j]
  }
  inc1 <- fipsfix(inc1)
  inc1<-inc1[,c(1:13)]
  colnames(inc1) <- c("fips", "inc2015","inc2020","inc2025","inc2030","inc2035","inc2040","inc2045","inc2050","inc2055","inc2060","inc2065","inc2070")
 
  return(inc1)
}

inc1 <- incomeunit(inc1,pop1)
inc2 <- incomeunit(inc2,pop2)
inc3 <- incomeunit(inc3,pop3)
inc4 <- incomeunit(inc4,pop4)
inc5 <- incomeunit(inc5,pop5)

#This function calculates ic projections out to 2070

ic.project <- function(water1, pop1, fuel1)
{
  water1 <- water1[,c(2,6,16,20,12)]
  
  water1<-water1 %>% mutate(ic.wpu.2020 = IC.perUnit*(1+IC.growth*(1+IC.decay)^(2020-2015))^5 )
  water1<-water1 %>% mutate(ic.wpu.2025 = ic.wpu.2020*(1+IC.growth*(1+IC.decay)^(2025-2015))^5 )
  water1<-water1 %>% mutate(ic.wpu.2030 = ic.wpu.2025*(1+IC.growth*(1+IC.decay)^(2030-2015))^5 )
  water1<-water1 %>% mutate(ic.wpu.2035 = ic.wpu.2030*(1+IC.growth*(1+IC.decay)^(2035-2015))^5 )
  water1<-water1 %>% mutate(ic.wpu.2040 = ic.wpu.2035*(1+IC.growth*(1+IC.decay)^(2040-2015))^5 )
  water1<-water1 %>% mutate(ic.wpu.2045 = ic.wpu.2040*(1+IC.growth*(1+IC.decay)^(2045-2015))^5 )
  water1<-water1 %>% mutate(ic.wpu.2050 = ic.wpu.2045*(1+IC.growth*(1+IC.decay)^(2050-2015))^5 )
  water1<-water1 %>% mutate(ic.wpu.2055 = ic.wpu.2050*(1+IC.growth*(1+IC.decay)^(2055-2015))^5 )
  water1<-water1 %>% mutate(ic.wpu.2060 = ic.wpu.2055*(1+IC.growth*(1+IC.decay)^(2060-2015))^5 )
  water1<-water1 %>% mutate(ic.wpu.2065 = ic.wpu.2060*(1+IC.growth*(1+IC.decay)^(2065-2015))^5 )
  water1<-water1 %>% mutate(ic.wpu.2070 = ic.wpu.2065*(1+IC.growth*(1+IC.decay)^(2070-2015))^5 )
  
  
  water1 <- merge(water1, pop1, by="fips")
  water1 <- water1[,c(1,5:16,2,18:28)]
  water1 <- merge(water1, fuel1, by="fips")
  for (n in 2:13){
    j <- n + 12
    k <- j + 12
    water1[,n] <- as.numeric(water1[,n])
    water1[,j] <- as.numeric(water1[,j])
    water1[,n] <- water1[,n]*water1[,j] + water1[,k]
  }
  water1<-water1[,c(1:13)]
  colnames(water1) <- c("fips", "Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
  water1$sector <- "ic"
  water1 <- water1[,c(1,14,2:13)]
  return(water1)
}

ic.ssp1 <- ic.project(water1,inc1,fuel)
ic.ssp2 <- ic.project(water2,inc2,fuel)
ic.ssp3 <- ic.project(water3,inc3,fuel)
ic.ssp4 <- ic.project(water4,inc4,fuel)
ic.ssp5 <- ic.project(water5,inc5,fuel)

# This section calculates ir projections out to 2070. We make the assumption 
# that infinite withdrawl per unit is 0, or NA values are 0.
# Infinite or NA values arise because some counties have small number of 
# irrigation withdrawals and no irrigated acres. Converting to withdrawal 
# per unit means this is infinite.

acre <- read.csv("acredata-updated.csv")
ir <- water1[,c(2,15,19,23,27)]
is.na(ir)<-sapply(ir, is.infinite)
ir[is.na(ir)] <- 0
ir.climate <- ir[,c(1,5)]

ir<-ir %>% mutate(ir.wpu.2020 = IR.perUnit*(1+IR.growth*(1+IR.decay)^(2020-2015))^5 )
ir<-ir %>% mutate(ir.wpu.2025 = ir.wpu.2020*(1+IR.growth*(1+IR.decay)^(2025-2015))^5 )
ir<-ir %>% mutate(ir.wpu.2030 = ir.wpu.2025*(1+IR.growth*(1+IR.decay)^(2030-2015))^5 )
ir<-ir %>% mutate(ir.wpu.2035 = ir.wpu.2030*(1+IR.growth*(1+IR.decay)^(2035-2015))^5 )
ir<-ir %>% mutate(ir.wpu.2040 = ir.wpu.2035*(1+IR.growth*(1+IR.decay)^(2040-2015))^5 )
ir<-ir %>% mutate(ir.wpu.2045 = ir.wpu.2040*(1+IR.growth*(1+IR.decay)^(2045-2015))^5 )
ir<-ir %>% mutate(ir.wpu.2050 = ir.wpu.2045*(1+IR.growth*(1+IR.decay)^(2050-2015))^5 )
ir<-ir %>% mutate(ir.wpu.2055 = ir.wpu.2050*(1+IR.growth*(1+IR.decay)^(2055-2015))^5 )
ir<-ir %>% mutate(ir.wpu.2060 = ir.wpu.2055*(1+IR.growth*(1+IR.decay)^(2060-2015))^5 )
ir<-ir %>% mutate(ir.wpu.2065 = ir.wpu.2060*(1+IR.growth*(1+IR.decay)^(2065-2015))^5 )
ir<-ir %>% mutate(ir.wpu.2070 = ir.wpu.2065*(1+IR.growth*(1+IR.decay)^(2070-2015))^5 )


ir <- merge(ir, acre, by="fips")

# exporting to work with this in Excel
write.csv(ir, 'irout.csv')


# ag withdrawals - this was done by an indexed loop
ir<-ir %>% mutate(ir.wd.2020 = ir.wpu.2020*pr_acre_2020)
ir<-ir %>% mutate(ir.wd.2025 = ir.wpu.2025*pr_acre_2025)
ir<-ir %>% mutate(ir.wd.2030 = ir.wpu.2030*pr_acre_2030)
ir<-ir %>% mutate(ir.wd.2035 = ir.wpu.2035*pr_acre_2035)
ir<-ir %>% mutate(ir.wd.2040 = ir.wpu.2040*pr_acre_2040)
ir<-ir %>% mutate(ir.wd.2045 = ir.wpu.2045*pr_acre_2045)
ir<-ir %>% mutate(ir.wd.2050 = ir.wpu.2050*pr_acre_2050)
ir<-ir %>% mutate(ir.wd.2055 = ir.wpu.2055*pr_acre_2055)
ir<-ir %>% mutate(ir.wd.2060 = ir.wpu.2060*pr_acre_2060)
ir<-ir %>% mutate(ir.wd.2065 = ir.wpu.2065*pr_acre_2065)
ir<-ir %>% mutate(ir.wd.2070 = ir.wpu.2070*pr_acre_2070)

# 
# ir <- ir[,c(1,2,5,6:16,18:29)]
# 
# for (n in 3:14){
#   j <- n + 12
#   ir[,n] <- as.numeric(ir[,n])
#   ir[,j] <- as.numeric(ir[,j])
#   ir[,n] <- (ir[,n])*ir[,j]
# }

ir$sector <- "ir"

# I think the columns have to be in a specific format for linear approximation to work
# there were some errors around here, so I'm just going brute force calculations

### Currently missing eithdrawals for 2015 in the dataframe

# DF$Y2016 <- DF$Y2015 + 1*(DF$Y2020 - DF$Y2015)/5
# DF$Y2017 <- DF$Y2015 + 2*(DF$Y2020 - DF$Y2015)/5
# DF$Y2018 <- DF$Y2015 + 3*(DF$Y2020 - DF$Y2015)/5
# DF$Y2019 <- DF$Y2015 + 4*(DF$Y2020 - DF$Y2015)/5

ir<-ir %>% mutate(ir.wd.2021 = ir.wd.2020 + (1/5)*(ir.wd.2025 - ir.wd.2020))
ir<-ir %>% mutate(ir.wd.2022 = ir.wd.2020 + (2/5)*(ir.wd.2025 - ir.wd.2020))
ir<-ir %>% mutate(ir.wd.2023 = ir.wd.2020 + (3/5)*(ir.wd.2025 - ir.wd.2020))
ir<-ir %>% mutate(ir.wd.2024 = ir.wd.2020 + (4/5)*(ir.wd.2025 - ir.wd.2020))

ir<-ir %>% mutate(ir.wd.2026 = ir.wd.2025 + (1/5)*(ir.wd.2030 - ir.wd.2025))
ir<-ir %>% mutate(ir.wd.2027 = ir.wd.2025 + (2/5)*(ir.wd.2030 - ir.wd.2025))
ir<-ir %>% mutate(ir.wd.2028 = ir.wd.2025 + (3/5)*(ir.wd.2030 - ir.wd.2025))
ir<-ir %>% mutate(ir.wd.2029 = ir.wd.2025 + (4/5)*(ir.wd.2030 - ir.wd.2025))

ir<-ir %>% mutate(ir.wd.2031 = ir.wd.2030 + (1/5)*(ir.wd.2035 - ir.wd.2030))
ir<-ir %>% mutate(ir.wd.2032 = ir.wd.2030 + (2/5)*(ir.wd.2035 - ir.wd.2030))
ir<-ir %>% mutate(ir.wd.2033 = ir.wd.2030 + (3/5)*(ir.wd.2035 - ir.wd.2030))
ir<-ir %>% mutate(ir.wd.2034 = ir.wd.2030 + (4/5)*(ir.wd.2035 - ir.wd.2030))

ir<-ir %>% mutate(ir.wd.2036 = ir.wd.2035 + (1/5)*(ir.wd.2040 - ir.wd.2035))
ir<-ir %>% mutate(ir.wd.2037 = ir.wd.2035 + (2/5)*(ir.wd.2040 - ir.wd.2035))
ir<-ir %>% mutate(ir.wd.2038 = ir.wd.2035 + (3/5)*(ir.wd.2040 - ir.wd.2035))
ir<-ir %>% mutate(ir.wd.2039 = ir.wd.2035 + (4/5)*(ir.wd.2040 - ir.wd.2035))

ir<-ir %>% mutate(ir.wd.2041 = ir.wd.2040 + (1/5)*(ir.wd.2045 - ir.wd.2040))
ir<-ir %>% mutate(ir.wd.2042 = ir.wd.2040 + (2/5)*(ir.wd.2045 - ir.wd.2040))
ir<-ir %>% mutate(ir.wd.2043 = ir.wd.2040 + (3/5)*(ir.wd.2045 - ir.wd.2040))
ir<-ir %>% mutate(ir.wd.2044 = ir.wd.2040 + (4/5)*(ir.wd.2045 - ir.wd.2040))

ir<-ir %>% mutate(ir.wd.2046 = ir.wd.2045 + (1/5)*(ir.wd.2050 - ir.wd.2045))
ir<-ir %>% mutate(ir.wd.2047 = ir.wd.2045 + (2/5)*(ir.wd.2050 - ir.wd.2045))
ir<-ir %>% mutate(ir.wd.2048 = ir.wd.2045 + (3/5)*(ir.wd.2050 - ir.wd.2045))
ir<-ir %>% mutate(ir.wd.2049 = ir.wd.2045 + (4/5)*(ir.wd.2050 - ir.wd.2045))

ir<-ir %>% mutate(ir.wd.2051 = ir.wd.2050 + (1/5)*(ir.wd.2055 - ir.wd.2050))
ir<-ir %>% mutate(ir.wd.2052 = ir.wd.2050 + (2/5)*(ir.wd.2055 - ir.wd.2050))
ir<-ir %>% mutate(ir.wd.2053 = ir.wd.2050 + (3/5)*(ir.wd.2055 - ir.wd.2050))
ir<-ir %>% mutate(ir.wd.2054 = ir.wd.2050 + (4/5)*(ir.wd.2055 - ir.wd.2050))

ir<-ir %>% mutate(ir.wd.2056 = ir.wd.2055 + (1/5)*(ir.wd.2060 - ir.wd.2055))
ir<-ir %>% mutate(ir.wd.2057 = ir.wd.2055 + (2/5)*(ir.wd.2060 - ir.wd.2055))
ir<-ir %>% mutate(ir.wd.2058 = ir.wd.2055 + (3/5)*(ir.wd.2060 - ir.wd.2055))
ir<-ir %>% mutate(ir.wd.2059 = ir.wd.2055 + (4/5)*(ir.wd.2060 - ir.wd.2055))

ir<-ir %>% mutate(ir.wd.2061 = ir.wd.2060 + (1/5)*(ir.wd.2065 - ir.wd.2060))
ir<-ir %>% mutate(ir.wd.2062 = ir.wd.2060 + (2/5)*(ir.wd.2065 - ir.wd.2060))
ir<-ir %>% mutate(ir.wd.2063 = ir.wd.2060 + (3/5)*(ir.wd.2065 - ir.wd.2060))
ir<-ir %>% mutate(ir.wd.2064 = ir.wd.2060 + (4/5)*(ir.wd.2065 - ir.wd.2060))

ir<-ir %>% mutate(ir.wd.2066 = ir.wd.2065 + (1/5)*(ir.wd.2070 - ir.wd.2065))
ir<-ir %>% mutate(ir.wd.2067 = ir.wd.2065 + (2/5)*(ir.wd.2070 - ir.wd.2065))
ir<-ir %>% mutate(ir.wd.2068 = ir.wd.2065 + (3/5)*(ir.wd.2070 - ir.wd.2065))
ir<-ir %>% mutate(ir.wd.2069 = ir.wd.2065 + (4/5)*(ir.wd.2070 - ir.wd.2065))


# ir <- ir[,c(1,27,3:14)]
# colnames(ir) <- c("fips","sector", "Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
# ir <- linapprox(ir)


# ir <- merge(ir, acre, by="fips")
# ir <- ir[,-59]
# for (n in 59:69){
#   j <- n+1
#   for (r in 1:5){
#   k <- 5*(n-59) + 70 + r
#   ir[,k] <- ir[,n] + r*(ir[,j]-ir[,n])/5
#   }
# }
# 

# Definitions from function used in Tom's paper
# plantwater is "a factor for the change due to the direct effect of rising atmospheric
# CO2 levels on plant water use"
# precip is "change in effective precipitation, the portion of total growing 
# season precipitation that is useable by the plant, from 2005 to the future year, 
# in cm/y, computed at the basin scale"
# et is "the change potential evapotranspiration during the growing season, from 2005 
# to the future year, in cm/y, computed at the basin scale"

# incorporate climate effects for irrigation sector, this function comes from 
# Tom's paper

#-----------------------------------------
#bringing in effective precipitation
#-----------------------------------------
#functions for effective precipitation
#converts to effective precipitation based on the function in Tom's paper. Note that  
# effectiveprecip <- function(df){
#   for (i in 2:length(colnames(df))){
#     for (j in 1:length(rownames(df))){
#       df[j,i] <- df[j,i] /10
#       df[j,i] <- ifelse(df[j,i] >= 25, 12.5+0.1*df[j,i] , (df[j,i]*(12.5-0.2*df[j,i]))/12.5   )
#     }
#   }
#   return(df)
# }

length(colnames(precip))

# input data is mm/month and the output needs to be in cm/month, 
# hence the dividing by 10.
for (i in 2:length(colnames(precip))){
  for (j in 1:length(rownames(precip))){
    precip[j,i] <- precip[j,i] /10
#    precip[j,i] <- ifelse(precip[j,i] >= 25, 12.5+0.1*precip[j,i] , (precip[j,i]*(12.5-0.2*df[j,i]))/12.5   )
  }}

# See 2010 Assessment. This is the proportion of precip usable by plants
# this is p.prime in the text. Maybe change name here
# This is usable precip, so I think I need to compare what is usable with historic?
# In the 2010 Assessment, it reads, p' is the part that does not need to be met by
# irrigation. So (p1' - p2') is new crop needs
for (i in 2:length(colnames(precip))){
  for (j in 1:length(rownames(precip))){
#    precip[j,i] <- precip[j,i] /10
        precip[j,i] <- ifelse(precip[j,i] >= 25, 12.5+0.1*precip[j,i] , (precip[j,i]*(12.5-0.2*precip[j,i]))/12.5   )
  }}

county <- function(climate,cnty){
  forlater <- cnty
  #first check that both datasets have the same HUC8s 
  check <- as.list(colnames(cnty))
  check3 <- as.list(colnames(climate))
  #climate data has but county does not
  check1 <- check3[! check3 %in% check]
  #county data has but climate does not
  check2 <- check[!  check %in% as.list(colnames(climate))]
  # make sure dataframes are ordered correctly
  cnty <- cnty[,order(as.numeric(as.character(colnames(cnty))))]
  climate <- climate[,order(as.numeric(as.character(colnames(climate))))]
  #removing bad HUC8s
  fixed <- climate[,!(names(climate)%in% check1)]
  cnty <- cnty[,!(names(cnty)%in% check2)]
  
  
  #make dataframes into matrices
  fixed <- t(as.matrix(fixed))
  cnty <- as.matrix(cnty)
  
  fixed <- cnty %*% fixed
  fixed <- as.data.frame(t(fixed))
  fixed[,(length(colnames(fixed))+1)] <- climate[,c(length(colnames(climate)))]
  colnames(fixed) <- c(forlater$FIPS,"year")
  
  fixed <- fixed[,c(length(colnames(fixed)),1:(length(colnames(fixed))-1))]
  
  return(fixed)
}

#converts precipitation to effective precipitation

precip.county <- subset(precip, month >=4 & month <= 9) %>% group_by(year) %>% summarise_all(mean)
# months are weird decimals - not quite right


  df <- df[,-2]
  
  df <- county(df,converthuc)
  df <- as.data.frame(t(df))
  colnames(df) <- df[1,]
  df$fips <- rownames(df)
  df <- df[-1,]
  df <- df[,c(151,66:121)]
  
  df <- effectiveprecip(df)
  
  for(i in 3:length(colnames(df))){
    df[,i] <- df[,i] - df[,2]
  }
  df[,2] <- 0
  return(df)
}

#converts pet to the correct units
convertpet <- function(df,converthuc){
  df <- county(df,converthuc)
  df <- as.data.frame(t(df))
  colnames(df) <- df[1,]
  df$fips <- rownames(df)
  df <- df[-1,]
  df <- df[,c(151,66:121)]
  
  for (i in 2:length(colnames(df))){
    for (j in 1:length(rownames(df))){
      df[j,i] <- df[j,i]/10
    }
  }
  
  
  for(i in 3:length(colnames(df))){
    df[,i] <- df[,i] - df[,2]
  }
  df[,2] <- 0
  return(df)
}

precip <- converteffective(precip, converthuc)
colnames(precip) <- c("fips","precip15","precip16","precip17","precip18","precip19","precip20","precip21","precip22","precip23","precip24","precip25","precip26","precip27","precip28","precip29","precip30","precip31","precip32","precip33","precip34","precip35","precip36","precip37","precip38","precip39","precip40","precip41","precip42","precip43","precip44","precip45","precip46","precip47","precip48","precip49","precip50","precip51","precip52","precip53","precip54","precip55","precip56","precip57","precip58","precip59","precip60","precip61","precip62","precip63","precip64","precip65","precip66","precip67","precip68","precip69","precip70")

pet <- convertpet(pet,converthuc)
colnames(pet) <- c("fips","et15","et16","et17","et18","et19","et20","et21","et22","et23","et24","et25","et26","et27","et28","et29","et30","et31","et32","et33","et34","et35","et36","et37","et38","et39","et40","et41","et42","et43","et44","et45","et46","et47","et48","et49","et50","et51","et52","et53","et54","et55","et56","et57","et58","et59","et60","et61","et62","et63","et64","et65","et66","et67","et68","et69","et70")




climateir <- function(climate1)
{
  for (n in 2:57){
    j <- n + 56
    k <- j + 56
    climate1[,n] <- (-climate1[,n]) + (climate1[,j])*(1-climate1[,k])
  }
  
  climate1 <- climate1[,c(1:57,114:169)]
#  colnames(climate1) <- c("fips","climate15","climate16","climate17","climate18","climate19","climate20","climate21","climate22","climate23","climate24","climate25","climate26","climate27","climate28","climate29","climate30","climate31","climate32","climate33","climate34","climate35","climate36","climate37","climate38","climate39","climate40","climate41","climate42","climate43","climate44","climate45","climate46","climate47","climate48","climate49","climate50","climate51","climate52","climate53","climate54","climate55","climate56","climate57","climate58","climate59","climate60","climate61","climate62","climate63","climate64","climate65","climate66","climate67","climate68","climate69","climate70",
#                          "plantwater15","plantwater16","plantwater17","plantwater18","plantwater19","plantwater20","plantwater21","plantwater22","plantwater23","plantwater24","plantwater25","plantwater26","plantwater27","plantwater28","plantwater29","plantwater30","plantwater31","plantwater32","plantwater33","plantwater34","plantwater35","plantwater36","plantwater37","plantwater38","plantwater39","plantwater40","plantwater41","plantwater42","plantwater43","plantwater44","plantwater45","plantwater46","plantwater47","plantwater48","plantwater49","plantwater50","plantwater51","plantwater52","plantwater53","plantwater54","plantwater55","plantwater56","plantwater57","plantwater58","plantwater59","plantwater60","plantwater61","plantwater62","plantwater63","plantwater64","plantwater65","plantwater66","plantwater67","plantwater68","plantwater69","plantwater70")
  return(climate1)
}
climate.ir <- climateir(climate)




ir <- ir[,c(1:59,71:125)]
ir <-merge(ir,climate.ir, by="fips")
ir <- merge(ir,ir.climate, by="fips")
for (n in 115:170){
  ir[,n] <- ir[,n]/ir[,227]
}


# climate impacts on wpu
ir<-ir %>% mutate(ir.wpuc.2020 = ir.wpu.2020 * 1)
ir<-ir %>% mutate(ir.wpuc.2025 = ir.wpu.2020)
ir<-ir %>% mutate(ir.wpuc.2030 = ir.wpu.2025)
ir<-ir %>% mutate(ir.wpuc.2035 = ir.wpu.2030)
ir<-ir %>% mutate(ir.wpuc.2040 = ir.wpu.2035)
ir<-ir %>% mutate(ir.wpuc.2045 = ir.wpu.2040)
ir<-ir %>% mutate(ir.wpuc.2050 = ir.wpu.2045)
ir<-ir %>% mutate(ir.wpuc.2055 = ir.wpu.2050)
ir<-ir %>% mutate(ir.wpuc.2060 = ir.wpu.2055)
ir<-ir %>% mutate(ir.wpuc.2065 = ir.wpu.2060)
ir<-ir %>% mutate(ir.wpuc.2070 = ir.wpu.2065)





ir <- ir[,-c(171:227)]

for (n in 3:58){
  j <- n + 56
  k <- j +56
  ir[,n] <- as.numeric(ir[,n])
  ir[,j] <- as.numeric(ir[,j])
  ir[,n] <- ir[,n]+ (ir[,k]*ir[,j])
}
ir<-ir[,c(1:58)]

## At this point, there are large values and zeros. I think this is where 
# things are wrong

# ------------------------------------------------------------------
# ------------------------------------------------------------------

for (n in 3:length(colnames(ir))){
  for (j in 1:length(rownames(ir))){
    ir[j,n] <-ifelse(ir[j,n]<0, 0, ir[j,n])
  }
}

#----------------------------------------------------------------------------------------------------------------------
# creates dataframes for each sector that is calculated above (dp, ic, ir).
# Equation used previously outputs every 5 years. This section also makes 
# linear approximations in between these 5 years in order to have annual data. 
#----------------------------------------------------------------------------------------------------------------------

#----- ic

ic.ssp1 <- linapprox(ic.ssp1)
ic.ssp2 <- linapprox(ic.ssp2)
ic.ssp3 <- linapprox(ic.ssp3)
ic.ssp4 <- linapprox(ic.ssp4)
ic.ssp5 <- linapprox(ic.ssp5)

#----------------------------------------------------------------------------------------------------------------------
#Calculating aquaculture and livestock withdrawal per unit for every 5 years. This section also creates linear apprximations in between those 5 years.
#First, we calculate WRR population, and WRR aquaculture and livestock withdrawal per unit. Growth and Decay rates are at the WRR level, so 
#we project at the WRR level and then convert it to county level.
#----------------------------------------------------------------------------------------------------------------------

#SSP1

#calculate proportion of FIPS in each WRR
df<-cntypercent
df$WRR <-floor(signif(df$HUC_10,2)/100000000)
df<-df[,c(1:3,8,6)]
df <- df %>% group_by(FIPS,WRR) %>% summarise(Shape_Area=sum(Shape_Area),CountyArea=mean(CountyArea))
df$pctwrr <- df$Shape_Area/df$CountyArea
df1 <- df[,c(1,5)]
df1 <- df1 %>% group_by(FIPS) %>% summarise_all(funs(sum))

df <- merge(df, df1, by="FIPS")
df$pctwrr.x <- df$pctwrr.x/df$pctwrr.y
df<- df[,c(1,2,5)]
colnames(df)[colnames(df)=="FIPS"] <- "fips"
proj <-df

#input water use data from "WaterUsedatacleanup.R"

water1 <- read.csv(file="inputs_ssp1.csv")
water1 <- water1[,c(2,5,13,14)]
water1$AQ.perUnit <- water1$AQ.perUnit*water1$pop
water1$LS.perUnit <- water1$LS.perUnit*water1$pop
colnames(water1)[colnames(water1)=="AQ.perUnit"] <- "AQ.wd.tot"
colnames(water1)[colnames(water1)=="LS.perUnit"] <- "LS.wd.tot"

#first, we calculate population and wpu at the WRR level

pop <- read.csv("pop_ssp1.csv")
df <- merge(df, pop, by='fips')
df <- merge(df, water1, by='fips')

df <- df %>% mutate(pr_pop_2015=pr_pop_2015 * pctwrr.x)
df <- df %>% mutate(pr_pop_2020=pr_pop_2020 * pctwrr.x)
df <- df %>% mutate(pr_pop_2025=pr_pop_2025 * pctwrr.x)
df <- df %>% mutate(pr_pop_2030=pr_pop_2030 * pctwrr.x)
df <- df %>% mutate(pr_pop_2035=pr_pop_2035 * pctwrr.x)
df <- df %>% mutate(pr_pop_2040=pr_pop_2040 * pctwrr.x)
df <- df %>% mutate(pr_pop_2045=pr_pop_2045 * pctwrr.x)
df <- df %>% mutate(pr_pop_2050=pr_pop_2050 * pctwrr.x)
df <- df %>% mutate(pr_pop_2055=pr_pop_2055 * pctwrr.x)
df <- df %>% mutate(pr_pop_2060=pr_pop_2060 * pctwrr.x)
df <- df %>% mutate(pr_pop_2065=pr_pop_2065 * pctwrr.x)
df <- df %>% mutate(pr_pop_2070=pr_pop_2070 * pctwrr.x)
df <- df %>% mutate(AQ.wd.tot=AQ.wd.tot * pctwrr.x)
df <- df %>% mutate(LS.wd.tot=LS.wd.tot * pctwrr.x)
df <- df %>% mutate(pop =pop*pctwrr.x)

df <- df %>% group_by(WRR) %>% summarize_all(funs(sum))

#calculate WRR wpu for LS and AQ
df$wrr.aq.wpu <- df$AQ.wd.tot/df$pop
df$wrr.ls.wpu <- df$LS.wd.tot/df$pop

df$wrr.aq.growth <- ifelse(df$WRR<=9, 0.0336,0.0829)
df$wrr.aq.decay <- ifelse(df$WRR<=9, -0.05,-.1)
df$wrr.ls.growth <- ifelse(df$WRR<=9, -0.0113,-0.0221)
df$wrr.ls.decay <- ifelse(df$WRR<=9, -0.04,-0.04)

df$wrr.aq.wd.2020 <- NA
df$wrr.aq.wd.2025 <- NA
df$wrr.aq.wd.2030 <- NA
df$wrr.aq.wd.2035 <- NA
df$wrr.aq.wd.2040 <- NA
df$wrr.aq.wd.2045 <- NA
df$wrr.aq.wd.2050 <- NA
df$wrr.aq.wd.2055 <- NA
df$wrr.aq.wd.2060 <- NA
df$wrr.aq.wd.2065 <- NA
df$wrr.aq.wd.2070 <- NA

df$wrr.ls.wd.2020 <- NA
df$wrr.ls.wd.2025 <- NA
df$wrr.ls.wd.2030 <- NA
df$wrr.ls.wd.2035 <- NA
df$wrr.ls.wd.2040 <- NA
df$wrr.ls.wd.2045 <- NA
df$wrr.ls.wd.2050 <- NA
df$wrr.ls.wd.2055 <- NA
df$wrr.ls.wd.2060 <- NA
df$wrr.ls.wd.2065 <- NA
df$wrr.ls.wd.2070 <- NA

#calculate projected wpu
df<-df %>% mutate(wrr.aq.wd.2020 = wrr.aq.wpu*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2020-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2025 = wrr.aq.wd.2020*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2025-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2030 = wrr.aq.wd.2025*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2030-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2035 = wrr.aq.wd.2030*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2035-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2040 = wrr.aq.wd.2035*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2040-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2045 = wrr.aq.wd.2040*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2045-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2050 = wrr.aq.wd.2045*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2050-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2055 = wrr.aq.wd.2050*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2055-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2060 = wrr.aq.wd.2055*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2060-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2065 = wrr.aq.wd.2060*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2065-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2070 = wrr.aq.wd.2065*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2070-2015))^5 )

df<-df %>% mutate(wrr.ls.wd.2020 = wrr.ls.wpu*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2020-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2025 = wrr.ls.wd.2020*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2025-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2030 = wrr.ls.wd.2025*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2030-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2035 = wrr.ls.wd.2030*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2035-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2040 = wrr.ls.wd.2035*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2040-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2045 = wrr.ls.wd.2040*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2045-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2050 = wrr.ls.wd.2045*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2050-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2055 = wrr.ls.wd.2050*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2055-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2060 = wrr.ls.wd.2055*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2060-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2065 = wrr.ls.wd.2060*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2065-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2070 = wrr.ls.wd.2065*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2070-2015))^5 )

#calculate actual wd
df<-df %>% mutate(wrr.aq.wpu = wrr.aq.wpu*pr_pop_2015 )
df<-df %>% mutate(wrr.aq.wd.2020 = wrr.aq.wd.2020*pr_pop_2020 )
df<-df %>% mutate(wrr.aq.wd.2025 = wrr.aq.wd.2025*pr_pop_2025 )
df<-df %>% mutate(wrr.aq.wd.2030 = wrr.aq.wd.2030*pr_pop_2030 )
df<-df %>% mutate(wrr.aq.wd.2035 = wrr.aq.wd.2035*pr_pop_2035 )
df<-df %>% mutate(wrr.aq.wd.2040 = wrr.aq.wd.2040*pr_pop_2040 )
df<-df %>% mutate(wrr.aq.wd.2045 = wrr.aq.wd.2045*pr_pop_2045 )
df<-df %>% mutate(wrr.aq.wd.2050 = wrr.aq.wd.2050*pr_pop_2050 )
df<-df %>% mutate(wrr.aq.wd.2055 = wrr.aq.wd.2055*pr_pop_2055 )
df<-df %>% mutate(wrr.aq.wd.2060 = wrr.aq.wd.2060*pr_pop_2060 )
df<-df %>% mutate(wrr.aq.wd.2065 = wrr.aq.wd.2065*pr_pop_2065 )
df<-df %>% mutate(wrr.aq.wd.2070 = wrr.aq.wd.2070*pr_pop_2070 )

df<-df %>% mutate(wrr.ls.wpu = wrr.ls.wpu*pr_pop_2015 )
df<-df %>% mutate(wrr.ls.wd.2020 = wrr.ls.wd.2020*pr_pop_2020 )
df<-df %>% mutate(wrr.ls.wd.2025 = wrr.ls.wd.2025*pr_pop_2025 )
df<-df %>% mutate(wrr.ls.wd.2030 = wrr.ls.wd.2030*pr_pop_2030 )
df<-df %>% mutate(wrr.ls.wd.2035 = wrr.ls.wd.2035*pr_pop_2035 )
df<-df %>% mutate(wrr.ls.wd.2040 = wrr.ls.wd.2040*pr_pop_2040 )
df<-df %>% mutate(wrr.ls.wd.2045 = wrr.ls.wd.2045*pr_pop_2045 )
df<-df %>% mutate(wrr.ls.wd.2050 = wrr.ls.wd.2050*pr_pop_2050 )
df<-df %>% mutate(wrr.ls.wd.2055 = wrr.ls.wd.2055*pr_pop_2055 )
df<-df %>% mutate(wrr.ls.wd.2060 = wrr.ls.wd.2060*pr_pop_2060 )
df<-df %>% mutate(wrr.ls.wd.2065 = wrr.ls.wd.2065*pr_pop_2065 )
df<-df %>% mutate(wrr.ls.wd.2070 = wrr.ls.wd.2070*pr_pop_2070 )

proj <- merge(proj,water1,by='fips')
proj <- merge(proj,df, by='WRR')

# this calculates the percentage of AQ wd in 2015 in the HUC4 of the total 
# WRR AQ wd. This is multiplied by the total AQ wd in the WRR for that year.

proj$AQ.wd.tot.x <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wpu)
proj$aq.wd.2020 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2020)
proj$aq.wd.2025 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2025)
proj$aq.wd.2030 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2030)
proj$aq.wd.2035 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2035)
proj$aq.wd.2040 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2040)
proj$aq.wd.2045 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2045)
proj$aq.wd.2050 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2050)
proj$aq.wd.2055 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2055)
proj$aq.wd.2060 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2060)
proj$aq.wd.2065 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2065)
proj$aq.wd.2070 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2070)

proj$LS.wd.tot.x <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wpu)
proj$ls.wd.2020 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2020)
proj$ls.wd.2025 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2025)
proj$ls.wd.2030 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2030)
proj$ls.wd.2035 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2035)
proj$ls.wd.2040 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2040)
proj$ls.wd.2045 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2045)
proj$ls.wd.2050 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2050)
proj$ls.wd.2055 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2055)
proj$ls.wd.2060 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2060)
proj$ls.wd.2065 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2065)
proj$ls.wd.2070 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2070)

colnames(proj)[colnames(proj)=="AQ.wd.tot.x"] <- "aq.wd.2015"
colnames(proj)[colnames(proj)=="LS.wd.tot.x"] <- "ls.wd.2015"

proj <- proj %>% group_by(fips.x) %>% summarize_all(funs(sum))
proj <-proj[,c(1,5,96:106,6,107:117)]

proj.ssp1 <- proj

#---------------------------------------------------------------------------------------------------------------------------
#SSP2

df<-cntypercent
df$WRR <-floor(signif(df$HUC_10,2)/100000000)
df<-df[,c(1:3,8,6)]
df <- df %>% group_by(FIPS,WRR) %>% summarise(Shape_Area=sum(Shape_Area),CountyArea=mean(CountyArea))
df$pctwrr <- df$Shape_Area/df$CountyArea
df1 <- df[,c(1,5)]
df1 <- df1 %>% group_by(FIPS) %>% summarise_all(funs(sum))

df <- merge(df, df1, by="FIPS")
df$pctwrr.x <- df$pctwrr.x/df$pctwrr.y
df<- df[,c(1,2,5)]
colnames(df)[colnames(df)=="FIPS"] <- "fips"
proj <-df

#input water use data from "WaterUsedatacleanup.R"

water1 <- read.csv(file="inputs_ssp2.csv")
water1 <- water1[,c(2,5,13,14)]
water1$AQ.perUnit <- water1$AQ.perUnit*water1$pop
water1$LS.perUnit <- water1$LS.perUnit*water1$pop
colnames(water1)[colnames(water1)=="AQ.perUnit"] <- "AQ.wd.tot"
colnames(water1)[colnames(water1)=="LS.perUnit"] <- "LS.wd.tot"

#calculate WRR population and wpu for LS and AQ

pop <- read.csv("pop_ssp2.csv")
df <- merge(df, pop, by='fips')
df <- merge(df, water1, by='fips')

df <- df %>% mutate(pr_pop_2015=pr_pop_2015 * pctwrr.x)
df <- df %>% mutate(pr_pop_2020=pr_pop_2020 * pctwrr.x)
df <- df %>% mutate(pr_pop_2025=pr_pop_2025 * pctwrr.x)
df <- df %>% mutate(pr_pop_2030=pr_pop_2030 * pctwrr.x)
df <- df %>% mutate(pr_pop_2035=pr_pop_2035 * pctwrr.x)
df <- df %>% mutate(pr_pop_2040=pr_pop_2040 * pctwrr.x)
df <- df %>% mutate(pr_pop_2045=pr_pop_2045 * pctwrr.x)
df <- df %>% mutate(pr_pop_2050=pr_pop_2050 * pctwrr.x)
df <- df %>% mutate(pr_pop_2055=pr_pop_2055 * pctwrr.x)
df <- df %>% mutate(pr_pop_2060=pr_pop_2060 * pctwrr.x)
df <- df %>% mutate(pr_pop_2065=pr_pop_2065 * pctwrr.x)
df <- df %>% mutate(pr_pop_2070=pr_pop_2070 * pctwrr.x)
df <- df %>% mutate(AQ.wd.tot=AQ.wd.tot * pctwrr.x)
df <- df %>% mutate(LS.wd.tot=LS.wd.tot * pctwrr.x)
df <- df %>% mutate(pop =pop*pctwrr.x)
df <- df %>% group_by(WRR) %>% summarize_all(funs(sum))

#calculate WRR wpu for LS and AQ
df$wrr.aq.wpu <- df$AQ.wd.tot/df$pop
df$wrr.ls.wpu <- df$LS.wd.tot/df$pop

df$wrr.aq.growth <- ifelse(df$WRR<=9, 0.0336,0.0829)
df$wrr.aq.decay <- ifelse(df$WRR<=9, -0.05,-.1)
df$wrr.ls.growth <- ifelse(df$WRR<=9, -0.0113,-0.0221)
df$wrr.ls.decay <- ifelse(df$WRR<=9, -0.04,-0.04)

df$wrr.aq.wd.2020 <- NA
df$wrr.aq.wd.2025 <- NA
df$wrr.aq.wd.2030 <- NA
df$wrr.aq.wd.2035 <- NA
df$wrr.aq.wd.2040 <- NA
df$wrr.aq.wd.2045 <- NA
df$wrr.aq.wd.2050 <- NA
df$wrr.aq.wd.2055 <- NA
df$wrr.aq.wd.2060 <- NA
df$wrr.aq.wd.2065 <- NA
df$wrr.aq.wd.2070 <- NA

df$wrr.ls.wd.2020 <- NA
df$wrr.ls.wd.2025 <- NA
df$wrr.ls.wd.2030 <- NA
df$wrr.ls.wd.2035 <- NA
df$wrr.ls.wd.2040 <- NA
df$wrr.ls.wd.2045 <- NA
df$wrr.ls.wd.2050 <- NA
df$wrr.ls.wd.2055 <- NA
df$wrr.ls.wd.2060 <- NA
df$wrr.ls.wd.2065 <- NA
df$wrr.ls.wd.2070 <- NA

#calculate projected wpu
df<-df %>% mutate(wrr.aq.wd.2020 = wrr.aq.wpu*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2020-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2025 = wrr.aq.wd.2020*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2025-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2030 = wrr.aq.wd.2025*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2030-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2035 = wrr.aq.wd.2030*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2035-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2040 = wrr.aq.wd.2035*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2040-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2045 = wrr.aq.wd.2040*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2045-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2050 = wrr.aq.wd.2045*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2050-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2055 = wrr.aq.wd.2050*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2055-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2060 = wrr.aq.wd.2055*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2060-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2065 = wrr.aq.wd.2060*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2065-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2070 = wrr.aq.wd.2065*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2070-2015))^5 )

df<-df %>% mutate(wrr.ls.wd.2020 = wrr.ls.wpu*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2020-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2025 = wrr.ls.wd.2020*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2025-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2030 = wrr.ls.wd.2025*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2030-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2035 = wrr.ls.wd.2030*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2035-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2040 = wrr.ls.wd.2035*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2040-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2045 = wrr.ls.wd.2040*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2045-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2050 = wrr.ls.wd.2045*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2050-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2055 = wrr.ls.wd.2050*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2055-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2060 = wrr.ls.wd.2055*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2060-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2065 = wrr.ls.wd.2060*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2065-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2070 = wrr.ls.wd.2065*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2070-2015))^5 )

#calculate actual wd
df<-df %>% mutate(wrr.aq.wpu = wrr.aq.wpu*pr_pop_2015 )
df<-df %>% mutate(wrr.aq.wd.2020 = wrr.aq.wd.2020*pr_pop_2020 )
df<-df %>% mutate(wrr.aq.wd.2025 = wrr.aq.wd.2025*pr_pop_2025 )
df<-df %>% mutate(wrr.aq.wd.2030 = wrr.aq.wd.2030*pr_pop_2030 )
df<-df %>% mutate(wrr.aq.wd.2035 = wrr.aq.wd.2035*pr_pop_2035 )
df<-df %>% mutate(wrr.aq.wd.2040 = wrr.aq.wd.2040*pr_pop_2040 )
df<-df %>% mutate(wrr.aq.wd.2045 = wrr.aq.wd.2045*pr_pop_2045 )
df<-df %>% mutate(wrr.aq.wd.2050 = wrr.aq.wd.2050*pr_pop_2050 )
df<-df %>% mutate(wrr.aq.wd.2055 = wrr.aq.wd.2055*pr_pop_2055 )
df<-df %>% mutate(wrr.aq.wd.2060 = wrr.aq.wd.2060*pr_pop_2060 )
df<-df %>% mutate(wrr.aq.wd.2065 = wrr.aq.wd.2065*pr_pop_2065 )
df<-df %>% mutate(wrr.aq.wd.2070 = wrr.aq.wd.2070*pr_pop_2070 )

df<-df %>% mutate(wrr.ls.wpu = wrr.ls.wpu*pr_pop_2015 )
df<-df %>% mutate(wrr.ls.wd.2020 = wrr.ls.wd.2020*pr_pop_2020 )
df<-df %>% mutate(wrr.ls.wd.2025 = wrr.ls.wd.2025*pr_pop_2025 )
df<-df %>% mutate(wrr.ls.wd.2030 = wrr.ls.wd.2030*pr_pop_2030 )
df<-df %>% mutate(wrr.ls.wd.2035 = wrr.ls.wd.2035*pr_pop_2035 )
df<-df %>% mutate(wrr.ls.wd.2040 = wrr.ls.wd.2040*pr_pop_2040 )
df<-df %>% mutate(wrr.ls.wd.2045 = wrr.ls.wd.2045*pr_pop_2045 )
df<-df %>% mutate(wrr.ls.wd.2050 = wrr.ls.wd.2050*pr_pop_2050 )
df<-df %>% mutate(wrr.ls.wd.2055 = wrr.ls.wd.2055*pr_pop_2055 )
df<-df %>% mutate(wrr.ls.wd.2060 = wrr.ls.wd.2060*pr_pop_2060 )
df<-df %>% mutate(wrr.ls.wd.2065 = wrr.ls.wd.2065*pr_pop_2065 )
df<-df %>% mutate(wrr.ls.wd.2070 = wrr.ls.wd.2070*pr_pop_2070 )

proj <- merge(proj,water1,by='fips')
proj <- merge(proj,df, by='WRR')

proj$AQ.wd.tot.x <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wpu)
proj$aq.wd.2020 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2020)
proj$aq.wd.2025 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2025)
proj$aq.wd.2030 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2030)
proj$aq.wd.2035 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2035)
proj$aq.wd.2040 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2040)
proj$aq.wd.2045 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2045)
proj$aq.wd.2050 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2050)
proj$aq.wd.2055 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2055)
proj$aq.wd.2060 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2060)
proj$aq.wd.2065 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2065)
proj$aq.wd.2070 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2070)

proj$LS.wd.tot.x <-((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wpu)
proj$ls.wd.2020 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2020)
proj$ls.wd.2025 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2025)
proj$ls.wd.2030 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2030)
proj$ls.wd.2035 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2035)
proj$ls.wd.2040 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2040)
proj$ls.wd.2045 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2045)
proj$ls.wd.2050 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2050)
proj$ls.wd.2055 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2055)
proj$ls.wd.2060 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2060)
proj$ls.wd.2065 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2065)
proj$ls.wd.2070 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2070)

colnames(proj)[colnames(proj)=="AQ.wd.tot.x"] <- "aq.wd.2015"
colnames(proj)[colnames(proj)=="LS.wd.tot.x"] <- "ls.wd.2015"

proj <- proj %>% group_by(fips.x) %>% summarize_all(funs(sum))
proj <-proj[,c(1,5,96:106,6,107:117)]

proj.ssp2 <- proj

#---------------------------------------------------------------------------------------------------------------------------
#SSP3

df<-cntypercent
df$WRR <-floor(signif(df$HUC_10,2)/100000000)
df<-df[,c(1:3,8,6)]
df <- df %>% group_by(FIPS,WRR) %>% summarise(Shape_Area=sum(Shape_Area),CountyArea=mean(CountyArea))
df$pctwrr <- df$Shape_Area/df$CountyArea
df1 <- df[,c(1,5)]
df1 <- df1 %>% group_by(FIPS) %>% summarise_all(funs(sum))

df <- merge(df, df1, by="FIPS")
df$pctwrr.x <- df$pctwrr.x/df$pctwrr.y
df<- df[,c(1,2,5)]
colnames(df)[colnames(df)=="FIPS"] <- "fips"
proj <-df

#input water use data from "WaterUsedatacleanup.R"

water1 <- read.csv(file="inputs_ssp3.csv")
water1 <- water1[,c(2,5,13,14)]
water1$AQ.perUnit <- water1$AQ.perUnit*water1$pop
water1$LS.perUnit <- water1$LS.perUnit*water1$pop
colnames(water1)[colnames(water1)=="AQ.perUnit"] <- "AQ.wd.tot"
colnames(water1)[colnames(water1)=="LS.perUnit"] <- "LS.wd.tot"

#calculate WRR population and wpu for LS and AQ

pop <- read.csv("pop_ssp3.csv")
df <- merge(df, pop, by='fips')
df <- merge(df, water1, by='fips')

df <- df %>% mutate(pr_pop_2015=pr_pop_2015 * pctwrr.x)
df <- df %>% mutate(pr_pop_2020=pr_pop_2020 * pctwrr.x)
df <- df %>% mutate(pr_pop_2025=pr_pop_2025 * pctwrr.x)
df <- df %>% mutate(pr_pop_2030=pr_pop_2030 * pctwrr.x)
df <- df %>% mutate(pr_pop_2035=pr_pop_2035 * pctwrr.x)
df <- df %>% mutate(pr_pop_2040=pr_pop_2040 * pctwrr.x)
df <- df %>% mutate(pr_pop_2045=pr_pop_2045 * pctwrr.x)
df <- df %>% mutate(pr_pop_2050=pr_pop_2050 * pctwrr.x)
df <- df %>% mutate(pr_pop_2055=pr_pop_2055 * pctwrr.x)
df <- df %>% mutate(pr_pop_2060=pr_pop_2060 * pctwrr.x)
df <- df %>% mutate(pr_pop_2065=pr_pop_2065 * pctwrr.x)
df <- df %>% mutate(pr_pop_2070=pr_pop_2070 * pctwrr.x)
df <- df %>% mutate(AQ.wd.tot=AQ.wd.tot * pctwrr.x)
df <- df %>% mutate(LS.wd.tot=LS.wd.tot * pctwrr.x)
df <- df %>% mutate(pop =pop*pctwrr.x)
df <- df %>% group_by(WRR) %>% summarize_all(funs(sum))

#calculate WRR wpu for LS and AQ
df$wrr.aq.wpu <- df$AQ.wd.tot/df$pop
df$wrr.ls.wpu <- df$LS.wd.tot/df$pop

df$wrr.aq.growth <- ifelse(df$WRR<=9, 0.0336,0.0829)
df$wrr.aq.decay <- ifelse(df$WRR<=9, -0.05,-.1)
df$wrr.ls.growth <- ifelse(df$WRR<=9, -0.0113,-0.0221)
df$wrr.ls.decay <- ifelse(df$WRR<=9, -0.04,-0.04)

df$wrr.aq.wd.2020 <- NA
df$wrr.aq.wd.2025 <- NA
df$wrr.aq.wd.2030 <- NA
df$wrr.aq.wd.2035 <- NA
df$wrr.aq.wd.2040 <- NA
df$wrr.aq.wd.2045 <- NA
df$wrr.aq.wd.2050 <- NA
df$wrr.aq.wd.2055 <- NA
df$wrr.aq.wd.2060 <- NA
df$wrr.aq.wd.2065 <- NA
df$wrr.aq.wd.2070 <- NA

df$wrr.ls.wd.2020 <- NA
df$wrr.ls.wd.2025 <- NA
df$wrr.ls.wd.2030 <- NA
df$wrr.ls.wd.2035 <- NA
df$wrr.ls.wd.2040 <- NA
df$wrr.ls.wd.2045 <- NA
df$wrr.ls.wd.2050 <- NA
df$wrr.ls.wd.2055 <- NA
df$wrr.ls.wd.2060 <- NA
df$wrr.ls.wd.2065 <- NA
df$wrr.ls.wd.2070 <- NA

#calculate projected wpu
df<-df %>% mutate(wrr.aq.wd.2020 = wrr.aq.wpu*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2020-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2025 = wrr.aq.wd.2020*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2025-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2030 = wrr.aq.wd.2025*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2030-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2035 = wrr.aq.wd.2030*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2035-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2040 = wrr.aq.wd.2035*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2040-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2045 = wrr.aq.wd.2040*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2045-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2050 = wrr.aq.wd.2045*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2050-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2055 = wrr.aq.wd.2050*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2055-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2060 = wrr.aq.wd.2055*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2060-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2065 = wrr.aq.wd.2060*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2065-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2070 = wrr.aq.wd.2065*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2070-2015))^5 )

df<-df %>% mutate(wrr.ls.wd.2020 = wrr.ls.wpu*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2020-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2025 = wrr.ls.wd.2020*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2025-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2030 = wrr.ls.wd.2025*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2030-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2035 = wrr.ls.wd.2030*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2035-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2040 = wrr.ls.wd.2035*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2040-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2045 = wrr.ls.wd.2040*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2045-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2050 = wrr.ls.wd.2045*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2050-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2055 = wrr.ls.wd.2050*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2055-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2060 = wrr.ls.wd.2055*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2060-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2065 = wrr.ls.wd.2060*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2065-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2070 = wrr.ls.wd.2065*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2070-2015))^5 )

#calculate actual wd
df<-df %>% mutate(wrr.aq.wpu = wrr.aq.wpu*pr_pop_2015 )
df<-df %>% mutate(wrr.aq.wd.2020 = wrr.aq.wd.2020*pr_pop_2020 )
df<-df %>% mutate(wrr.aq.wd.2025 = wrr.aq.wd.2025*pr_pop_2025 )
df<-df %>% mutate(wrr.aq.wd.2030 = wrr.aq.wd.2030*pr_pop_2030 )
df<-df %>% mutate(wrr.aq.wd.2035 = wrr.aq.wd.2035*pr_pop_2035 )
df<-df %>% mutate(wrr.aq.wd.2040 = wrr.aq.wd.2040*pr_pop_2040 )
df<-df %>% mutate(wrr.aq.wd.2045 = wrr.aq.wd.2045*pr_pop_2045 )
df<-df %>% mutate(wrr.aq.wd.2050 = wrr.aq.wd.2050*pr_pop_2050 )
df<-df %>% mutate(wrr.aq.wd.2055 = wrr.aq.wd.2055*pr_pop_2055 )
df<-df %>% mutate(wrr.aq.wd.2060 = wrr.aq.wd.2060*pr_pop_2060 )
df<-df %>% mutate(wrr.aq.wd.2065 = wrr.aq.wd.2065*pr_pop_2065 )
df<-df %>% mutate(wrr.aq.wd.2070 = wrr.aq.wd.2070*pr_pop_2070 )

df<-df %>% mutate(wrr.ls.wpu = wrr.ls.wpu*pr_pop_2015 )
df<-df %>% mutate(wrr.ls.wd.2020 = wrr.ls.wd.2020*pr_pop_2020 )
df<-df %>% mutate(wrr.ls.wd.2025 = wrr.ls.wd.2025*pr_pop_2025 )
df<-df %>% mutate(wrr.ls.wd.2030 = wrr.ls.wd.2030*pr_pop_2030 )
df<-df %>% mutate(wrr.ls.wd.2035 = wrr.ls.wd.2035*pr_pop_2035 )
df<-df %>% mutate(wrr.ls.wd.2040 = wrr.ls.wd.2040*pr_pop_2040 )
df<-df %>% mutate(wrr.ls.wd.2045 = wrr.ls.wd.2045*pr_pop_2045 )
df<-df %>% mutate(wrr.ls.wd.2050 = wrr.ls.wd.2050*pr_pop_2050 )
df<-df %>% mutate(wrr.ls.wd.2055 = wrr.ls.wd.2055*pr_pop_2055 )
df<-df %>% mutate(wrr.ls.wd.2060 = wrr.ls.wd.2060*pr_pop_2060 )
df<-df %>% mutate(wrr.ls.wd.2065 = wrr.ls.wd.2065*pr_pop_2065 )
df<-df %>% mutate(wrr.ls.wd.2070 = wrr.ls.wd.2070*pr_pop_2070 )

proj <- merge(proj,water1,by='fips')
proj <- merge(proj,df, by='WRR')

proj$AQ.wd.tot.x <-((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wpu)
proj$aq.wd.2020 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2020)
proj$aq.wd.2025 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2025)
proj$aq.wd.2030 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2030)
proj$aq.wd.2035 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2035)
proj$aq.wd.2040 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2040)
proj$aq.wd.2045 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2045)
proj$aq.wd.2050 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2050)
proj$aq.wd.2055 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2055)
proj$aq.wd.2060 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2060)
proj$aq.wd.2065 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2065)
proj$aq.wd.2070 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2070)

proj$LS.wd.tot.x <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wpu)
proj$ls.wd.2020 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2020)
proj$ls.wd.2025 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2025)
proj$ls.wd.2030 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2030)
proj$ls.wd.2035 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2035)
proj$ls.wd.2040 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2040)
proj$ls.wd.2045 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2045)
proj$ls.wd.2050 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2050)
proj$ls.wd.2055 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2055)
proj$ls.wd.2060 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2060)
proj$ls.wd.2065 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2065)
proj$ls.wd.2070 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2070)

colnames(proj)[colnames(proj)=="AQ.wd.tot.x"] <- "aq.wd.2015"
colnames(proj)[colnames(proj)=="LS.wd.tot.x"] <- "ls.wd.2015"

proj <- proj %>% group_by(fips.x) %>% summarize_all(funs(sum))
proj <-proj[,c(1,5,96:106,6,107:117)]

proj.ssp3 <- proj

#---------------------------------------------------------------------------------------------------------------------------
#SSP4

df<-cntypercent
df$WRR <-floor(signif(df$HUC_10,2)/100000000)
df<-df[,c(1:3,8,6)]
df <- df %>% group_by(FIPS,WRR) %>% summarise(Shape_Area=sum(Shape_Area),CountyArea=mean(CountyArea))
df$pctwrr <- df$Shape_Area/df$CountyArea
df1 <- df[,c(1,5)]
df1 <- df1 %>% group_by(FIPS) %>% summarise_all(funs(sum))

df <- merge(df, df1, by="FIPS")
df$pctwrr.x <- df$pctwrr.x/df$pctwrr.y
df<- df[,c(1,2,5)]
colnames(df)[colnames(df)=="FIPS"] <- "fips"
proj <-df

#input water use data from "WaterUsedatacleanup.R"

water1 <- read.csv(file="inputs_ssp4.csv")
water1 <- water1[,c(2,5,13,14)]
water1$AQ.perUnit <- water1$AQ.perUnit*water1$pop
water1$LS.perUnit <- water1$LS.perUnit*water1$pop
colnames(water1)[colnames(water1)=="AQ.perUnit"] <- "AQ.wd.tot"
colnames(water1)[colnames(water1)=="LS.perUnit"] <- "LS.wd.tot"

#calculate WRR population and wpu for LS and AQ

pop <- read.csv("pop_ssp4.csv")
df <- merge(df, pop, by='fips')
df <- merge(df, water1, by='fips')

df <- df %>% mutate(pr_pop_2015=pr_pop_2015 * pctwrr.x)
df <- df %>% mutate(pr_pop_2020=pr_pop_2020 * pctwrr.x)
df <- df %>% mutate(pr_pop_2025=pr_pop_2025 * pctwrr.x)
df <- df %>% mutate(pr_pop_2030=pr_pop_2030 * pctwrr.x)
df <- df %>% mutate(pr_pop_2035=pr_pop_2035 * pctwrr.x)
df <- df %>% mutate(pr_pop_2040=pr_pop_2040 * pctwrr.x)
df <- df %>% mutate(pr_pop_2045=pr_pop_2045 * pctwrr.x)
df <- df %>% mutate(pr_pop_2050=pr_pop_2050 * pctwrr.x)
df <- df %>% mutate(pr_pop_2055=pr_pop_2055 * pctwrr.x)
df <- df %>% mutate(pr_pop_2060=pr_pop_2060 * pctwrr.x)
df <- df %>% mutate(pr_pop_2065=pr_pop_2065 * pctwrr.x)
df <- df %>% mutate(pr_pop_2070=pr_pop_2070 * pctwrr.x)
df <- df %>% mutate(AQ.wd.tot=AQ.wd.tot * pctwrr.x)
df <- df %>% mutate(LS.wd.tot=LS.wd.tot * pctwrr.x)
df <- df %>% mutate(pop =pop*pctwrr.x)
df <- df %>% group_by(WRR) %>% summarize_all(funs(sum))

#calculate WRR wpu for LS and AQ
df$wrr.aq.wpu <- df$AQ.wd.tot/df$pop
df$wrr.ls.wpu <- df$LS.wd.tot/df$pop

df$wrr.aq.growth <- ifelse(df$WRR<=9, 0.0336,0.0829)
df$wrr.aq.decay <- ifelse(df$WRR<=9, -0.05,-.1)
df$wrr.ls.growth <- ifelse(df$WRR<=9, -0.0113,-0.0221)
df$wrr.ls.decay <- ifelse(df$WRR<=9, -0.04,-0.04)

df$wrr.aq.wd.2020 <- NA
df$wrr.aq.wd.2025 <- NA
df$wrr.aq.wd.2030 <- NA
df$wrr.aq.wd.2035 <- NA
df$wrr.aq.wd.2040 <- NA
df$wrr.aq.wd.2045 <- NA
df$wrr.aq.wd.2050 <- NA
df$wrr.aq.wd.2055 <- NA
df$wrr.aq.wd.2060 <- NA
df$wrr.aq.wd.2065 <- NA
df$wrr.aq.wd.2070 <- NA

df$wrr.ls.wd.2020 <- NA
df$wrr.ls.wd.2025 <- NA
df$wrr.ls.wd.2030 <- NA
df$wrr.ls.wd.2035 <- NA
df$wrr.ls.wd.2040 <- NA
df$wrr.ls.wd.2045 <- NA
df$wrr.ls.wd.2050 <- NA
df$wrr.ls.wd.2055 <- NA
df$wrr.ls.wd.2060 <- NA
df$wrr.ls.wd.2065 <- NA
df$wrr.ls.wd.2070 <- NA

#calculate projected wpu
df<-df %>% mutate(wrr.aq.wd.2020 = wrr.aq.wpu*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2020-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2025 = wrr.aq.wd.2020*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2025-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2030 = wrr.aq.wd.2025*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2030-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2035 = wrr.aq.wd.2030*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2035-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2040 = wrr.aq.wd.2035*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2040-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2045 = wrr.aq.wd.2040*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2045-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2050 = wrr.aq.wd.2045*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2050-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2055 = wrr.aq.wd.2050*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2055-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2060 = wrr.aq.wd.2055*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2060-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2065 = wrr.aq.wd.2060*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2065-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2070 = wrr.aq.wd.2065*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2070-2015))^5 )

df<-df %>% mutate(wrr.ls.wd.2020 = wrr.ls.wpu*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2020-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2025 = wrr.ls.wd.2020*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2025-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2030 = wrr.ls.wd.2025*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2030-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2035 = wrr.ls.wd.2030*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2035-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2040 = wrr.ls.wd.2035*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2040-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2045 = wrr.ls.wd.2040*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2045-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2050 = wrr.ls.wd.2045*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2050-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2055 = wrr.ls.wd.2050*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2055-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2060 = wrr.ls.wd.2055*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2060-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2065 = wrr.ls.wd.2060*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2065-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2070 = wrr.ls.wd.2065*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2070-2015))^5 )

#calculate actual wd
df<-df %>% mutate(wrr.aq.wpu = wrr.aq.wpu*pr_pop_2015 )
df<-df %>% mutate(wrr.aq.wd.2020 = wrr.aq.wd.2020*pr_pop_2020 )
df<-df %>% mutate(wrr.aq.wd.2025 = wrr.aq.wd.2025*pr_pop_2025 )
df<-df %>% mutate(wrr.aq.wd.2030 = wrr.aq.wd.2030*pr_pop_2030 )
df<-df %>% mutate(wrr.aq.wd.2035 = wrr.aq.wd.2035*pr_pop_2035 )
df<-df %>% mutate(wrr.aq.wd.2040 = wrr.aq.wd.2040*pr_pop_2040 )
df<-df %>% mutate(wrr.aq.wd.2045 = wrr.aq.wd.2045*pr_pop_2045 )
df<-df %>% mutate(wrr.aq.wd.2050 = wrr.aq.wd.2050*pr_pop_2050 )
df<-df %>% mutate(wrr.aq.wd.2055 = wrr.aq.wd.2055*pr_pop_2055 )
df<-df %>% mutate(wrr.aq.wd.2060 = wrr.aq.wd.2060*pr_pop_2060 )
df<-df %>% mutate(wrr.aq.wd.2065 = wrr.aq.wd.2065*pr_pop_2065 )
df<-df %>% mutate(wrr.aq.wd.2070 = wrr.aq.wd.2070*pr_pop_2070 )

df<-df %>% mutate(wrr.ls.wpu = wrr.ls.wpu*pr_pop_2015 )
df<-df %>% mutate(wrr.ls.wd.2020 = wrr.ls.wd.2020*pr_pop_2020 )
df<-df %>% mutate(wrr.ls.wd.2025 = wrr.ls.wd.2025*pr_pop_2025 )
df<-df %>% mutate(wrr.ls.wd.2030 = wrr.ls.wd.2030*pr_pop_2030 )
df<-df %>% mutate(wrr.ls.wd.2035 = wrr.ls.wd.2035*pr_pop_2035 )
df<-df %>% mutate(wrr.ls.wd.2040 = wrr.ls.wd.2040*pr_pop_2040 )
df<-df %>% mutate(wrr.ls.wd.2045 = wrr.ls.wd.2045*pr_pop_2045 )
df<-df %>% mutate(wrr.ls.wd.2050 = wrr.ls.wd.2050*pr_pop_2050 )
df<-df %>% mutate(wrr.ls.wd.2055 = wrr.ls.wd.2055*pr_pop_2055 )
df<-df %>% mutate(wrr.ls.wd.2060 = wrr.ls.wd.2060*pr_pop_2060 )
df<-df %>% mutate(wrr.ls.wd.2065 = wrr.ls.wd.2065*pr_pop_2065 )
df<-df %>% mutate(wrr.ls.wd.2070 = wrr.ls.wd.2070*pr_pop_2070 )

proj <- merge(proj,water1,by='fips')
proj <- merge(proj,df, by='WRR')

proj$AQ.wd.tot.x <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wpu)
proj$aq.wd.2020 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2020)
proj$aq.wd.2025 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2025)
proj$aq.wd.2030 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2030)
proj$aq.wd.2035 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2035)
proj$aq.wd.2040 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2040)
proj$aq.wd.2045 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2045)
proj$aq.wd.2050 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2050)
proj$aq.wd.2055 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2055)
proj$aq.wd.2060 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2060)
proj$aq.wd.2065 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2065)
proj$aq.wd.2070 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2070)

proj$LS.wd.tot.x <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wpu)
proj$ls.wd.2020 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2020)
proj$ls.wd.2025 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2025)
proj$ls.wd.2030 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2030)
proj$ls.wd.2035 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2035)
proj$ls.wd.2040 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2040)
proj$ls.wd.2045 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2045)
proj$ls.wd.2050 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2050)
proj$ls.wd.2055 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2055)
proj$ls.wd.2060 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2060)
proj$ls.wd.2065 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2065)
proj$ls.wd.2070 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2070)

colnames(proj)[colnames(proj)=="AQ.wd.tot.x"] <- "aq.wd.2015"
colnames(proj)[colnames(proj)=="LS.wd.tot.x"] <- "ls.wd.2015"

proj <- proj %>% group_by(fips.x) %>% summarize_all(funs(sum))
proj <-proj[,c(1,5,96:106,6,107:117)]

proj.ssp4 <- proj

#---------------------------------------------------------------------------------------------------------------------------
#SSP5

df<-cntypercent
df$WRR <-floor(signif(df$HUC_10,2)/100000000)
df<-df[,c(1:3,8,6)]
df <- df %>% group_by(FIPS,WRR) %>% summarise(Shape_Area=sum(Shape_Area),CountyArea=mean(CountyArea))
df$pctwrr <- df$Shape_Area/df$CountyArea
df1 <- df[,c(1,5)]
df1 <- df1 %>% group_by(FIPS) %>% summarise_all(funs(sum))

df <- merge(df, df1, by="FIPS")
df$pctwrr.x <- df$pctwrr.x/df$pctwrr.y
df<- df[,c(1,2,5)]
colnames(df)[colnames(df)=="FIPS"] <- "fips"
proj <-df

#input water use data from "WaterUsedatacleanup.R"

water1 <- read.csv(file="inputs_ssp5.csv")
water1 <- water1[,c(2,5,13,14)]
water1$AQ.perUnit <- water1$AQ.perUnit*water1$pop
water1$LS.perUnit <- water1$LS.perUnit*water1$pop
colnames(water1)[colnames(water1)=="AQ.perUnit"] <- "AQ.wd.tot"
colnames(water1)[colnames(water1)=="LS.perUnit"] <- "LS.wd.tot"

#calculate WRR population and wpu for LS and AQ

pop <- read.csv("pop_ssp5.csv")
df <- merge(df, pop, by='fips')
df <- merge(df, water1, by='fips')

df <- df %>% mutate(pr_pop_2015=pr_pop_2015 * pctwrr.x)
df <- df %>% mutate(pr_pop_2020=pr_pop_2020 * pctwrr.x)
df <- df %>% mutate(pr_pop_2025=pr_pop_2025 * pctwrr.x)
df <- df %>% mutate(pr_pop_2030=pr_pop_2030 * pctwrr.x)
df <- df %>% mutate(pr_pop_2035=pr_pop_2035 * pctwrr.x)
df <- df %>% mutate(pr_pop_2040=pr_pop_2040 * pctwrr.x)
df <- df %>% mutate(pr_pop_2045=pr_pop_2045 * pctwrr.x)
df <- df %>% mutate(pr_pop_2050=pr_pop_2050 * pctwrr.x)
df <- df %>% mutate(pr_pop_2055=pr_pop_2055 * pctwrr.x)
df <- df %>% mutate(pr_pop_2060=pr_pop_2060 * pctwrr.x)
df <- df %>% mutate(pr_pop_2065=pr_pop_2065 * pctwrr.x)
df <- df %>% mutate(pr_pop_2070=pr_pop_2070 * pctwrr.x)
df <- df %>% mutate(AQ.wd.tot=AQ.wd.tot * pctwrr.x)
df <- df %>% mutate(LS.wd.tot=LS.wd.tot * pctwrr.x)
df <- df %>% mutate(pop =pop*pctwrr.x)
df <- df %>% group_by(WRR) %>% summarize_all(funs(sum))

#calculate WRR wpu for LS and AQ
df$wrr.aq.wpu <- df$AQ.wd.tot/df$pop
df$wrr.ls.wpu <- df$LS.wd.tot/df$pop

df$wrr.aq.growth <- ifelse(df$WRR<=9, 0.0336,0.0829)
df$wrr.aq.decay <- ifelse(df$WRR<=9, -0.05,-.1)
df$wrr.ls.growth <- ifelse(df$WRR<=9, -0.0113,-0.0221)
df$wrr.ls.decay <- ifelse(df$WRR<=9, -0.04,-0.04)

df$wrr.aq.wd.2020 <- NA
df$wrr.aq.wd.2025 <- NA
df$wrr.aq.wd.2030 <- NA
df$wrr.aq.wd.2035 <- NA
df$wrr.aq.wd.2040 <- NA
df$wrr.aq.wd.2045 <- NA
df$wrr.aq.wd.2050 <- NA
df$wrr.aq.wd.2055 <- NA
df$wrr.aq.wd.2060 <- NA
df$wrr.aq.wd.2065 <- NA
df$wrr.aq.wd.2070 <- NA

df$wrr.ls.wd.2020 <- NA
df$wrr.ls.wd.2025 <- NA
df$wrr.ls.wd.2030 <- NA
df$wrr.ls.wd.2035 <- NA
df$wrr.ls.wd.2040 <- NA
df$wrr.ls.wd.2045 <- NA
df$wrr.ls.wd.2050 <- NA
df$wrr.ls.wd.2055 <- NA
df$wrr.ls.wd.2060 <- NA
df$wrr.ls.wd.2065 <- NA
df$wrr.ls.wd.2070 <- NA

#calculate projected wpu
df<-df %>% mutate(wrr.aq.wd.2020 = wrr.aq.wpu*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2020-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2025 = wrr.aq.wd.2020*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2025-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2030 = wrr.aq.wd.2025*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2030-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2035 = wrr.aq.wd.2030*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2035-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2040 = wrr.aq.wd.2035*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2040-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2045 = wrr.aq.wd.2040*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2045-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2050 = wrr.aq.wd.2045*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2050-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2055 = wrr.aq.wd.2050*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2055-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2060 = wrr.aq.wd.2055*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2060-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2065 = wrr.aq.wd.2060*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2065-2015))^5 )
df<-df %>% mutate(wrr.aq.wd.2070 = wrr.aq.wd.2065*(1+wrr.aq.growth*(1+wrr.aq.decay)^(2070-2015))^5 )

df<-df %>% mutate(wrr.ls.wd.2020 = wrr.ls.wpu*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2020-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2025 = wrr.ls.wd.2020*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2025-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2030 = wrr.ls.wd.2025*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2030-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2035 = wrr.ls.wd.2030*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2035-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2040 = wrr.ls.wd.2035*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2040-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2045 = wrr.ls.wd.2040*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2045-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2050 = wrr.ls.wd.2045*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2050-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2055 = wrr.ls.wd.2050*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2055-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2060 = wrr.ls.wd.2055*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2060-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2065 = wrr.ls.wd.2060*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2065-2015))^5 )
df<-df %>% mutate(wrr.ls.wd.2070 = wrr.ls.wd.2065*(1+wrr.ls.growth*(1+wrr.ls.decay)^(2070-2015))^5 )

#calculate actual wd
df<-df %>% mutate(wrr.aq.wpu = wrr.aq.wpu*pr_pop_2015 )
df<-df %>% mutate(wrr.aq.wd.2020 = wrr.aq.wd.2020*pr_pop_2020 )
df<-df %>% mutate(wrr.aq.wd.2025 = wrr.aq.wd.2025*pr_pop_2025 )
df<-df %>% mutate(wrr.aq.wd.2030 = wrr.aq.wd.2030*pr_pop_2030 )
df<-df %>% mutate(wrr.aq.wd.2035 = wrr.aq.wd.2035*pr_pop_2035 )
df<-df %>% mutate(wrr.aq.wd.2040 = wrr.aq.wd.2040*pr_pop_2040 )
df<-df %>% mutate(wrr.aq.wd.2045 = wrr.aq.wd.2045*pr_pop_2045 )
df<-df %>% mutate(wrr.aq.wd.2050 = wrr.aq.wd.2050*pr_pop_2050 )
df<-df %>% mutate(wrr.aq.wd.2055 = wrr.aq.wd.2055*pr_pop_2055 )
df<-df %>% mutate(wrr.aq.wd.2060 = wrr.aq.wd.2060*pr_pop_2060 )
df<-df %>% mutate(wrr.aq.wd.2065 = wrr.aq.wd.2065*pr_pop_2065 )
df<-df %>% mutate(wrr.aq.wd.2070 = wrr.aq.wd.2070*pr_pop_2070 )

df<-df %>% mutate(wrr.ls.wpu = wrr.ls.wpu*pr_pop_2015 )
df<-df %>% mutate(wrr.ls.wd.2020 = wrr.ls.wd.2020*pr_pop_2020 )
df<-df %>% mutate(wrr.ls.wd.2025 = wrr.ls.wd.2025*pr_pop_2025 )
df<-df %>% mutate(wrr.ls.wd.2030 = wrr.ls.wd.2030*pr_pop_2030 )
df<-df %>% mutate(wrr.ls.wd.2035 = wrr.ls.wd.2035*pr_pop_2035 )
df<-df %>% mutate(wrr.ls.wd.2040 = wrr.ls.wd.2040*pr_pop_2040 )
df<-df %>% mutate(wrr.ls.wd.2045 = wrr.ls.wd.2045*pr_pop_2045 )
df<-df %>% mutate(wrr.ls.wd.2050 = wrr.ls.wd.2050*pr_pop_2050 )
df<-df %>% mutate(wrr.ls.wd.2055 = wrr.ls.wd.2055*pr_pop_2055 )
df<-df %>% mutate(wrr.ls.wd.2060 = wrr.ls.wd.2060*pr_pop_2060 )
df<-df %>% mutate(wrr.ls.wd.2065 = wrr.ls.wd.2065*pr_pop_2065 )
df<-df %>% mutate(wrr.ls.wd.2070 = wrr.ls.wd.2070*pr_pop_2070 )

proj <- merge(proj,water1,by='fips')
proj <- merge(proj,df, by='WRR')

proj$AQ.wd.tot.x <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wpu)
proj$aq.wd.2020 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2020)
proj$aq.wd.2025 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2025)
proj$aq.wd.2030 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2030)
proj$aq.wd.2035 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2035)
proj$aq.wd.2040 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2040)
proj$aq.wd.2045 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2045)
proj$aq.wd.2050 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2050)
proj$aq.wd.2055 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2055)
proj$aq.wd.2060 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2060)
proj$aq.wd.2065 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2065)
proj$aq.wd.2070 <- ((proj$AQ.wd.tot.x*proj$pctwrr.x.x)/proj$AQ.wd.tot.y)*(proj$wrr.aq.wd.2070)

proj$LS.wd.tot.x <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wpu)
proj$ls.wd.2020 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2020)
proj$ls.wd.2025 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2025)
proj$ls.wd.2030 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2030)
proj$ls.wd.2035 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2035)
proj$ls.wd.2040 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2040)
proj$ls.wd.2045 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2045)
proj$ls.wd.2050 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2050)
proj$ls.wd.2055 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2055)
proj$ls.wd.2060 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2060)
proj$ls.wd.2065 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2065)
proj$ls.wd.2070 <- ((proj$LS.wd.tot.x*proj$pctwrr.x.x)/proj$LS.wd.tot.y)*(proj$wrr.ls.wd.2070)

colnames(proj)[colnames(proj)=="AQ.wd.tot.x"] <- "aq.wd.2015"
colnames(proj)[colnames(proj)=="LS.wd.tot.x"] <- "ls.wd.2015"

proj <- proj %>% group_by(fips.x) %>% summarize_all(funs(sum))
proj <-proj[,c(1,5,96:106,6,107:117)]

proj.ssp5 <- proj


#-------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------
# Creating new dataframes for each scenario (ssp1, etc) and creating linear 
# approximations for in between years.
#----------------------------------------------------------------------------------------------------------------------

aq.ssp1 <- proj.ssp1[,c(1:13)]
aq.ssp1$sector <- "aq"
aq.ssp1 <- aq.ssp1[,c(1,14,2:13)]
names(aq.ssp1) <- c("fips","sector", "Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
aq.ssp1 <- linapprox(aq.ssp1)

aq.ssp2 <- proj.ssp2[,c(1:13)]
aq.ssp2$sector <- "aq"
aq.ssp2 <- aq.ssp2[,c(1,14,2:13)]
names(aq.ssp2) <- c("fips","sector", "Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
aq.ssp2 <- linapprox(aq.ssp2)

aq.ssp3 <- proj.ssp3[,c(1:13)]
aq.ssp3$sector <- "aq"
aq.ssp3 <- aq.ssp3[,c(1,14,2:13)]
names(aq.ssp3) <- c("fips","sector", "Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
aq.ssp3 <- linapprox(aq.ssp3)

aq.ssp4 <- proj.ssp4[,c(1:13)]
aq.ssp4$sector <- "aq"
aq.ssp4 <- aq.ssp4[,c(1,14,2:13)]
names(aq.ssp4) <- c("fips","sector", "Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
aq.ssp4 <- linapprox(aq.ssp4)

aq.ssp5 <- proj.ssp5[,c(1:13)]
aq.ssp5$sector <- "aq"
aq.ssp5 <- aq.ssp5[,c(1,14,2:13)]
names(aq.ssp5) <- c("fips","sector", "Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
aq.ssp5 <- linapprox(aq.ssp5)

ls.ssp1 <- proj.ssp1[,c(1,14:25)]
ls.ssp1$sector <- "ls"
ls.ssp1 <- ls.ssp1[,c(1,14,2:13)]
names(ls.ssp1) <- c("fips","sector","Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
ls.ssp1 <- linapprox(ls.ssp1)

ls.ssp2 <- proj.ssp2[,c(1,14:25)]
ls.ssp2$sector <- "ls"
ls.ssp2 <- ls.ssp2[,c(1,14,2:13)]
names(ls.ssp2) <- c("fips","sector","Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
ls.ssp2 <- linapprox(ls.ssp2)

ls.ssp3 <- proj.ssp3[,c(1,14:25)]
ls.ssp3$sector <- "ls"
ls.ssp3 <- ls.ssp3[,c(1,14,2:13)]
names(ls.ssp3) <- c("fips","sector","Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
ls.ssp3 <- linapprox(ls.ssp3)

ls.ssp4 <- proj.ssp4[,c(1,14:25)]
ls.ssp4$sector <- "ls"
ls.ssp4 <- ls.ssp4[,c(1,14,2:13)]
names(ls.ssp4) <- c("fips","sector","Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
ls.ssp4 <- linapprox(ls.ssp4)

ls.ssp5 <- proj.ssp5[,c(1,14:25)]
ls.ssp5$sector <- "ls"
ls.ssp5 <- ls.ssp5[,c(1,14,2:13)]
names(ls.ssp5) <- c("fips","sector","Y2015","Y2020","Y2025","Y2030","Y2035","Y2040","Y2045","Y2050","Y2055","Y2060","Y2065","Y2070")
ls.ssp5 <- linapprox(ls.ssp5)

#---------------------------------------------------------------------
# THERMO - uses appendix data from Diehl and Harris (2014) and EIA to project 
# water use at the county level

# First we use EIA state level energy data to determine residential and IC 
# thermo energy use per driver (pop and income respectively)
# Next we use Tom's growth and decay rates and project per driver energy demand
# Using driver data we calculate total annual demand per county.
# Next we allocate county demand to the energy region the county is located in. This data is used to project energy demand increases at the energy region level.
# Now we bring in water consumption by power plant from Deihl and Harris. These plants are mapped to a county, and assumed that these plants have demand related to energy regions.
# That is, if plant x is located in county y, and county y is located 30% in ERCOT and 70% in FRPP. We assume that 30% demand of the power plant comes from ERCOT and 70% from FRPP.
# We use the demand increased for each energy region to model increase in energy production at each power plant.
# Continuing our example. If ERCOT demand increases by 100% and FRPP increases by 50% between 5 year increment, then plant x increases production by (1.00*0.30)+(0.50*0.70).
# Water consumption from each power plant is projected using energy demand increases by energy regions. This is our final output for thermo at the county level.
#---------------------------------------------------------------------

#values are trillion BTU, for both residential and I/C. This is then converted to percent increases by county from year to year
#data comes from EIA
df<-read.csv('energyinput15.csv',header=TRUE)
df$commercial <- as.numeric(as.character(df$commercial))
df$industrial <- as.numeric(as.character(df$industrial))
df$icenergy <- df$commercial +df$industrial

#calculate energy demand excluding renewable energy
df$Total <- as.numeric(as.character(df$Total))
df$interstate.flow <- as.numeric(as.character(df$interestate.flow))
df$renewable <- as.numeric(as.character(df$renewable))
df$thermoprop <- ((df$Total - df$interestate.flow) - df$renewable)/(df$Total - df$interestate.flow)
df <- subset(df, df$State != "US")

#add population driver data
#Note that this is a different file than previously used for population. This file already has population aggregated to
#the state level. It comes from the same source, but was incorporated at an earlier date. It is kept only for ease.
pop <- read.csv('Wear_Population.csv', header=TRUE)
pop <-subset(pop, pop$COUNTY > 0)
pop <-subset(pop, pop$TYPE == 3)
pop <- pop[,c("NAME", "X2015","COUNTY")]
names(pop)[names(pop) == 'NAME'] <- 'State'
names(pop)[names(pop) == 'X2015'] <- 'pop'
df <- merge(df, pop, by="State")

#add income driver data
#Note: this is the same as the note above for population.
inc <- read.csv('Wear_PersIncome.csv', header=TRUE)
inc <-subset(inc, inc$COUNTY > 0)
inc <-subset(inc, inc$TYPE == 3)
inc <- inc[,c("NAME", "X2015")]
names(inc)[names(inc) == 'X2015'] <- 'inc'
names(inc)[names(inc) == 'NAME'] <- 'State'
df<- merge(df, inc, by="State")

#convert to thermo demand per income (for IC) or capita (for residential)
df$thres <- (((df$residential/ df$pop))*df$thermoprop)
df$thic <- (((df$icenergy / df$inc))*df$thermoprop)
df <- df[,c(1,19,21,22)]

#add growth and decay rates from Tom's paper (Methods for Projecting Largescale Annual Water Use in the United States)
df$growth <- ifelse(df$thres!="NA",0.0058,0)
df$decay <- ifelse(df$thres!="NA",-0.04,0)

#projecting residential thermoelectric demand at the state level
df$thres2015 <- df$thres
df$thres2020 = (df$thres*(1+df$growth*(1+df$decay)^(2020-2015))^5)
df$thres2025 = (df$thres2020*(1+df$growth*(1+df$decay)^(2025-2015))^5)
df$thres2030 = (df$thres2025*(1+df$growth*(1+df$decay)^(2030-2015))^5)
df$thres2035 = (df$thres2030*(1+df$growth*(1+df$decay)^(2035-2015))^5)
df$thres2040 = (df$thres2035*(1+df$growth*(1+df$decay)^(2040-2015))^5)
df$thres2045 = (df$thres2040*(1+df$growth*(1+df$decay)^(2045-2015))^5)
df$thres2050 = (df$thres2045*(1+df$growth*(1+df$decay)^(2050-2015))^5)
df$thres2055 = (df$thres2050*(1+df$growth*(1+df$decay)^(2055-2015))^5)
df$thres2060 = (df$thres2055*(1+df$growth*(1+df$decay)^(2060-2015))^5)
df$thres2065 = (df$thres2060*(1+df$growth*(1+df$decay)^(2065-2015))^5)
df$thres2070 = (df$thres2065*(1+df$growth*(1+df$decay)^(2070-2015))^5)
df$thres2075 = (df$thres2070*(1+df$growth*(1+df$decay)^(2075-2015))^5)
df$thres2080 = (df$thres2075*(1+df$growth*(1+df$decay)^(2080-2015))^5)
df$thres2085 = (df$thres2080*(1+df$growth*(1+df$decay)^(2085-2015))^5)
df$thres2090 = (df$thres2085*(1+df$growth*(1+df$decay)^(2090-2015))^5)
df$thres2095 = (df$thres2090*(1+df$growth*(1+df$decay)^(2095-2015))^5)
df$thres2100 = (df$thres2095*(1+df$growth*(1+df$decay)^(2100-2015))^5)

#projecting IC thermoelectric demand at the state level
df$thic2015 <- df$thic
df$thic2020 = (df$thic*(1+df$growth*(1+df$decay)^(2020-2015))^5)
df$thic2025 = (df$thic2020*(1+df$growth*(1+df$decay)^(2025-2015))^5)
df$thic2030 = (df$thic2025*(1+df$growth*(1+df$decay)^(2030-2015))^5)
df$thic2035 = (df$thic2030*(1+df$growth*(1+df$decay)^(2035-2015))^5)
df$thic2040 = (df$thic2035*(1+df$growth*(1+df$decay)^(2040-2015))^5)
df$thic2045 = (df$thic2040*(1+df$growth*(1+df$decay)^(2045-2015))^5)
df$thic2050 = (df$thic2045*(1+df$growth*(1+df$decay)^(2050-2015))^5)
df$thic2055 = (df$thic2050*(1+df$growth*(1+df$decay)^(2055-2015))^5)
df$thic2060 = (df$thic2055*(1+df$growth*(1+df$decay)^(2060-2015))^5)
df$thic2065 = (df$thic2060*(1+df$growth*(1+df$decay)^(2065-2015))^5)
df$thic2070 = (df$thic2065*(1+df$growth*(1+df$decay)^(2070-2015))^5)
df$thic2075 = (df$thic2070*(1+df$growth*(1+df$decay)^(2075-2015))^5)
df$thic2080 = (df$thic2075*(1+df$growth*(1+df$decay)^(2080-2015))^5)
df$thic2085 = (df$thic2080*(1+df$growth*(1+df$decay)^(2085-2015))^5)
df$thic2090 = (df$thic2085*(1+df$growth*(1+df$decay)^(2090-2015))^5)
df$thic2095 = (df$thic2090*(1+df$growth*(1+df$decay)^(2095-2015))^5)
df$thic2100 = (df$thic2095*(1+df$growth*(1+df$decay)^(2100-2015))^5)
df <- df[,c(1:3,7:24,4,25:42,5,6)]
#df is state level energy demand for thermo (ic and residential)

#produces county level driver data
#Note that this uses "waterdatafinal.csv", this is an old file and no data is actually coming from this. It is effectively a list of all the FIPS codes and year combinations needed.
#Note Incdata and Popdata are used to include 2010 data, it is an older file, but comes from the same source.
countyinc <- read.csv('Incdata.csv', header=TRUE)
waterdata <- read.csv('waterdatafinal.csv')
waterdata <- waterdata[waterdata$year==2010,]
waterdata <- waterdata[,c(2,4,46,47)]
colnames(waterdata)[colnames(waterdata)=="FIPS"] <- "fips"
countyinc <- merge(countyinc,waterdata, by="fips")
countyinc$COUNTY <- floor(countyinc$fips/1000) * 1000
countyinc <- countyinc[,c(1,2,16,3:14,17)]
countypop <- read.csv('Popdata.csv', header=TRUE)
countypop$COUNTY <- floor(countypop$fips/1000) * 1000

#merge county level population and total income (income per capita*population)
countymerge <- function(inc,pop)
{
  inc <- merge(inc, pop, by=c("fips","scenario"))
  for (n in 4:15){
    j <- n + 13
    inc[,n] <- as.numeric(inc[,n])
    inc[,j] <- as.numeric(inc[,j])
    inc[,n] <- inc[,n]*inc[,j]
  }
  return(inc)
}

countyinc<-countymerge(countyinc,countypop)
countyinc <-countyinc[,c(1,2,16:28,3:15,29)]
countyinc$fips <- ifelse(countyinc$fips == 51083, 51925, countyinc$fips)

countyinc.ssp1 <- countyinc[countyinc$scenario=="SSP1",]
countyinc.ssp2 <- countyinc[countyinc$scenario=="SSP2",]
countyinc.ssp3 <- countyinc[countyinc$scenario=="SSP3",]
countyinc.ssp4 <- countyinc[countyinc$scenario=="SSP4",]
countyinc.ssp5 <- countyinc[countyinc$scenario=="SSP5",]

# returns residential + commercial energy demand
# It doesn't matter where demand comes from for power plants so merge both 
# residential and commercial demands
driver <- function(driver,energy)
{
  energy <- merge(driver, energy, by="COUNTY")
  energy <-energy[,c(1:43,50:62)]
  for (n in 4:29){
    j <- n + 27
    energy[,n] <- as.numeric(energy[,n])
    energy[,j] <- as.numeric(energy[,j])
    energy[,j] <- energy[,n]*energy[,j]
  }
  for (n in 31:43){
    j <- n + 13
    energy[,n] <- as.numeric(energy[,n])
    energy[,j] <- as.numeric(energy[,j])
    energy[,n] <- energy[,n] + energy[,j]
  }
  return(energy[,c(1:3,31:43)])
}
energyproj.ssp1 <- driver(countyinc.ssp1,df)
energyproj.ssp2 <- driver(countyinc.ssp2,df)
energyproj.ssp3 <- driver(countyinc.ssp3,df)
energyproj.ssp4 <- driver(countyinc.ssp4,df)
energyproj.ssp5 <- driver(countyinc.ssp5,df)

#-----------------------------------------------------------------------------------------
#Bring in data for energy regions
#-----------------------------------------------------------------------------------------

#bring in data for each energy region (data mapping proportion of counties in each region)
ercot<-read.csv('Energy_ERCOT.csv',header=TRUE)
ercot<-subset(ercot, ercot$SUBNAME=="ERCOT ALL")
frcc<-read.csv('Energy_FRCC.csv',header=TRUE)
frcc<-subset(frcc, frcc$SUBNAME=="FRCC ALL")
mro<-read.csv('Energy_MRO.csv',header=TRUE)
mro<-subset(mro, mro$NAME_1=="MIDWEST RELIABILITY ORGANIZATION (MRO)")
npcc<-read.csv('Energy_NPCC.csv',header=TRUE)
npcc<-subset(npcc, npcc$NAME_1=="NORTHEAST POWER COORDINATING COUNCIL (NPCC)")
rfc<-read.csv('Energy_RFC.csv',header=TRUE)
rfc<-subset(rfc, rfc$NAME_1=="RELIABILITYFIRST CORPORATION (RFC)")
serc<-read.csv('Energy_SERC.csv',header=TRUE)
serc<-subset(serc, serc$NAME_1=="SERC RELIABILITY CORPORATION (SERC)")
spp<-read.csv('Energy_SPP.csv',header=TRUE)
spp<-subset(spp, spp$NAME_1=="SOUTHWEST POWER POOL, RE (SPP)")
wecc<-read.csv('Energy_WECC.csv',header=TRUE)
wecc<-subset(wecc, wecc$NAME_1=="WESTERN ELECTRICITY COORDINATING COUNCIL (WECC)")

#fixing VA FIPS codes
ercot<-ercot[,c(7,12,48)]
colnames(ercot)[1] <- "fips"
ercot <- fipsfixenergy(ercot)
colnames(ercot)[1] <- "GEOID"

frcc<-frcc[,c(7,12,48)]
colnames(frcc)[1] <- "fips"
frcc <- fipsfixenergy(frcc)
colnames(frcc)[1] <- "GEOID"

mro<-mro[,c(7,12,48)]
colnames(mro)[1] <- "fips"
mro <- fipsfixenergy(mro)
colnames(mro)[1] <- "GEOID"

npcc<-npcc[,c(7,12,48)]
colnames(npcc)[1] <- "fips"
npcc <- fipsfixenergy(npcc)
colnames(npcc)[1] <- "GEOID"

rfc<-rfc[,c(7,12,48)]
colnames(rfc)[1] <- "fips"
rfc <- fipsfixenergy(rfc)
colnames(rfc)[1] <- "GEOID"

serc<-serc[,c(7,12,48)]
colnames(serc)[1] <- "fips"
serc <- fipsfixenergy(serc)
colnames(serc)[1] <- "GEOID"

spp<-spp[,c(7,12,48)]
colnames(spp)[1] <- "fips"
spp <- fipsfixenergy(spp)
colnames(spp)[1] <- "GEOID"

wecc<-wecc[,c(7,12,48)]
colnames(wecc)[1] <- "fips"
wecc <- fipsfixenergy(wecc)
colnames(wecc)[1] <- "GEOID"

ercot$Area_mean <- ifelse(ercot$GEOID >51900 && ercot$GEOID <52000, ercot$Area_sum, ercot$Area_mean)
ercot<-ercot[,c(1,4,3)]
frcc$Area_mean <- ifelse(frcc$GEOID >51900 && frcc$GEOID <52000, frcc$Area_sum, frcc$Area_mean)
frcc<-frcc[,c(1,4,3)]
mro$Area_mean <- ifelse(mro$GEOID >51900 && mro$GEOID <52000, mro$Area_sum, mro$Area_mean)
mro<-mro[,c(1,4,3)]
npcc$Area_mean <- ifelse(npcc$GEOID >51900 && npcc$GEOID <52000, npcc$Area_sum, npcc$Area_mean)
npcc<-npcc[,c(1,4,3)]
rfc$Area_mean <- ifelse(rfc$GEOID >51900 && rfc$GEOID <52000, rfc$Area_sum, rfc$Area_mean)
rfc<-rfc[,c(1,4,3)]
serc$Area_mean <- ifelse(serc$GEOID >51900 && serc$GEOID <52000, serc$Area_sum, serc$Area_mean)
serc<-serc[,c(1,4,3)]
spp$Area_mean <- ifelse(spp$GEOID >51900 && spp$GEOID <52000, spp$Area_sum, spp$Area_mean)
spp<-spp[,c(1,4,3)]
wecc$Area_mean <- ifelse(wecc$GEOID >51900 && wecc$GEOID <52000, wecc$Area_sum, wecc$Area_mean)
wecc<-wecc[,c(1,4,3)]

#calculating the proportion of each county in each region
ercot$ercotprop <- (ercot$Shape_Area_sum/ercot$Area_mean)
frcc$frccprop <- (frcc$Shape_Area_sum/frcc$Area_mean)
mro$mroprop <- (mro$Shape_Area_sum/mro$Area_mean)
npcc$npccprop <- (npcc$Shape_Area_sum/npcc$Area_mean)
rfc$rfcprop <- (rfc$Shape_Area_sum/rfc$Area_mean)
serc$sercprop <- (serc$Shape_Area_sum/serc$Area_mean)
spp$sppprop <- (spp$Shape_Area_sum/spp$Area_mean)
wecc$weccprop <- (wecc$Shape_Area_sum/wecc$Area_mean)

ercot<-ercot[,c(1,4)]
frcc<-frcc[,c(1,4)]
mro<-mro[,c(1,4)]
npcc<-npcc[,c(1,4)]
rfc<-rfc[,c(1,4)]
serc<-serc[,c(1,4)]
spp<-spp[,c(1,4)]
wecc<-wecc[,c(1,4)]
wecc$totnerc <-NA

#bring in energy demanded by state from EIA data earlier

percentincrease <- function(proj)
{
  colnames(proj)[colnames(proj)=="fips"] <- "GEOID"
  energy<- merge(proj,frcc, by="GEOID", all.x=TRUE)
  energy<- merge(energy,ercot, by="GEOID", all.x=TRUE)
  energy<- merge(energy,mro, by="GEOID", all.x=TRUE)
  energy<- merge(energy,npcc, by="GEOID", all.x=TRUE)
  energy<- merge(energy,rfc, by="GEOID", all.x=TRUE)
  energy<- merge(energy,serc, by="GEOID", all.x=TRUE)
  energy<- merge(energy,spp, by="GEOID", all.x=TRUE)
  energy<- merge(energy,wecc, by="GEOID", all.x=TRUE)
  energy[is.na(energy)] <- 0
  
  #normalize proportions (overlapping regions cause greater than 100% energy demanded)
  energy[,25] <- energy[,17]+energy[,18]+energy[,19]+energy[,20]+energy[,21]+energy[,22]+energy[,23]+energy[,24]
  energy[, c(25)][energy[, c(25)] == 0] <- 1
  
  #normalizing proportions
  for (n in 17:24)
  {
    energy[,n] <- energy[,n]/energy[,25]
  }
  
  #calculating energy demand in each region by county
  #If adding more years k needs to increase. I.E. 0 to 12 corresponds to 2010 to 2070
  for (k in 0:12) 
  {
    h <- 4 + k
    for (n in 17:24)
    {
      j <- n+ 9 +(k*8)
      energy[,j] <- energy[,h]*energy[,n] 
    }
  }
  
  colnames(energy)[26] <- "frcc10"
  colnames(energy)[27] <- "ercot10"
  colnames(energy)[28] <- "mro10"
  colnames(energy)[29] <- "npcc10"
  colnames(energy)[30] <- "rfc10"
  colnames(energy)[31] <- "serc10"
  colnames(energy)[32] <- "spp10"
  colnames(energy)[33] <- "wecc10"
  
  colnames(energy)[34] <- "frcc15"
  colnames(energy)[35] <- "ercot15"
  colnames(energy)[36] <- "mro15"
  colnames(energy)[37] <- "npcc15"
  colnames(energy)[38] <- "rfc15"
  colnames(energy)[39] <- "serc15"
  colnames(energy)[40] <- "spp15"
  colnames(energy)[41] <- "wecc15"
  
  colnames(energy)[42] <- "frcc20"
  colnames(energy)[43] <- "ercot20"
  colnames(energy)[44] <- "mro20"
  colnames(energy)[45] <- "npcc20"
  colnames(energy)[46] <- "rfc20"
  colnames(energy)[47] <- "serc20"
  colnames(energy)[48] <- "spp20"
  colnames(energy)[49] <- "wecc20"
  
  colnames(energy)[50] <- "frcc25"
  colnames(energy)[51] <- "ercot25"
  colnames(energy)[52] <- "mro25"
  colnames(energy)[53] <- "npcc25"
  colnames(energy)[54] <- "rfc25"
  colnames(energy)[55] <- "serc25"
  colnames(energy)[56] <- "spp25"
  colnames(energy)[57] <- "wecc25"
  
  colnames(energy)[58] <- "frcc30"
  colnames(energy)[59] <- "ercot30"
  colnames(energy)[60] <- "mro30"
  colnames(energy)[61] <- "npcc30"
  colnames(energy)[62] <- "rfc30"
  colnames(energy)[63] <- "serc30"
  colnames(energy)[64] <- "spp30"
  colnames(energy)[65] <- "wecc30"
  
  colnames(energy)[66] <- "frcc35"
  colnames(energy)[67] <- "ercot35"
  colnames(energy)[68] <- "mro35"
  colnames(energy)[69] <- "npcc35"
  colnames(energy)[70] <- "rfc35"
  colnames(energy)[71] <- "serc35"
  colnames(energy)[72] <- "spp35"
  colnames(energy)[73] <- "wecc35"
  
  colnames(energy)[74] <- "frcc40"
  colnames(energy)[75] <- "ercot40"
  colnames(energy)[76] <- "mro40"
  colnames(energy)[77] <- "npcc40"
  colnames(energy)[78] <- "rfc40"
  colnames(energy)[79] <- "serc40"
  colnames(energy)[80] <- "spp40"
  colnames(energy)[81] <- "wecc40"
  
  colnames(energy)[82] <- "frcc45"
  colnames(energy)[83] <- "ercot45"
  colnames(energy)[84] <- "mro45"
  colnames(energy)[85] <- "npcc45"
  colnames(energy)[86] <- "rfc45"
  colnames(energy)[87] <- "serc45"
  colnames(energy)[88] <- "spp45"
  colnames(energy)[89] <- "wecc45"
  
  colnames(energy)[90] <- "frcc50"
  colnames(energy)[91] <- "ercot50"
  colnames(energy)[92] <- "mro50"
  colnames(energy)[93] <- "npcc50"
  colnames(energy)[94] <- "rfc50"
  colnames(energy)[95] <- "serc50"
  colnames(energy)[96] <- "spp50"
  colnames(energy)[97] <- "wecc50"
  
  colnames(energy)[98] <- "frcc55"
  colnames(energy)[99] <- "ercot55"
  colnames(energy)[100] <- "mro55"
  colnames(energy)[101] <- "npcc55"
  colnames(energy)[102] <- "rfc55"
  colnames(energy)[103] <- "serc55"
  colnames(energy)[104] <- "spp55"
  colnames(energy)[105] <- "wecc55"
  
  colnames(energy)[106] <- "frcc60"
  colnames(energy)[107] <- "ercot60"
  colnames(energy)[108] <- "mro60"
  colnames(energy)[109] <- "npcc60"
  colnames(energy)[110] <- "rfc60"
  colnames(energy)[111] <- "serc60"
  colnames(energy)[112] <- "spp60"
  colnames(energy)[113] <- "wecc60"
  
  colnames(energy)[114] <- "frcc65"
  colnames(energy)[115] <- "ercot65"
  colnames(energy)[116] <- "mro65"
  colnames(energy)[117] <- "npcc65"
  colnames(energy)[118] <- "rfc65"
  colnames(energy)[119] <- "serc65"
  colnames(energy)[120] <- "spp65"
  colnames(energy)[121] <- "wecc65"
  
  colnames(energy)[122] <- "frcc70"
  colnames(energy)[123] <- "ercot70"
  colnames(energy)[124] <- "mro70"
  colnames(energy)[125] <- "npcc70"
  colnames(energy)[126] <- "rfc70"
  colnames(energy)[127] <- "serc70"
  colnames(energy)[128] <- "spp70"
  colnames(energy)[129] <- "wecc70"
  
  #calculating percent increase for energy region by year
  #next calculating percent increase multiplied by the proportion for that county (applying increase % to county)
  #finally adding up each increase in region for each county (% increase for wecc for county X + % increase for frcc for county X + etc)
  
  #calculate % increase for each energy region and then multiplied by proportion of region for each county
  for (h in 0:11)
  {
    
    for (n in 1:8)
    {
      m <- 16+n
      j <- n + 129 + (h*9)
      k <- 33+n + (h*8)
      p <- 25 + n + (h*8)
      energy[,j]<- (((sum(energy[,k])) - (sum(energy[,p])))/(sum(energy[,p])))*energy[,m]
      #  x <- % increase of energy demand over 5 years mutliplied by proportion
    }
    q <- (h*9) + 138
    a<- q - 1
    b<- q - 2
    c<- q - 3
    d<- q - 4
    e<- q - 5
    f<- q - 6
    g<- q - 7
    i<- q - 8
    
    energy[,q] <- energy[,a] + energy[,b]+ energy[,c]+ energy[,d]+ energy[,e]+ energy[,f]+ energy[,g]+ energy[,i]
    
  }
  colnames(energy)[138] <- "increase2015"
  colnames(energy)[147] <- "increase2020"
  colnames(energy)[156] <- "increase2025"
  colnames(energy)[165] <- "increase2030"
  colnames(energy)[174] <- "increase2035"
  colnames(energy)[183] <- "increase2040"
  colnames(energy)[192] <- "increase2045"
  colnames(energy)[201] <- "increase2050"
  colnames(energy)[210] <- "increase2055"
  colnames(energy)[219] <- "increase2060"
  colnames(energy)[228] <- "increase2065"
  colnames(energy)[237] <- "increase2070"
  
  #keeping percent increase energy demand for county
  energy<-energy[,c(1,3,138,147,156,165,174,183,192,201,210,219,228,237)]
  
  return(energy)
}

energyproj.ssp1 <- percentincrease(energyproj.ssp1)
energyproj.ssp2 <- percentincrease(energyproj.ssp2)
energyproj.ssp3 <- percentincrease(energyproj.ssp3)
energyproj.ssp4 <- percentincrease(energyproj.ssp4)
energyproj.ssp5 <- percentincrease(energyproj.ssp5)

#-------------------------------------------------------------------
#Bring in Deihl and Harris thermo water consumption data
#-------------------------------------------------------------------

#Diehl and Harris data has every power plant in US in 2015. This includes freshwater and brackish water for example.
#This section filters out non-fresh water power plants. In cases where there is a combination (brackish and fresh) we assume an even split (50% fresh and 50% brackish).
TH <-read.csv("TH_r_2015.csv", header=TRUE)
TH <- filter(TH,TH$WATER_TYPE_CODE=="FR" | TH$WATER_TYPE_CODE=="FR & BE")
TH$TH_WD <- ifelse(TH$WATER_TYPE_CODE=="FR & BE",(TH$TH_WD/2),TH$TH_WD )
TH[,c(1)] <- ifelse(TH[,c(1)] == 51059, 51919, TH[,c(1)])
TH[,c(1)] <- ifelse(TH[,c(1)] == 51083, 51925, TH[,c(1)])
TH[,c(1)] <- ifelse(TH[,c(1)] == 51143, 51939, TH[,c(1)])

fipscodes <- dp.ssp1$fips
TH <- TH[TH$FIPS %in% fipscodes ,]
TH_final <- TH[,c(1,12)]
colnames(TH_final)[colnames(TH_final)=="FIPS"] <- "GEOID"
TH_final <- TH_final %>% group_by(GEOID) %>% summarise_all(sum)

#------------------------------------------------------------------
#Using water consumption from Diehl and Harris we apply increased demand at the energy region level.

#applying increase to plant water consumption
th.ssp1 <- merge(energyproj.ssp1, TH_final,by="GEOID", all.x=TRUE)
colnames(th.ssp1)[colnames(th.ssp1)=="TH_WD"] <- "th2015"
th.ssp1$th2020 <- th.ssp1$th2015*(1+th.ssp1$increase2020)
th.ssp1$th2025 <- th.ssp1$th2020*(1+th.ssp1$increase2025)
th.ssp1$th2030 <- th.ssp1$th2025*(1+th.ssp1$increase2030)
th.ssp1$th2035 <- th.ssp1$th2030*(1+th.ssp1$increase2035)
th.ssp1$th2040 <- th.ssp1$th2035*(1+th.ssp1$increase2040)
th.ssp1$th2045 <- th.ssp1$th2040*(1+th.ssp1$increase2045)
th.ssp1$th2050 <- th.ssp1$th2045*(1+th.ssp1$increase2050)
th.ssp1$th2055 <- th.ssp1$th2050*(1+th.ssp1$increase2055)
th.ssp1$th2060 <- th.ssp1$th2055*(1+th.ssp1$increase2060)
th.ssp1$th2065 <- th.ssp1$th2060*(1+th.ssp1$increase2065)
th.ssp1$th2070 <- th.ssp1$th2065*(1+th.ssp1$increase2070)
colnames(th.ssp1)[colnames(th.ssp1)=="GEOID"] <- "FIPS"
th.ssp1<-th.ssp1[,c(1,15:26)]
th.ssp1[is.na(th.ssp1)]<-0

th.ssp2 <- merge(energyproj.ssp2, TH_final,by="GEOID", all.x=TRUE)
colnames(th.ssp2)[colnames(th.ssp2)=="TH_WD"] <- "th2015"
th.ssp2$th2020 <- th.ssp2$th2015*(1+th.ssp2$increase2020)
th.ssp2$th2025 <- th.ssp2$th2020*(1+th.ssp2$increase2025)
th.ssp2$th2030 <- th.ssp2$th2025*(1+th.ssp2$increase2030)
th.ssp2$th2035 <- th.ssp2$th2030*(1+th.ssp2$increase2035)
th.ssp2$th2040 <- th.ssp2$th2035*(1+th.ssp2$increase2040)
th.ssp2$th2045 <- th.ssp2$th2040*(1+th.ssp2$increase2045)
th.ssp2$th2050 <- th.ssp2$th2045*(1+th.ssp2$increase2050)
th.ssp2$th2055 <- th.ssp2$th2050*(1+th.ssp2$increase2055)
th.ssp2$th2060 <- th.ssp2$th2055*(1+th.ssp2$increase2060)
th.ssp2$th2065 <- th.ssp2$th2060*(1+th.ssp2$increase2065)
th.ssp2$th2070 <- th.ssp2$th2065*(1+th.ssp2$increase2070)
colnames(th.ssp2)[colnames(th.ssp2)=="GEOID"] <- "FIPS"
th.ssp2<-th.ssp2[,c(1,15:26)]
th.ssp2[is.na(th.ssp2)]<-0

th.ssp3 <- merge(energyproj.ssp3, TH_final,by="GEOID", all.x=TRUE)
colnames(th.ssp3)[colnames(th.ssp3)=="TH_WD"] <- "th2015"
th.ssp3$th2020 <- th.ssp3$th2015*(1+th.ssp3$increase2020)
th.ssp3$th2025 <- th.ssp3$th2020*(1+th.ssp3$increase2025)
th.ssp3$th2030 <- th.ssp3$th2025*(1+th.ssp3$increase2030)
th.ssp3$th2035 <- th.ssp3$th2030*(1+th.ssp3$increase2035)
th.ssp3$th2040 <- th.ssp3$th2035*(1+th.ssp3$increase2040)
th.ssp3$th2045 <- th.ssp3$th2040*(1+th.ssp3$increase2045)
th.ssp3$th2050 <- th.ssp3$th2045*(1+th.ssp3$increase2050)
th.ssp3$th2055 <- th.ssp3$th2050*(1+th.ssp3$increase2055)
th.ssp3$th2060 <- th.ssp3$th2055*(1+th.ssp3$increase2060)
th.ssp3$th2065 <- th.ssp3$th2060*(1+th.ssp3$increase2065)
th.ssp3$th2070 <- th.ssp3$th2065*(1+th.ssp3$increase2070)
colnames(th.ssp3)[colnames(th.ssp3)=="GEOID"] <- "FIPS"
th.ssp3<-th.ssp3[,c(1,15:26)]
th.ssp3[is.na(th.ssp3)]<-0

th.ssp4 <- merge(energyproj.ssp4, TH_final,by="GEOID", all.x=TRUE)
colnames(th.ssp4)[colnames(th.ssp4)=="TH_WD"] <- "th2015"
th.ssp4$th2020 <- th.ssp4$th2015*(1+th.ssp4$increase2020)
th.ssp4$th2025 <- th.ssp4$th2020*(1+th.ssp4$increase2025)
th.ssp4$th2030 <- th.ssp4$th2025*(1+th.ssp4$increase2030)
th.ssp4$th2035 <- th.ssp4$th2030*(1+th.ssp4$increase2035)
th.ssp4$th2040 <- th.ssp4$th2035*(1+th.ssp4$increase2040)
th.ssp4$th2045 <- th.ssp4$th2040*(1+th.ssp4$increase2045)
th.ssp4$th2050 <- th.ssp4$th2045*(1+th.ssp4$increase2050)
th.ssp4$th2055 <- th.ssp4$th2050*(1+th.ssp4$increase2055)
th.ssp4$th2060 <- th.ssp4$th2055*(1+th.ssp4$increase2060)
th.ssp4$th2065 <- th.ssp4$th2060*(1+th.ssp4$increase2065)
th.ssp4$th2070 <- th.ssp4$th2065*(1+th.ssp4$increase2070)
colnames(th.ssp4)[colnames(th.ssp4)=="GEOID"] <- "FIPS"
th.ssp4<-th.ssp4[,c(1,15:26)]
th.ssp4[is.na(th.ssp4)]<-0

th.ssp5 <- merge(energyproj.ssp5, TH_final,by="GEOID", all.x=TRUE)
colnames(th.ssp5)[colnames(th.ssp5)=="TH_WD"] <- "th2015"
th.ssp5$th2020 <- th.ssp5$th2015*(1+th.ssp5$increase2020)
th.ssp5$th2025 <- th.ssp5$th2020*(1+th.ssp5$increase2025)
th.ssp5$th2030 <- th.ssp5$th2025*(1+th.ssp5$increase2030)
th.ssp5$th2035 <- th.ssp5$th2030*(1+th.ssp5$increase2035)
th.ssp5$th2040 <- th.ssp5$th2035*(1+th.ssp5$increase2040)
th.ssp5$th2045 <- th.ssp5$th2040*(1+th.ssp5$increase2045)
th.ssp5$th2050 <- th.ssp5$th2045*(1+th.ssp5$increase2050)
th.ssp5$th2055 <- th.ssp5$th2050*(1+th.ssp5$increase2055)
th.ssp5$th2060 <- th.ssp5$th2055*(1+th.ssp5$increase2060)
th.ssp5$th2065 <- th.ssp5$th2060*(1+th.ssp5$increase2065)
th.ssp5$th2070 <- th.ssp5$th2065*(1+th.ssp5$increase2070)
colnames(th.ssp5)[colnames(th.ssp5)=="GEOID"] <- "FIPS"
th.ssp5<-th.ssp5[,c(1,15:26)]
th.ssp5[is.na(th.ssp5)]<-0

#---------------------------------------------------------------------
# Creates linear approximations for in between years. Converts thermo 
# data to match formats of other sectors.
#---------------------------------------------------------------------

colnames(th.ssp1) <- c("fips", "Y2015","Y2020", "Y2025", "Y2030", "Y2035", "Y2040", "Y2045", "Y2050", "Y2055", "Y2060", "Y2065", "Y2070")
colnames(th.ssp2) <- c("fips", "Y2015","Y2020", "Y2025", "Y2030", "Y2035", "Y2040", "Y2045", "Y2050", "Y2055", "Y2060", "Y2065", "Y2070")
colnames(th.ssp3) <- c("fips", "Y2015","Y2020", "Y2025", "Y2030", "Y2035", "Y2040", "Y2045", "Y2050", "Y2055", "Y2060", "Y2065", "Y2070")
colnames(th.ssp4) <- c("fips", "Y2015", "Y2020","Y2025", "Y2030", "Y2035", "Y2040", "Y2045", "Y2050", "Y2055", "Y2060", "Y2065", "Y2070")
colnames(th.ssp5) <- c("fips", "Y2015","Y2020", "Y2025", "Y2030", "Y2035", "Y2040", "Y2045", "Y2050", "Y2055", "Y2060", "Y2065", "Y2070")

th.ssp1$sector <- NA
th.ssp2$sector <- NA
th.ssp3$sector <- NA
th.ssp4$sector <- NA
th.ssp5$sector <- NA

th.ssp1 <- th.ssp1[,c(1,14,2:13)]
th.ssp2 <- th.ssp2[,c(1,14,2:13)]
th.ssp3 <- th.ssp3[,c(1,14,2:13)]
th.ssp4 <- th.ssp4[,c(1,14,2:13)]
th.ssp5 <- th.ssp5[,c(1,14,2:13)]

th.ssp1 <-linapprox(th.ssp1)
th.ssp2 <-linapprox(th.ssp2)
th.ssp3 <-linapprox(th.ssp3)
th.ssp4 <-linapprox(th.ssp4)
th.ssp5 <-linapprox(th.ssp5)
  
#=====================================================================================================

#The following section is code from Ro

# ---------------------------------------------------------------------
# Climate change impacts from tempertature rise
#
# WRR-specific "multiplier" to represent the effect of temperature increase on the
# per capita electricity consumption rate as per Tom's calculations from 2010 assessment
#
# ----------------------------------------------------------------------
mm <- read.csv("thermoTempMultiplier.csv", header = TRUE, check.names = FALSE)

#calculate proportion of FIPS in each WRR
df<-cntypercent
df$WRR <-floor(signif(df$HUC_10,2)/100000000)
df<-df[,c(1:3,8,6)]
df <- df %>% group_by(FIPS,WRR) %>% summarise(Shape_Area=sum(Shape_Area),CountyArea=mean(CountyArea))
df$pctwrr <- df$Shape_Area/df$CountyArea
df1 <- df[,c(1,5)]
df1 <- df1 %>% group_by(FIPS) %>% summarise_all(funs(sum))

df <- merge(df, df1, by="FIPS")
df$pctwrr.x <- df$pctwrr.x/df$pctwrr.y
df<- df[,c(1,2,5)]
colnames(df)[colnames(df)=="FIPS"] <- "fips"
proj <-df

mm <- merge(mm, proj, by = "WRR")

mm[,c(2:20)] <- data.frame(apply(mm[,c(2:20)], 2, function (WRRlevel) {
  countyLevel <- WRRlevel*mm$pctwrr.x
  return(countyLevel)
} ))
mm <- mm %>% group_by(fips) %>% summarise_all(funs(sum))
mm <- mm[,-c(2,22)]

#create dataframe for each climate scenario annual temperatures
tempFiles <-list.files (path = "ClimateData", pattern = "^T_")
tempFiles <-strsplit(tempFiles, ".csv")

for(i in tempFiles){
  filepath <- file.path("ClimateData",paste(i,".csv",sep=""))
  assign(i, read.csv(filepath, header = TRUE, check.names = FALSE))
}

#aggregate temp to annual
names(tempFiles) <- tempFiles
for (i in tempFiles){
  df <- get(i)
  df<- df[,-c(1)]
  df <- df %>% group_by(year) 
  df <- setDT(df)[, lapply(.SD, mean), by = year]
  df <- df[,-c(2)]
  nam <- paste0(strsplit(names(tempFiles[i]), split = "_Monthly"), "_annual")
  assign(nam, df)
}

HUC8toCounty <- cntypercent
HUC8toCounty$HUC_8 <- 0
for (i in 1:nrow(HUC8toCounty)){
  if (nchar(HUC8toCounty$HUC_10[i]) == 10){
    HUC8toCounty$HUC_8[i] <- substr(HUC8toCounty$HUC_10[i], 1,8)
  } else{
    HUC8toCounty$HUC_8[i] <- substr(HUC8toCounty$HUC_10[i], 1,7)
  }
}
HUC8toCounty <- HUC8toCounty %>% group_by(FIPS, HUC_8) %>% summarise(Shape_Area=sum(Shape_Area),CountyArea=mean(CountyArea))
HUC8toCounty$pctHUC8 <- HUC8toCounty$Shape_Area/HUC8toCounty$CountyArea
HUC8toCounty <- HUC8toCounty[,c(1:2,5)]
df1 <- HUC8toCounty[,c(1,3)]
df1 <- df1 %>% group_by(FIPS) %>% summarise_all(funs(sum))

HUC8toCounty <- merge(HUC8toCounty, df1, by="FIPS")
HUC8toCounty$pctHUC8.x <- HUC8toCounty$pctHUC8.x/HUC8toCounty$pctHUC8.y
HUC8toCounty<- HUC8toCounty[,c(1:3)]
colnames(HUC8toCounty)[colnames(HUC8toCounty)=="FIPS"] <- "fips"
HUC8toCounty$HUC_8 <- as.numeric(HUC8toCounty$HUC_8)

# convert HUC8 temp to county level for each scenario
tempAnnual <- as.list(ls(pattern = "annual"))

for (i in tempAnnual) {
  df <- get(i)
  df <- df[,-c(1:2)]
  df <- as.data.frame(t(as.matrix(df)))
  year <- seq(1950, 2099, 1)
  colnames(df) <- year
  df$HUC_8 <- as.numeric(rownames(df))
  rownames(df) <- c()
  df <- merge(df, HUC8toCounty, by =  "HUC_8", no.dups = FALSE)
  df[,c(2:151)] <- data.frame(apply(df[,c(2:151)], 2, function(HUC8level){
    countyLevel <- HUC8level*df$pctHUC8
    return(countyLevel)
  } ))
  df <- df[,-c(1)]
  df <- df %>% group_by(fips) %>% summarise_all(funs(sum))
  assign(i, df)
}

#calculate annual temp changes and apply mm by fips
#linearly approximate mm for missing years
mm <- mm[,-c(15:20)]
colnames(mm)[2:14] <- paste0("Y", colnames(mm)[2:14])
mm <- linapprox(mm)

for (ii in tempAnnual){
  df <- get(ii)
  
  #this portion calculates annual temp change from year to year
  # df <- df[,-c(2:60, 123:152)] #drop unneeded years
  # for (a in 3:ncol(df)){  #change in units of temp
  #   b <- a - 1
  #   c <- a + 61
  #   df[,c] <- ( df[,a] - df[,b] )
  # }
  # df <- df[, -c(2:68)] #from 2015 only
  # colnames(df)[2:ncol(df)] <-  seq(2015, 2070, 1)
  
  # creating temp changes from the baseline -- let's say the 10-year average before 2015
  df <- df[,-c(2:56, 123:152)]
  df$baselineAvg <- rowSums(df[,c(2:11)])/10
  df <- df[,-c(2:11)]
  for (a in 2:57){
    df[,c(a)] <- df[,c(a)] - df$baselineAvg
  }
  colnames(df)[2:57] <-  seq(2015, 2070, 1)
  df <- df[,-c(58)]
  
  #apply mm by fips level
  for (d in 2:ncol(df)) {
    df[,d] <- df[,d]*(mm[,d]-1)
  }
  assign(ii,df)
}

#apply temperature impacts to thermo water use for each scenario
listSSP <- as.list(ls(pattern = "th.ssp"))
for (ii in tempAnnual) {
  for (jj in listSSP){
    df <- get(ii)
    th <- get(jj)
    df <- merge(df, th, by = "fips")
    for (i in 2:57) {
      k <- i + 57
      df[,i] <- (1 + df[,i])*df[,k]
    }
    df <- df[,-c(58:ncol(df))]
    colnames(df)[2:ncol(df)] <- paste0("th", colnames(df)[2:ncol(df)] )
    df$sector <- NA
    df <- cbind(df[,c(1)], df[,c(58)] ,df[,c(2:57)])
    colnames(df)[1:2] <- c("fips", "sector")
    assign(paste0(jj,ii), df) #change the naming convention here
  }
}

#--------------------------------------------------------------------------------------

#=====================================================================================================


#This part is messy, but takes the global variables and picks the thermo results (from Ro) that correspond to the correct model.

if(carbon==45 & gcm=="cnrm_c5"){
  th.ssp5 <- th.ssp5T_CNRM_C5_45_annual
  th.ssp4 <- th.ssp4T_CNRM_C5_45_annual
  th.ssp3 <- th.ssp3T_CNRM_C5_45_annual
  th.ssp2 <- th.ssp2T_CNRM_C5_45_annual
  th.ssp1 <- th.ssp1T_CNRM_C5_45_annual
}
if(carbon==85 & gcm=="cnrm_c5"){
  th.ssp5 <- th.ssp5T_CNRM_C5_85_annual
  th.ssp4 <- th.ssp4T_CNRM_C5_85_annual
  th.ssp3 <- th.ssp3T_CNRM_C5_85_annual
  th.ssp2 <- th.ssp2T_CNRM_C5_85_annual
  th.ssp1 <- th.ssp1T_CNRM_C5_85_annual
}
if(carbon==45 & gcm=="hadgem"){
  th.ssp5 <- th.ssp5T_HadGEM_45_annual
  th.ssp4 <- th.ssp4T_HadGEM_45_annual
  th.ssp3 <- th.ssp3T_HadGEM_45_annual
  th.ssp2 <- th.ssp2T_HadGEM_45_annual
  th.ssp1 <- th.ssp1T_HadGEM_45_annual
}
if(carbon==85 & gcm=="hadgem"){
  th.ssp5 <- th.ssp5T_HadGEM_85_annual
  th.ssp4 <- th.ssp4T_HadGEM_85_annual
  th.ssp3 <- th.ssp3T_HadGEM_85_annual
  th.ssp2 <- th.ssp2T_HadGEM_85_annual
  th.ssp1 <- th.ssp1T_HadGEM_85_annual
}
if(carbon==45 & gcm=="ipsl_cm5a"){
  th.ssp5 <- th.ssp5T_IPSL_CM5A_45_annual
  th.ssp4 <- th.ssp4T_IPSL_CM5A_45_annual
  th.ssp3 <- th.ssp3T_IPSL_CM5A_45_annual
  th.ssp2 <- th.ssp2T_IPSL_CM5A_45_annual
  th.ssp1 <- th.ssp1T_IPSL_CM5A_45_annual
}
if(carbon==85 & gcm=="ipsl_cm5a"){
  th.ssp5 <- th.ssp5T_IPSL_CM5A_85_annual
  th.ssp4 <- th.ssp4T_IPSL_CM5A_85_annual
  th.ssp3 <- th.ssp3T_IPSL_CM5A_85_annual
  th.ssp2 <- th.ssp2T_IPSL_CM5A_85_annual
  th.ssp1 <- th.ssp1T_IPSL_CM5A_85_annual
}
if(carbon==45 & gcm=="mri_cgcm3"){
  th.ssp5 <- th.ssp5T_MRI_CGCM3_45_annual
  th.ssp4 <- th.ssp4T_MRI_CGCM3_45_annual
  th.ssp3 <- th.ssp3T_MRI_CGCM3_45_annual
  th.ssp2 <- th.ssp2T_MRI_CGCM3_45_annual
  th.ssp1 <- th.ssp1T_MRI_CGCM3_45_annual
}
if(carbon==85 & gcm=="mri_cgcm3"){
  th.ssp5 <- th.ssp5T_MRI_CGCM3_85_annual
  th.ssp4 <- th.ssp4T_MRI_CGCM3_85_annual
  th.ssp3 <- th.ssp3T_MRI_CGCM3_85_annual
  th.ssp2 <- th.ssp2T_MRI_CGCM3_85_annual
  th.ssp1 <- th.ssp1T_MRI_CGCM3_85_annual
}
if(carbon==45 & gcm=="noresm"){
  th.ssp5 <- th.ssp5T_NorESM_45_annual
  th.ssp4 <- th.ssp4T_NorESM_45_annual
  th.ssp3 <- th.ssp3T_NorESM_45_annual
  th.ssp2 <- th.ssp2T_NorESM_45_annual
  th.ssp1 <- th.ssp1T_NorESM_45_annual
}
if(carbon==85 & gcm=="noresm"){
  th.ssp5 <- th.ssp5T_NorESM_85_annual
  th.ssp4 <- th.ssp4T_NorESM_85_annual
  th.ssp3 <- th.ssp3T_NorESM_85_annual
  th.ssp2 <- th.ssp2T_NorESM_85_annual
  th.ssp1 <- th.ssp1T_NorESM_85_annual
}

#---------------------------------------------------------------------
# Brings in consumptive proportions and then converts data to consumptive 
# water use.
# Note Thermo is already in consumptive use.
#---------------------------------------------------------------------
cons <- read.csv(file="inputs_ssp1.csv")
cons <- cons[,c(2,24,9,26:27,25)]

#function to convert withdrawal data to consumptive water use
#NUM = 2 for IC, 3 for DP, 4 for LS, 5 for IR, 6 for AQ
consumption <- function(DF, NUM)
{
  cons1 <- cons[,c(1,NUM)]
  DF <- merge(DF, cons1, by="fips")
  DF[,1] <- as.numeric(DF[,1])
  for (n in 3:58){
    DF[,n]<- DF[,n]*DF[,59]
  }
  return(DF[order(DF[,1]),c(1:58)])
}

ic.ssp1 <-consumption(ic.ssp1, 2)
ic.ssp2 <-consumption(ic.ssp2, 2)
ic.ssp3 <-consumption(ic.ssp3, 2)
ic.ssp4 <-consumption(ic.ssp4, 2)
ic.ssp5 <-consumption(ic.ssp5, 2)

dp.ssp1 <- consumption(dp.ssp1, 3)
dp.ssp2 <- consumption(dp.ssp2, 3)
dp.ssp3 <- consumption(dp.ssp3, 3)
dp.ssp4 <- consumption(dp.ssp4, 3)
dp.ssp5 <- consumption(dp.ssp5, 3)

ls.ssp1 <- consumption(ls.ssp1, 4)
ls.ssp2 <- consumption(ls.ssp2, 4)
ls.ssp3 <- consumption(ls.ssp3, 4)
ls.ssp4 <- consumption(ls.ssp4, 4)
ls.ssp5 <- consumption(ls.ssp5, 4)

ir <- consumption(ir, 5)

aq.ssp1 <- consumption(aq.ssp1, 6)
aq.ssp2 <- consumption(aq.ssp2, 6)
aq.ssp3 <- consumption(aq.ssp3, 6)
aq.ssp4 <- consumption(aq.ssp4, 6)
aq.ssp5 <- consumption(aq.ssp5, 6)

#=====================================================================================================
#This section allows for county level final results
#only used when picking out specific results

#=====================================================================================================ic.ssp1.county <-ic.ssp1
# ic.ssp2.county <-ic.ssp2
# ic.ssp3.county <-ic.ssp3
# ic.ssp4.county <-ic.ssp4
# ic.ssp5.county <-ic.ssp5
# 
# dp.ssp1.county <-dp.ssp1
# dp.ssp2.county <-dp.ssp2
# dp.ssp3.county <-dp.ssp3
# dp.ssp4.county <-dp.ssp4
# dp.ssp5.county <-dp.ssp5
# 
# ls.ssp1.county <-ls.ssp1
# ls.ssp2.county <-ls.ssp2
# ls.ssp3.county <-ls.ssp3
# ls.ssp4.county <-ls.ssp4
# ls.ssp5.county <-ls.ssp5
# 
# th.ssp1.county <-th.ssp1
# th.ssp2.county <-th.ssp2
# th.ssp3.county <-th.ssp3
# th.ssp4.county <-th.ssp4
# th.ssp5.county <-th.ssp5
# 
# aq.ssp1.county <-aq.ssp1
# aq.ssp2.county <-aq.ssp2
# aq.ssp3.county <-aq.ssp3
# aq.ssp4.county <-aq.ssp4
# aq.ssp5.county <-aq.ssp5
# 
# ir.county <- ir
#  
# 
# #This function specifies which counties to pick out results for
# countypick<- function(DF)
# {
#   DF <- DF[DF$fips %in% c(8001,8005,8013,8014,8019,8031,8035,8037,8041,8045,8047,8049,8057,8059,8069,8075,8087,8093,8107,8115,8117,8119,8121,8123,31033,31049,31105,56001,56007,56021),]
#   
#   return(DF)
# }
# 
# dp.ssp1.county <- countypick(dp.ssp1.county)
# dp.ssp2.county <- countypick(dp.ssp2.county)
# dp.ssp3.county <- countypick(dp.ssp3.county)
# dp.ssp4.county <- countypick(dp.ssp4.county)
# dp.ssp5.county <- countypick(dp.ssp5.county)
# 
# ls.ssp1.county <- countypick(ls.ssp1.county)
# ls.ssp2.county <- countypick(ls.ssp2.county)
# ls.ssp3.county <- countypick(ls.ssp3.county)
# ls.ssp4.county <- countypick(ls.ssp4.county)
# ls.ssp5.county <- countypick(ls.ssp5.county)
# 
# th.ssp1.county <- countypick(th.ssp1.county)
# th.ssp2.county <- countypick(th.ssp2.county)
# th.ssp3.county <- countypick(th.ssp3.county)
# th.ssp4.county <- countypick(th.ssp4.county)
# th.ssp5.county <- countypick(th.ssp5.county)
# 
# aq.ssp1.county <- countypick(aq.ssp1.county)
# aq.ssp2.county <- countypick(aq.ssp2.county)
# aq.ssp3.county <- countypick(aq.ssp3.county)
# aq.ssp4.county <- countypick(aq.ssp4.county)
# aq.ssp5.county <- countypick(aq.ssp5.county)
# 
# ic.ssp1.county <- countypick(ic.ssp1.county)
# ic.ssp2.county <- countypick(ic.ssp2.county)
# ic.ssp3.county <- countypick(ic.ssp3.county)
# ic.ssp4.county <- countypick(ic.ssp4.county)
# ic.ssp5.county <- countypick(ic.ssp5.county)
# 
# ir.county <- countypick(ir.county)
# 
# ssp2.county <- countypick(countyinc.ssp2)
# ssp2.county <- ssp2.county[,c(1,4:15,17:28)]
# 
# 
# write.csv(dp.ssp1.county, file="arnf_dp_ssp1.csv")
# write.csv(dp.ssp2.county, file="arnf_dp_ssp2.csv")
# write.csv(dp.ssp3.county, file="arnf_dp_ssp3.csv")
# write.csv(dp.ssp4.county, file="arnf_dp_ssp4.csv")
# write.csv(dp.ssp5.county, file="arnf_dp_ssp5.csv")
# 
# write.csv(ic.ssp1.county, file="arnf_ic_ssp1.csv")
# write.csv(ic.ssp2.county, file="arnf_ic_ssp2.csv")
# write.csv(ic.ssp3.county, file="arnf_ic_ssp3.csv")
# write.csv(ic.ssp4.county, file="arnf_ic_ssp4.csv")
# write.csv(ic.ssp5.county, file="arnf_ic_ssp5.csv")
# 
# write.csv(aq.ssp1.county, file="arnf_aq_ssp1.csv")
# write.csv(aq.ssp2.county, file="arnf_aq_ssp2.csv")
# write.csv(aq.ssp3.county, file="arnf_aq_ssp3.csv")
# write.csv(aq.ssp4.county, file="arnf_aq_ssp4.csv")
# write.csv(aq.ssp5.county, file="arnf_aq_ssp5.csv")
# 
# write.csv(ls.ssp1.county, file="arnf_ls_ssp1.csv")
# write.csv(ls.ssp2.county, file="arnf_ls_ssp2.csv")
# write.csv(ls.ssp3.county, file="arnf_ls_ssp3.csv")
# write.csv(ls.ssp4.county, file="arnf_ls_ssp4.csv")
# write.csv(ls.ssp5.county, file="arnf_ls_ssp5.csv")
# 
# write.csv(th.ssp1.county, file="arnf_th_ssp1.csv")
# write.csv(th.ssp2.county, file="arnf_th_ssp2.csv")
# write.csv(th.ssp3.county, file="arnf_th_ssp3.csv")
# write.csv(th.ssp4.county, file="arnf_th_ssp4.csv")
# write.csv(th.ssp5.county, file="arnf_th_ssp5.csv")
# 
# write.csv(ir.county, file="arnf_ir.csv")
# write.csv(ssp2.county, file="arnf_popinc.csv")

#=====================================================================================================

#----------------------------------------------------------------------------------------------------------------------
# Transforms every sector into HUC4s. Calculations are done at the county level, 
# but input for WEAP needs to be in HUC4s.
#----------------------------------------------------------------------------------------------------------------------

df<-cntypercent
df$HUC4 <-signif(df$HUC_10,4)/1000000
df<-df[,c(1:3,8,6)]
df <- df %>% group_by(FIPS,HUC4) %>% summarise(Shape_Area=sum(Shape_Area),CountyArea=mean(CountyArea))
df$pcthuc8 <- df$Shape_Area/df$CountyArea
df1 <- df[,c(1,5)]
df1 <- df1 %>% group_by(FIPS) %>% summarise_all(funs(sum))

df <- merge(df, df1, by="FIPS")
df$pcthuc8.x <- df$pcthuc8.x/df$pcthuc8.y
df<- df[,c(1,2,5)]
colnames(df)[colnames(df)=="FIPS"] <- "fips"

huc <- function(DF,sect)
{
  DF <- merge(DF, df, by="fips")
  DF[,1] <- as.numeric(DF[,1])
  for (n in 3:58){
    DF[,n]<- DF[,n]*DF[,60]
  }
  DF <- DF[,c(1,3:60)]
  DF <- DF%>% group_by(HUC4) %>% summarise_all(funs(sum))
  DF$sector <- sect
  DF <- DF[,c(1,60,3:58)]
  return(DF[,c(1:58)])
}
ic.ssp1 <- huc(ic.ssp1, "ic")
ic.ssp2 <- huc(ic.ssp2, "ic")
ic.ssp3 <- huc(ic.ssp3, "ic")
ic.ssp4 <- huc(ic.ssp4, "ic")
ic.ssp5 <- huc(ic.ssp5, "ic")

dp.ssp1 <- huc(dp.ssp1, "dp")
dp.ssp2 <- huc(dp.ssp2, "dp")
dp.ssp3 <- huc(dp.ssp3, "dp")
dp.ssp4 <- huc(dp.ssp4, "dp")
dp.ssp5 <- huc(dp.ssp5, "dp")

ls.ssp1 <- huc(ls.ssp1, "ls")
ls.ssp2 <- huc(ls.ssp2, "ls")
ls.ssp3 <- huc(ls.ssp3, "ls")
ls.ssp4 <- huc(ls.ssp4, "ls")
ls.ssp5 <- huc(ls.ssp5, "ls")

ir <- huc(ir, "ir")

aq.ssp1 <- huc(aq.ssp1, "aq")
aq.ssp2 <- huc(aq.ssp2, "aq")
aq.ssp3 <- huc(aq.ssp3, "aq")
aq.ssp4 <- huc(aq.ssp4, "aq")
aq.ssp5 <- huc(aq.ssp5, "aq")

th.ssp1 <-huc(th.ssp1, "th")
th.ssp2 <-huc(th.ssp2, "th")
th.ssp3 <-huc(th.ssp3, "th")
th.ssp4 <-huc(th.ssp4, "th")
th.ssp5 <-huc(th.ssp5, "th")


# create one large dataframe with results

aq.ssp1$ssp <- 'ssp1'
aq.ssp2$ssp <- 'ssp2'
aq.ssp3$ssp <- 'ssp3'
aq.ssp4$ssp <- 'ssp4'
aq.ssp5$ssp <- 'ssp5'

dp.ssp1$ssp <- 'ssp1'
dp.ssp2$ssp <- 'ssp2'
dp.ssp3$ssp <- 'ssp3'
dp.ssp4$ssp <- 'ssp4'
dp.ssp5$ssp <- 'ssp5'

ic.ssp1$ssp <- 'ssp1'
ic.ssp2$ssp <- 'ssp2'
ic.ssp3$ssp <- 'ssp3'
ic.ssp4$ssp <- 'ssp4'
ic.ssp5$ssp <- 'ssp5'

ls.ssp1$ssp <- 'ssp1'
ls.ssp2$ssp <- 'ssp2'
ls.ssp3$ssp <- 'ssp3'
ls.ssp4$ssp <- 'ssp4'
ls.ssp5$ssp <- 'ssp5'

th.ssp1$ssp <- 'ssp1'
th.ssp2$ssp <- 'ssp2'
th.ssp3$ssp <- 'ssp3'
th.ssp4$ssp <- 'ssp4'
th.ssp5$ssp <- 'ssp5'

ir$ssp <- 'ssp1'

# need to change column headiings for thermo

colnames(th.ssp1) <- colnames(ic.ssp1)
colnames(th.ssp2) <- colnames(ic.ssp2)
colnames(th.ssp3) <- colnames(ic.ssp3)
colnames(th.ssp4) <- colnames(ic.ssp4)
colnames(th.ssp5) <- colnames(ic.ssp5)

# Results of wd for climate scenario. These results are in a format that is difficult to work with
results <- rbind(
  dp.ssp1, dp.ssp2, dp.ssp3, dp.ssp4, dp.ssp5,
  ic.ssp1, ic.ssp2, ic.ssp3, ic.ssp4, ic.ssp5,
  ls.ssp1, ls.ssp2, ls.ssp3, ls.ssp4, ls.ssp5,
  aq.ssp1, aq.ssp2, aq.ssp3, aq.ssp4, aq.ssp5,
  th.ssp1, th.ssp2, th.ssp3, th.ssp4, th.ssp5)

# To make results easier to work with, begin by adding new columns for "year" and "climate scenario"                               
results$year <- NA
results$gcm <- NA

## - redoing Shaunie's code but with counties. I skipped the part of Ryan's code that coveretd to HUCs
#--------------------

# Create new data frames that subset wd for each year. This code is currently repetitive
# and should be rewritten to loop through year columns and create each new data frame                               
Y2015 <- select(results, fips, sector, ssp, Y2015, year, gcm)
Y2015$year <- "2015"
names(Y2015)[names(Y2015) == "Y2015"] <- "Demand"

Y2016 <- select(results, fips, sector, ssp, Y2016, year, gcm)
Y2016$year <- "2016"
names(Y2016)[names(Y2016) == "Y2016"] <- "Demand"

Y2017 <- select(results, fips, sector, ssp, Y2017, year, gcm)
Y2017$year <- "2017"
names(Y2017)[names(Y2017) == "Y2017"] <- "Demand"

Y2018 <- select(results, fips, sector, ssp, Y2018, year, gcm)
Y2018$year <- "2018"
names(Y2018)[names(Y2018) == "Y2018"] <- "Demand"

Y2019 <- select(results, fips, sector, ssp, Y2019, year, gcm)
Y2019$year <- "2019"
names(Y2019)[names(Y2019) == "Y2019"] <- "Demand"

Y2020 <- select(results, fips, sector, ssp, Y2020, year, gcm)
Y2020$year <- "2020"
names(Y2020)[names(Y2020) == "Y2020"] <- "Demand"

Y2021 <- select(results, fips, sector, ssp, Y2021, year, gcm)
Y2021$year <- "2021"
names(Y2021)[names(Y2021) == "Y2021"] <- "Demand"

Y2022 <- select(results, fips, sector, ssp, Y2022, year, gcm)
Y2022$year <- "2022"
names(Y2022)[names(Y2022) == "Y2022"] <- "Demand"

Y2023 <- select(results, fips, sector, ssp, Y2023, year, gcm)
Y2023$year <- "2023"
names(Y2023)[names(Y2023) == "Y2023"] <- "Demand"

Y2024 <- select(results, fips, sector, ssp, Y2024, year, gcm)
Y2024$year <- "2024"
names(Y2024)[names(Y2024) == "Y2024"] <- "Demand"

Y2025 <- select(results, fips, sector, ssp, Y2025, year, gcm)
Y2025$year <- "2025"
names(Y2025)[names(Y2025) == "Y2025"] <- "Demand"

Y2026 <- select(results, fips, sector, ssp, Y2026, year, gcm)
Y2026$year <- "2026"
names(Y2026)[names(Y2026) == "Y2026"] <- "Demand"

Y2027 <- select(results, fips, sector, ssp, Y2027, year, gcm)
Y2027$year <- "2027"
names(Y2027)[names(Y2027) == "Y2027"] <- "Demand"

Y2028 <- select(results, fips, sector, ssp, Y2028, year, gcm)
Y2028$year <- "2028"
names(Y2028)[names(Y2028) == "Y2028"] <- "Demand"

Y2029 <- select(results, fips, sector, ssp, Y2029, year, gcm)
Y2029$year <- "2029"
names(Y2029)[names(Y2029) == "Y2029"] <- "Demand"

Y2030 <- select(results, fips, sector, ssp, Y2030, year, gcm)
Y2030$year <- "2030"
names(Y2030)[names(Y2030) == "Y2030"] <- "Demand"

Y2031 <- select(results, fips, sector, ssp, Y2031, year, gcm)
Y2031$year <- "2031"
names(Y2031)[names(Y2031) == "Y2031"] <- "Demand"

Y2032 <- select(results, fips, sector, ssp, Y2032, year, gcm)
Y2032$year <- "2032"
names(Y2032)[names(Y2032) == "Y2032"] <- "Demand"

Y2033 <- select(results, fips, sector, ssp, Y2033, year, gcm)
Y2033$year <- "2033"
names(Y2033)[names(Y2033) == "Y2033"] <- "Demand"

Y2034 <- select(results, fips, sector, ssp, Y2034, year, gcm)
Y2034$year <- "2034"
names(Y2034)[names(Y2034) == "Y2034"] <- "Demand"

Y2035 <- select(results, fips, sector, ssp, Y2035, year, gcm)
Y2035$year <- "2035"
names(Y2035)[names(Y2035) == "Y2035"] <- "Demand"

Y2036 <- select(results, fips, sector, ssp, Y2036, year, gcm)
Y2036$year <- "2036"
names(Y2036)[names(Y2036) == "Y2036"] <- "Demand"

Y2037 <- select(results, fips, sector, ssp, Y2037, year, gcm)
Y2037$year <- "2037"
names(Y2037)[names(Y2037) == "Y2037"] <- "Demand"

Y2038 <- select(results, fips, sector, ssp, Y2038, year, gcm)
Y2038$year <- "2038"
names(Y2038)[names(Y2038) == "Y2038"] <- "Demand"

Y2039 <- select(results, fips, sector, ssp, Y2039, year, gcm)
Y2039$year <- "2039"
names(Y2039)[names(Y2039) == "Y2039"] <- "Demand"

Y2040 <- select(results, fips, sector, ssp, Y2040, year, gcm)
Y2040$year <- "2040"
names(Y2040)[names(Y2040) == "Y2040"] <- "Demand"

Y2041 <- select(results, fips, sector, ssp, Y2041, year, gcm)
Y2041$year <- "2041"
names(Y2041)[names(Y2041) == "Y2041"] <- "Demand"

Y2042 <- select(results, fips, sector, ssp, Y2042, year, gcm)
Y2042$year <- "2042"
names(Y2042)[names(Y2042) == "Y2042"] <- "Demand"

Y2043 <- select(results, fips, sector, ssp, Y2043, year, gcm)
Y2043$year <- "2043"
names(Y2043)[names(Y2043) == "Y2043"] <- "Demand"

Y2044 <- select(results, fips, sector, ssp, Y2044, year, gcm)
Y2044$year <- "2044"
names(Y2044)[names(Y2044) == "Y2044"] <- "Demand"

Y2045 <- select(results, fips, sector, ssp, Y2045, year, gcm)
Y2045$year <- "2045"
names(Y2045)[names(Y2045) == "Y2045"] <- "Demand"

Y2046 <- select(results, fips, sector, ssp, Y2046, year, gcm)
Y2046$year <- "2046"
names(Y2046)[names(Y2046) == "Y2046"] <- "Demand"

Y2047 <- select(results, fips, sector, ssp, Y2047, year, gcm)
Y2047$year <- "2047"
names(Y2047)[names(Y2047) == "Y2047"] <- "Demand"

Y2048 <- select(results, fips, sector, ssp, Y2048, year, gcm)
Y2048$year <- "2048"
names(Y2048)[names(Y2048) == "Y2048"] <- "Demand"

Y2049 <- select(results, fips, sector, ssp, Y2049, year, gcm)
Y2049$year <- "2049"
names(Y2049)[names(Y2049) == "Y2049"] <- "Demand"

Y2050 <- select(results, fips, sector, ssp, Y2050, year, gcm)
Y2050$year <- "2050"
names(Y2050)[names(Y2050) == "Y2050"] <- "Demand"

Y2051 <- select(results, fips, sector, ssp, Y2051, year, gcm)
Y2051$year <- "2051"
names(Y2051)[names(Y2051) == "Y2051"] <- "Demand"

Y2052 <- select(results, fips, sector, ssp, Y2052, year, gcm)
Y2052$year <- "2052"
names(Y2052)[names(Y2052) == "Y2052"] <- "Demand"

Y2053 <- select(results, fips, sector, ssp, Y2053, year, gcm)
Y2053$year <- "2053"
names(Y2053)[names(Y2053) == "Y2053"] <- "Demand"

Y2054 <- select(results, fips, sector, ssp, Y2054, year, gcm)
Y2054$year <- "2054"
names(Y2054)[names(Y2054) == "Y2054"] <- "Demand"

Y2055 <- select(results, fips, sector, ssp, Y2055, year, gcm)
Y2055$year <- "2055"
names(Y2055)[names(Y2055) == "Y2055"] <- "Demand"

Y2056 <- select(results, fips, sector, ssp, Y2056, year, gcm)
Y2056$year <- "2056"
names(Y2056)[names(Y2056) == "Y2056"] <- "Demand"

Y2057 <- select(results, fips, sector, ssp, Y2057, year, gcm)
Y2057$year <- "2057"
names(Y2057)[names(Y2057) == "Y2057"] <- "Demand"

Y2058 <- select(results, fips, sector, ssp, Y2058, year, gcm)
Y2058$year <- "2058"
names(Y2058)[names(Y2058) == "Y2058"] <- "Demand"

Y2059 <- select(results, fips, sector, ssp, Y2059, year, gcm)
Y2059$year <- "2059"
names(Y2059)[names(Y2059) == "Y2059"] <- "Demand"

Y2060 <- select(results, fips, sector, ssp, Y2060, year, gcm)
Y2060$year <- "2060"
names(Y2060)[names(Y2060) == "Y2060"] <- "Demand"

Y2061 <- select(results, fips, sector, ssp, Y2061, year, gcm)
Y2061$year <- "2061"
names(Y2061)[names(Y2061) == "Y2061"] <- "Demand"

Y2062 <- select(results, fips, sector, ssp, Y2062, year, gcm)
Y2062$year <- "2062"
names(Y2062)[names(Y2062) == "Y2062"] <- "Demand"

Y2063 <- select(results, fips, sector, ssp, Y2063, year, gcm)
Y2063$year <- "2063"
names(Y2063)[names(Y2063) == "Y2063"] <- "Demand"

Y2064 <- select(results, fips, sector, ssp, Y2064, year, gcm)
Y2064$year <- "2064"
names(Y2064)[names(Y2064) == "Y2064"] <- "Demand"

Y2065 <- select(results, fips, sector, ssp, Y2065, year, gcm)
Y2065$year <- "2065"
names(Y2065)[names(Y2065) == "Y2065"] <- "Demand"

Y2066 <- select(results, fips, sector, ssp, Y2066, year, gcm)
Y2066$year <- "2066"
names(Y2066)[names(Y2066) == "Y2066"] <- "Demand"

Y2067 <- select(results, fips, sector, ssp, Y2067, year, gcm)
Y2067$year <- "2067"
names(Y2067)[names(Y2067) == "Y2067"] <- "Demand"

Y2068 <- select(results, fips, sector, ssp, Y2068, year, gcm)
Y2068$year <- "2068"
names(Y2068)[names(Y2068) == "Y2068"] <- "Demand"

Y2069 <- select(results, fips, sector, ssp, Y2069, year, gcm)
Y2069$year <- "2069"
names(Y2069)[names(Y2069) == "Y2069"] <- "Demand"

Y2070 <- select(results, fips, sector, ssp, Y2070, year, gcm)
Y2070$year <- "2070"
names(Y2070)[names(Y2070) == "Y2070"] <- "Demand"


### -- if you skipped the above and are using HUCs use what is below here
#------------------


# Create new data frames that subset wd for each year. This code is currently repetitive
# and should be rewritten to loop through year columns and create each new data frame                               
Y2015 <- select(results, HUC4, sector, ssp, Y2015, year, gcm)
Y2015$year <- "2015"
names(Y2015)[names(Y2015) == "Y2015"] <- "Demand"

Y2016 <- select(results, HUC4, sector, ssp, Y2016, year, gcm)
Y2016$year <- "2016"
names(Y2016)[names(Y2016) == "Y2016"] <- "Demand"

Y2017 <- select(results, HUC4, sector, ssp, Y2017, year, gcm)
Y2017$year <- "2017"
names(Y2017)[names(Y2017) == "Y2017"] <- "Demand"

Y2018 <- select(results, HUC4, sector, ssp, Y2018, year, gcm)
Y2018$year <- "2018"
names(Y2018)[names(Y2018) == "Y2018"] <- "Demand"

Y2019 <- select(results, HUC4, sector, ssp, Y2019, year, gcm)
Y2019$year <- "2019"
names(Y2019)[names(Y2019) == "Y2019"] <- "Demand"

Y2020 <- select(results, HUC4, sector, ssp, Y2020, year, gcm)
Y2020$year <- "2020"
names(Y2020)[names(Y2020) == "Y2020"] <- "Demand"

Y2021 <- select(results, HUC4, sector, ssp, Y2021, year, gcm)
Y2021$year <- "2021"
names(Y2021)[names(Y2021) == "Y2021"] <- "Demand"

Y2022 <- select(results, HUC4, sector, ssp, Y2022, year, gcm)
Y2022$year <- "2022"
names(Y2022)[names(Y2022) == "Y2022"] <- "Demand"

Y2023 <- select(results, HUC4, sector, ssp, Y2023, year, gcm)
Y2023$year <- "2023"
names(Y2023)[names(Y2023) == "Y2023"] <- "Demand"

Y2024 <- select(results, HUC4, sector, ssp, Y2024, year, gcm)
Y2024$year <- "2024"
names(Y2024)[names(Y2024) == "Y2024"] <- "Demand"

Y2025 <- select(results, HUC4, sector, ssp, Y2025, year, gcm)
Y2025$year <- "2025"
names(Y2025)[names(Y2025) == "Y2025"] <- "Demand"

Y2026 <- select(results, HUC4, sector, ssp, Y2026, year, gcm)
Y2026$year <- "2026"
names(Y2026)[names(Y2026) == "Y2026"] <- "Demand"

Y2027 <- select(results, HUC4, sector, ssp, Y2027, year, gcm)
Y2027$year <- "2027"
names(Y2027)[names(Y2027) == "Y2027"] <- "Demand"

Y2028 <- select(results, HUC4, sector, ssp, Y2028, year, gcm)
Y2028$year <- "2028"
names(Y2028)[names(Y2028) == "Y2028"] <- "Demand"

Y2029 <- select(results, HUC4, sector, ssp, Y2029, year, gcm)
Y2029$year <- "2029"
names(Y2029)[names(Y2029) == "Y2029"] <- "Demand"

Y2030 <- select(results, HUC4, sector, ssp, Y2030, year, gcm)
Y2030$year <- "2030"
names(Y2030)[names(Y2030) == "Y2030"] <- "Demand"

Y2031 <- select(results, HUC4, sector, ssp, Y2031, year, gcm)
Y2031$year <- "2031"
names(Y2031)[names(Y2031) == "Y2031"] <- "Demand"

Y2032 <- select(results, HUC4, sector, ssp, Y2032, year, gcm)
Y2032$year <- "2032"
names(Y2032)[names(Y2032) == "Y2032"] <- "Demand"

Y2033 <- select(results, HUC4, sector, ssp, Y2033, year, gcm)
Y2033$year <- "2033"
names(Y2033)[names(Y2033) == "Y2033"] <- "Demand"

Y2034 <- select(results, HUC4, sector, ssp, Y2034, year, gcm)
Y2034$year <- "2034"
names(Y2034)[names(Y2034) == "Y2034"] <- "Demand"

Y2035 <- select(results, HUC4, sector, ssp, Y2035, year, gcm)
Y2035$year <- "2035"
names(Y2035)[names(Y2035) == "Y2035"] <- "Demand"

Y2036 <- select(results, HUC4, sector, ssp, Y2036, year, gcm)
Y2036$year <- "2036"
names(Y2036)[names(Y2036) == "Y2036"] <- "Demand"

Y2037 <- select(results, HUC4, sector, ssp, Y2037, year, gcm)
Y2037$year <- "2037"
names(Y2037)[names(Y2037) == "Y2037"] <- "Demand"

Y2038 <- select(results, HUC4, sector, ssp, Y2038, year, gcm)
Y2038$year <- "2038"
names(Y2038)[names(Y2038) == "Y2038"] <- "Demand"

Y2039 <- select(results, HUC4, sector, ssp, Y2039, year, gcm)
Y2039$year <- "2039"
names(Y2039)[names(Y2039) == "Y2039"] <- "Demand"

Y2040 <- select(results, HUC4, sector, ssp, Y2040, year, gcm)
Y2040$year <- "2040"
names(Y2040)[names(Y2040) == "Y2040"] <- "Demand"

Y2041 <- select(results, HUC4, sector, ssp, Y2041, year, gcm)
Y2041$year <- "2041"
names(Y2041)[names(Y2041) == "Y2041"] <- "Demand"

Y2042 <- select(results, HUC4, sector, ssp, Y2042, year, gcm)
Y2042$year <- "2042"
names(Y2042)[names(Y2042) == "Y2042"] <- "Demand"

Y2043 <- select(results, HUC4, sector, ssp, Y2043, year, gcm)
Y2043$year <- "2043"
names(Y2043)[names(Y2043) == "Y2043"] <- "Demand"

Y2044 <- select(results, HUC4, sector, ssp, Y2044, year, gcm)
Y2044$year <- "2044"
names(Y2044)[names(Y2044) == "Y2044"] <- "Demand"

Y2045 <- select(results, HUC4, sector, ssp, Y2045, year, gcm)
Y2045$year <- "2045"
names(Y2045)[names(Y2045) == "Y2045"] <- "Demand"

Y2046 <- select(results, HUC4, sector, ssp, Y2046, year, gcm)
Y2046$year <- "2046"
names(Y2046)[names(Y2046) == "Y2046"] <- "Demand"

Y2047 <- select(results, HUC4, sector, ssp, Y2047, year, gcm)
Y2047$year <- "2047"
names(Y2047)[names(Y2047) == "Y2047"] <- "Demand"

Y2048 <- select(results, HUC4, sector, ssp, Y2048, year, gcm)
Y2048$year <- "2048"
names(Y2048)[names(Y2048) == "Y2048"] <- "Demand"

Y2049 <- select(results, HUC4, sector, ssp, Y2049, year, gcm)
Y2049$year <- "2049"
names(Y2049)[names(Y2049) == "Y2049"] <- "Demand"

Y2050 <- select(results, HUC4, sector, ssp, Y2050, year, gcm)
Y2050$year <- "2050"
names(Y2050)[names(Y2050) == "Y2050"] <- "Demand"

Y2051 <- select(results, HUC4, sector, ssp, Y2051, year, gcm)
Y2051$year <- "2051"
names(Y2051)[names(Y2051) == "Y2051"] <- "Demand"

Y2052 <- select(results, HUC4, sector, ssp, Y2052, year, gcm)
Y2052$year <- "2052"
names(Y2052)[names(Y2052) == "Y2052"] <- "Demand"

Y2053 <- select(results, HUC4, sector, ssp, Y2053, year, gcm)
Y2053$year <- "2053"
names(Y2053)[names(Y2053) == "Y2053"] <- "Demand"

Y2054 <- select(results, HUC4, sector, ssp, Y2054, year, gcm)
Y2054$year <- "2054"
names(Y2054)[names(Y2054) == "Y2054"] <- "Demand"

Y2055 <- select(results, HUC4, sector, ssp, Y2055, year, gcm)
Y2055$year <- "2055"
names(Y2055)[names(Y2055) == "Y2055"] <- "Demand"

Y2056 <- select(results, HUC4, sector, ssp, Y2056, year, gcm)
Y2056$year <- "2056"
names(Y2056)[names(Y2056) == "Y2056"] <- "Demand"

Y2057 <- select(results, HUC4, sector, ssp, Y2057, year, gcm)
Y2057$year <- "2057"
names(Y2057)[names(Y2057) == "Y2057"] <- "Demand"

Y2058 <- select(results, HUC4, sector, ssp, Y2058, year, gcm)
Y2058$year <- "2058"
names(Y2058)[names(Y2058) == "Y2058"] <- "Demand"

Y2059 <- select(results, HUC4, sector, ssp, Y2059, year, gcm)
Y2059$year <- "2059"
names(Y2059)[names(Y2059) == "Y2059"] <- "Demand"

Y2060 <- select(results, HUC4, sector, ssp, Y2060, year, gcm)
Y2060$year <- "2060"
names(Y2060)[names(Y2060) == "Y2060"] <- "Demand"

Y2061 <- select(results, HUC4, sector, ssp, Y2061, year, gcm)
Y2061$year <- "2061"
names(Y2061)[names(Y2061) == "Y2061"] <- "Demand"

Y2062 <- select(results, HUC4, sector, ssp, Y2062, year, gcm)
Y2062$year <- "2062"
names(Y2062)[names(Y2062) == "Y2062"] <- "Demand"

Y2063 <- select(results, HUC4, sector, ssp, Y2063, year, gcm)
Y2063$year <- "2063"
names(Y2063)[names(Y2063) == "Y2063"] <- "Demand"

Y2064 <- select(results, HUC4, sector, ssp, Y2064, year, gcm)
Y2064$year <- "2064"
names(Y2064)[names(Y2064) == "Y2064"] <- "Demand"

Y2065 <- select(results, HUC4, sector, ssp, Y2065, year, gcm)
Y2065$year <- "2065"
names(Y2065)[names(Y2065) == "Y2065"] <- "Demand"

Y2066 <- select(results, HUC4, sector, ssp, Y2066, year, gcm)
Y2066$year <- "2066"
names(Y2066)[names(Y2066) == "Y2066"] <- "Demand"

Y2067 <- select(results, HUC4, sector, ssp, Y2067, year, gcm)
Y2067$year <- "2067"
names(Y2067)[names(Y2067) == "Y2067"] <- "Demand"

Y2068 <- select(results, HUC4, sector, ssp, Y2068, year, gcm)
Y2068$year <- "2068"
names(Y2068)[names(Y2068) == "Y2068"] <- "Demand"

Y2069 <- select(results, HUC4, sector, ssp, Y2069, year, gcm)
Y2069$year <- "2069"
names(Y2069)[names(Y2069) == "Y2069"] <- "Demand"

Y2070 <- select(results, HUC4, sector, ssp, Y2070, year, gcm)
Y2070$year <- "2070"
names(Y2070)[names(Y2070) == "Y2070"] <- "Demand"

# Bind together resulting data frames for all years                             
results <- rbind(Y2015,
                    Y2016, Y2017, Y2018, Y2019,
                    Y2020, Y2021, Y2022, Y2023,
                    Y2024, Y2025, Y2026, Y2027,
                    Y2028, Y2029, Y2030, Y2031,
                    Y2032, Y2033, Y2034, Y2035,
                    Y2036, Y2037, Y2038, Y2039,
                    Y2040, Y2041, Y2042, Y2043,
                    Y2044, Y2045, Y2046, Y2047,
                    Y2048, Y2049, Y2050, Y2051,
                    Y2052, Y2053, Y2054, Y2055,
                    Y2056, Y2057, Y2058, Y2059,
                    Y2060, Y2061, Y2062, Y2063,
                    Y2064, Y2065, Y2066, Y2067,
                    Y2068, Y2069, Y2070)
                                 
# Specify the climate scenario in the "gcm" column
results$gcm <- "cnrm_c5"
results$rcp <- 45

# recall the core scenarios:

# High-high: RCP 8.5, SSP5
# High-med: RCP 8.5, SSP2
# High-low: RCP 8.5, SSP3
# Low-med: RCP 4.5, SSP1

results$pop <- NA
results$acres <- NA
results$region <- NA
results$wpu <- NA # withdrawals per unit


# fake pop - need to replace



results.ssp1 <- results
results.ssp1 <- merge(results.ssp1, water1, by="fips")


results$wpu <- results$Demand / results$pop

# -- Figures ---

# plot of dp withdrawals over time

dp.results <- subset(results, sector=="dp")
dp.results.ssp1 <- subset(dp.results, ssp=="ssp1")
dp.results.ssp2 <- subset(dp.results, ssp=="ssp2")

dp.res.ssp1.cnrm <- aggregate(dp.results.ssp1$Demand, by=list(Category=dp.results.ssp1$year), FUN=sum)
dp.res.ssp2.cnrm <- aggregate(dp.results.ssp2$Demand, by=list(Category=dp.results.ssp2$year), FUN=sum)

colnames(dp.res.ssp1.cnrm) <- c('year','dp.spp1.cnrm')
colnames(dp.res.ssp2.cnrm) <- c('year2','dp.spp2.cnrm')

dp.res <- cbind(dp.res.ssp1.cnrm, dp.res.ssp2.cnrm)

summary(dp.res.ssp1.cnrm)

dp.res$year <- as.numeric(dp.res$year)
dp.res$dp.ssp1.cnrm <- as.numeric(dp.res$dp.ssp1.cnrm)

length(dp.res$year)

### Why isn't this working with the dp.res data?
plot(dp.res$year, dp.res.ssp1.cnrm$dp.spp1.cnrm, type="l")
lines(dp.res$year, dp.res.ssp2.cnrm$dp.spp2.cnrm)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#OUTPUTTING COUNTY RESULTS BY SECTOR
#this section outputs all projections at the county level by sector. Units are in Mgal per day rather than cubic meters (needed for WEAP)
#if comparison to WEAP output is needed, then move this section under the unit conversion section currently at line 3182.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write.csv(aq.ssp1, file="aq.ssp1.csv")
write.csv(aq.ssp2, file="aq.ssp2.csv")
write.csv(aq.ssp3, file="aq.ssp3.csv")
write.csv(aq.ssp4, file="aq.ssp4.csv")
write.csv(aq.ssp5, file="aq.ssp5.csv")

write.csv(dp.ssp1, file="dp.ssp1.csv")
write.csv(dp.ssp2, file="dp.ssp2.csv")
write.csv(dp.ssp3, file="dp.ssp3.csv")
write.csv(dp.ssp4, file="dp.ssp4.csv")
write.csv(dp.ssp5, file="dp.ssp5.csv")

write.csv(ic.ssp1, file="ic.ssp1.csv")
write.csv(ic.ssp2, file="ic.ssp2.csv")
write.csv(ic.ssp3, file="ic.ssp3.csv")
write.csv(ic.ssp4, file="ic.ssp4.csv")
write.csv(ic.ssp5, file="ic.ssp5.csv")

write.csv(th.ssp1, file="th.ssp1.csv")
write.csv(th.ssp2, file="th.ssp2.csv")
write.csv(th.ssp3, file="th.ssp3.csv")
write.csv(th.ssp4, file="th.ssp4.csv")
write.csv(th.ssp5, file="th.ssp5.csv")

write.csv(ir, file="ir.csv")

#=====================================================================================================
#converting to consumptive use per capita for DP
#Note this is not necessary for WEAP but can be helpful for looking at results. Units are still in Mgal per day, so output is mgal per day per person
# pop1$sector <- NA
# pop1<- pop1[,c(1,58,2:57)]
# pop2$sector <- NA
# pop2<- pop2[,c(1,58,2:57)]
# pop3$sector <- NA
# pop3<- pop3[,c(1,58,2:57)]
# pop4$sector <- NA
# pop4<- pop4[,c(1,58,2:57)]
# pop5$sector <- NA
# pop5<- pop5[,c(1,58,2:57)]
# 
# pop1 <- huc(pop1, "pop")
# pop2 <- huc(pop2, "pop")
# pop3 <- huc(pop3, "pop")
# pop4 <- huc(pop4, "pop")
# pop5 <- huc(pop5, "pop")
# 
# wpu <- function(wd,pop,sect)
# {
#   wd <- merge(wd, pop, by="HUC4")
#   for (n in 3:58){
#     j<-n+57
#     wd[,n]<- (wd[,n]/wd[,j])/1000 #population is in 1,000s so divide by 1000
#   }
#   wd <- wd[,c(1,3:58)]
#   wd$sector <- sect
#   wd <- wd[,c(1,58,2:57)]
#   return(wd[,c(1:58)])
# }
# 
# dp.wpu.ssp1 <- wpu(dp.ssp1, pop1, "dp")
# dp.wpu.ssp2 <- wpu(dp.ssp2, pop2, "dp")
# dp.wpu.ssp3 <- wpu(dp.ssp3, pop3, "dp")
# dp.wpu.ssp4 <- wpu(dp.ssp4, pop4, "dp")
# dp.wpu.ssp5 <- wpu(dp.ssp5, pop5, "dp")
# 
# write.csv(dp.wpu.ssp1, file="dp_wpu_ssp1.csv")
# write.csv(dp.wpu.ssp2, file="dp_wpu_ssp2.csv")
# write.csv(dp.wpu.ssp3, file="dp_wpu_ssp3.csv")
# write.csv(dp.wpu.ssp4, file="dp_wpu_ssp4.csv")
# write.csv(dp.wpu.ssp5, file="dp_wpu_ssp5.csv")

#---------------------------------------------------------------------------------
#Outputing projections by HUC4 for 2015 and 2070 for RPA meeting map
# write.csv(aq.ssp1[,c(1:3,58)],file="aq_ssp1.csv")
# write.csv(aq.ssp2[,c(1:3,58)],file="aq_ssp2.csv")
# write.csv(aq.ssp3[,c(1:3,58)],file="aq_ssp3.csv")
# write.csv(aq.ssp4[,c(1:3,58)],file="aq_ssp4.csv")
# write.csv(aq.ssp5[,c(1:3,58)],file="aq_ssp5.csv")
# 
# write.csv(dp.ssp1[,c(1:3,58)],file="dp_ssp1.csv")
# write.csv(dp.ssp2[,c(1:3,58)],file="dp_ssp2.csv")
# write.csv(dp.ssp3[,c(1:3,58)],file="dp_ssp3.csv")
# write.csv(dp.ssp4[,c(1:3,58)],file="dp_ssp4.csv")
# write.csv(dp.ssp5[,c(1:3,58)],file="dp_ssp5.csv")
# 
# write.csv(ic.ssp1[,c(1:3,58)],file="ic_ssp1.csv")
# write.csv(ic.ssp2[,c(1:3,58)],file="ic_ssp2.csv")
# write.csv(ic.ssp3[,c(1:3,58)],file="ic_ssp3.csv")
# write.csv(ic.ssp4[,c(1:3,58)],file="ic_ssp4.csv")
# write.csv(ic.ssp5[,c(1:3,58)],file="ic_ssp5.csv")
# 
# write.csv(ls.ssp1[,c(1:3,58)],file="ls_ssp1.csv")
# write.csv(ls.ssp2[,c(1:3,58)],file="ls_ssp2.csv")
# write.csv(ls.ssp3[,c(1:3,58)],file="ls_ssp3.csv")
# write.csv(ls.ssp4[,c(1:3,58)],file="ls_ssp4.csv")
# write.csv(ls.ssp5[,c(1:3,58)],file="ls_ssp5.csv")
# 
# write.csv(th.ssp1[,c(1:3,58)],file="th_ssp1.csv")
# write.csv(th.ssp2[,c(1:3,58)],file="th_ssp2.csv")
# write.csv(th.ssp3[,c(1:3,58)],file="th_ssp3.csv")
# write.csv(th.ssp4[,c(1:3,58)],file="th_ssp4.csv")
# write.csv(th.ssp5[,c(1:3,58)],file="th_ssp5.csv")
# 
# write.csv(ir[,c(1:3,58)],file="ir_ssp.csv")


#=====================================================================================================

# WEAP INPUT -- takes county (?) results and turns into WEAP-friendly input files

#----------------------------------------------------------------------------------------------------------------------
#WEAP input needs to be converted to thousands of cubic meters.
#convert from mgal to 1000 cubic meters
#1 Mgal/day = 1381.6345 (thousand) m^3
#----------------------------------------------------------------------------------------------------------------------

unitconv <- function(DF)
{
  for (n in 3:58){
    DF[,n]<- DF[,n]*(1381.6345)
  }
  
  return(DF)
}

aq.ssp1 <- unitconv(aq.ssp1)
aq.ssp2 <- unitconv(aq.ssp2)
aq.ssp3 <- unitconv(aq.ssp3)
aq.ssp4 <- unitconv(aq.ssp4)
aq.ssp5 <- unitconv(aq.ssp5)

dp.ssp1 <- unitconv(dp.ssp1)
dp.ssp2 <- unitconv(dp.ssp2)
dp.ssp3 <- unitconv(dp.ssp3)
dp.ssp4 <- unitconv(dp.ssp4)
dp.ssp5 <- unitconv(dp.ssp5)

ic.ssp1 <- unitconv(ic.ssp1)
ic.ssp2 <- unitconv(ic.ssp2)
ic.ssp3 <- unitconv(ic.ssp3)
ic.ssp4 <- unitconv(ic.ssp4)
ic.ssp5 <- unitconv(ic.ssp5)

ls.ssp1 <- unitconv(ls.ssp1)
ls.ssp2 <- unitconv(ls.ssp2)
ls.ssp3 <- unitconv(ls.ssp3)
ls.ssp4 <- unitconv(ls.ssp4)
ls.ssp5 <- unitconv(ls.ssp5)

th.ssp1 <- unitconv(th.ssp1)
th.ssp2 <- unitconv(th.ssp2)
th.ssp3 <- unitconv(th.ssp3)
th.ssp4 <- unitconv(th.ssp4)
th.ssp5 <- unitconv(th.ssp5)

ir <- unitconv(ir)

#----------------------------------------------------------------------------------------------------------------------
# Takes monthly proportions for each sector from Tom's previous work 
# and converts everything from annual to monthly data.
#----------------------------------------------------------------------------------------------------------------------

month <- read.csv("monthly.csv")

irmonthly <- function(df, time)
{
  df <- merge(df,time, by="HUC4")
  df <-  df[,c(1:58,95:106)]
  for (j in 0:55){
  for (n in 59:70){
    k=n + 12 + (j*12)
    m=j+3  
  df[,k] <- df[,m]*df[,n]
  }}
  df <- df[,c(1:2,71:742)]
  colnames(df) <- c("HUC4","sector","1/1/2015","2/1/2015","3/1/2015","4/1/2015","5/1/2015","6/1/2015","7/1/2015","8/1/2015","9/1/2015","10/1/2015","11/1/2015","12/1/2015",
                    "1/1/2016","2/1/2016","3/1/2016","4/1/2016","5/1/2016","6/1/2016","7/1/2016","8/1/2016","9/1/2016","10/1/2016","11/1/2016","12/1/2016",
                    "1/1/2017","2/1/2017","3/1/2017","4/1/2017","5/1/2017","6/1/2017","7/1/2017","8/1/2017","9/1/2017","10/1/2017","11/1/2017","12/1/2017",
                    "1/1/2018","2/1/2018","3/1/2018","4/1/2018","5/1/2018","6/1/2018","7/1/2018","8/1/2018","9/1/2018","10/1/2018","11/1/2018","12/1/2018",
                    "1/1/2019","2/1/2019","3/1/2019","4/1/2019","5/1/2019","6/1/2019","7/1/2019","8/1/2019","9/1/2019","10/1/2019","11/1/2019","12/1/2019",
                    "1/1/2020","2/1/2020","3/1/2020","4/1/2020","5/1/2020","6/1/2020","7/1/2020","8/1/2020","9/1/2020","10/1/2020","11/1/2020","12/1/2020",
                    "1/1/2021","2/1/2021","3/1/2021","4/1/2021","5/1/2021","6/1/2021","7/1/2021","8/1/2021","9/1/2021","10/1/2021","11/1/2021","12/1/2021",
                    "1/1/2022","2/1/2022","3/1/2022","4/1/2022","5/1/2022","6/1/2022","7/1/2022","8/1/2022","9/1/2022","10/1/2022","11/1/2022","12/1/2022",
                    "1/1/2023","2/1/2023","3/1/2023","4/1/2023","5/1/2023","6/1/2023","7/1/2023","8/1/2023","9/1/2023","10/1/2023","11/1/2023","12/1/2023",
                    "1/1/2024","2/1/2024","3/1/2024","4/1/2024","5/1/2024","6/1/2024","7/1/2024","8/1/2024","9/1/2024","10/1/2024","11/1/2024","12/1/2024",
                    "1/1/2025","2/1/2025","3/1/2025","4/1/2025","5/1/2025","6/1/2025","7/1/2025","8/1/2025","9/1/2025","10/1/2025","11/1/2025","12/1/2025",
                    "1/1/2026","2/1/2026","3/1/2026","4/1/2026","5/1/2026","6/1/2026","7/1/2026","8/1/2026","9/1/2026","10/1/2026","11/1/2026","12/1/2026",
                    "1/1/2027","2/1/2027","3/1/2027","4/1/2027","5/1/2027","6/1/2027","7/1/2027","8/1/2027","9/1/2027","10/1/2027","11/1/2027","12/1/2027",
                    "1/1/2028","2/1/2028","3/1/2028","4/1/2028","5/1/2028","6/1/2028","7/1/2028","8/1/2028","9/1/2028","10/1/2028","11/1/2028","12/1/2028",
                    "1/1/2029","2/1/2029","3/1/2029","4/1/2029","5/1/2029","6/1/2029","7/1/2029","8/1/2029","9/1/2029","10/1/2029","11/1/2029","12/1/2029",
                    "1/1/2030","2/1/2030","3/1/2030","4/1/2030","5/1/2030","6/1/2030","7/1/2030","8/1/2030","9/1/2030","10/1/2030","11/1/2030","12/1/2030",
                    "1/1/2031","2/1/2031","3/1/2031","4/1/2031","5/1/2031","6/1/2031","7/1/2031","8/1/2031","9/1/2031","10/1/2031","11/1/2031","12/1/2031",
                    "1/1/2032","2/1/2032","3/1/2032","4/1/2032","5/1/2032","6/1/2032","7/1/2032","8/1/2032","9/1/2032","10/1/2032","11/1/2032","12/1/2032",
                    "1/1/2033","2/1/2033","3/1/2033","4/1/2033","5/1/2033","6/1/2033","7/1/2033","8/1/2033","9/1/2033","10/1/2033","11/1/2033","12/1/2033",
                    "1/1/2034","2/1/2034","3/1/2034","4/1/2034","5/1/2034","6/1/2034","7/1/2034","8/1/2034","9/1/2034","10/1/2034","11/1/2034","12/1/2034",
                    "1/1/2035","2/1/2035","3/1/2035","4/1/2035","5/1/2035","6/1/2035","7/1/2035","8/1/2035","9/1/2035","10/1/2035","11/1/2035","12/1/2035",
                    "1/1/2036","2/1/2036","3/1/2036","4/1/2036","5/1/2036","6/1/2036","7/1/2036","8/1/2036","9/1/2036","10/1/2036","11/1/2036","12/1/2036",
                    "1/1/2037","2/1/2037","3/1/2037","4/1/2037","5/1/2037","6/1/2037","7/1/2037","8/1/2037","9/1/2037","10/1/2037","11/1/2037","12/1/2037",
                    "1/1/2038","2/1/2038","3/1/2038","4/1/2038","5/1/2038","6/1/2038","7/1/2038","8/1/2038","9/1/2038","10/1/2038","11/1/2038","12/1/2038",
                    "1/1/2039","2/1/2039","3/1/2039","4/1/2039","5/1/2039","6/1/2039","7/1/2039","8/1/2039","9/1/2039","10/1/2039","11/1/2039","12/1/2039",
                    "1/1/2040","2/1/2040","3/1/2040","4/1/2040","5/1/2040","6/1/2040","7/1/2040","8/1/2040","9/1/2040","10/1/2040","11/1/2040","12/1/2040",
                    "1/1/2041","2/1/2041","3/1/2041","4/1/2041","5/1/2041","6/1/2041","7/1/2041","8/1/2041","9/1/2041","10/1/2041","11/1/2041","12/1/2041",
                    "1/1/2042","2/1/2042","3/1/2042","4/1/2042","5/1/2042","6/1/2042","7/1/2042","8/1/2042","9/1/2042","10/1/2042","11/1/2042","12/1/2042",
                    "1/1/2043","2/1/2043","3/1/2043","4/1/2043","5/1/2043","6/1/2043","7/1/2043","8/1/2043","9/1/2043","10/1/2043","11/1/2043","12/1/2043",
                    "1/1/2044","2/1/2044","3/1/2044","4/1/2044","5/1/2044","6/1/2044","7/1/2044","8/1/2044","9/1/2044","10/1/2044","11/1/2044","12/1/2044",
                    "1/1/2045","2/1/2045","3/1/2045","4/1/2045","5/1/2045","6/1/2045","7/1/2045","8/1/2045","9/1/2045","10/1/2045","11/1/2045","12/1/2045",
                    "1/1/2046","2/1/2046","3/1/2046","4/1/2046","5/1/2046","6/1/2046","7/1/2046","8/1/2046","9/1/2046","10/1/2046","11/1/2046","12/1/2046",
                    "1/1/2047","2/1/2047","3/1/2047","4/1/2047","5/1/2047","6/1/2047","7/1/2047","8/1/2047","9/1/2047","10/1/2047","11/1/2047","12/1/2047",
                    "1/1/2048","2/1/2048","3/1/2048","4/1/2048","5/1/2048","6/1/2048","7/1/2048","8/1/2048","9/1/2048","10/1/2048","11/1/2048","12/1/2048",
                    "1/1/2049","2/1/2049","3/1/2049","4/1/2049","5/1/2049","6/1/2049","7/1/2049","8/1/2049","9/1/2049","10/1/2049","11/1/2049","12/1/2049",
                    "1/1/2050","2/1/2050","3/1/2050","4/1/2050","5/1/2050","6/1/2050","7/1/2050","8/1/2050","9/1/2050","10/1/2050","11/1/2050","12/1/2050",
                    "1/1/2051","2/1/2051","3/1/2051","4/1/2051","5/1/2051","6/1/2051","7/1/2051","8/1/2051","9/1/2051","10/1/2051","11/1/2051","12/1/2051",
                    "1/1/2052","2/1/2052","3/1/2052","4/1/2052","5/1/2052","6/1/2052","7/1/2052","8/1/2052","9/1/2052","10/1/2052","11/1/2052","12/1/2052",
                    "1/1/2053","2/1/2053","3/1/2053","4/1/2053","5/1/2053","6/1/2053","7/1/2053","8/1/2053","9/1/2053","10/1/2053","11/1/2053","12/1/2053",
                    "1/1/2054","2/1/2054","3/1/2054","4/1/2054","5/1/2054","6/1/2054","7/1/2054","8/1/2054","9/1/2054","10/1/2054","11/1/2054","12/1/2054",
                    "1/1/2055","2/1/2055","3/1/2055","4/1/2055","5/1/2055","6/1/2055","7/1/2055","8/1/2055","9/1/2055","10/1/2055","11/1/2055","12/1/2055",
                    "1/1/2056","2/1/2056","3/1/2056","4/1/2056","5/1/2056","6/1/2056","7/1/2056","8/1/2056","9/1/2056","10/1/2056","11/1/2056","12/1/2056",
                    "1/1/2057","2/1/2057","3/1/2057","4/1/2057","5/1/2057","6/1/2057","7/1/2057","8/1/2057","9/1/2057","10/1/2057","11/1/2057","12/1/2057",
                    "1/1/2058","2/1/2058","3/1/2058","4/1/2058","5/1/2058","6/1/2058","7/1/2058","8/1/2058","9/1/2058","10/1/2058","11/1/2058","12/1/2058",
                    "1/1/2059","2/1/2059","3/1/2059","4/1/2059","5/1/2059","6/1/2059","7/1/2059","8/1/2059","9/1/2059","10/1/2059","11/1/2059","12/1/2059",
                    "1/1/2060","2/1/2060","3/1/2060","4/1/2060","5/1/2060","6/1/2060","7/1/2060","8/1/2060","9/1/2060","10/1/2060","11/1/2060","12/1/2060",
                    "1/1/2061","2/1/2061","3/1/2061","4/1/2061","5/1/2061","6/1/2061","7/1/2061","8/1/2061","9/1/2061","10/1/2061","11/1/2061","12/1/2061",
                    "1/1/2062","2/1/2062","3/1/2062","4/1/2062","5/1/2062","6/1/2062","7/1/2062","8/1/2062","9/1/2062","10/1/2062","11/1/2062","12/1/2062",
                    "1/1/2063","2/1/2063","3/1/2063","4/1/2063","5/1/2063","6/1/2063","7/1/2063","8/1/2063","9/1/2063","10/1/2063","11/1/2063","12/1/2063",
                    "1/1/2064","2/1/2064","3/1/2064","4/1/2064","5/1/2064","6/1/2064","7/1/2064","8/1/2064","9/1/2064","10/1/2064","11/1/2064","12/1/2064",
                    "1/1/2065","2/1/2065","3/1/2065","4/1/2065","5/1/2065","6/1/2065","7/1/2065","8/1/2065","9/1/2065","10/1/2065","11/1/2065","12/1/2065",
                    "1/1/2066","2/1/2066","3/1/2066","4/1/2066","5/1/2066","6/1/2066","7/1/2066","8/1/2066","9/1/2066","10/1/2066","11/1/2066","12/1/2066",
                    "1/1/2067","2/1/2067","3/1/2067","4/1/2067","5/1/2067","6/1/2067","7/1/2067","8/1/2067","9/1/2067","10/1/2067","11/1/2067","12/1/2067",
                    "1/1/2068","2/1/2068","3/1/2068","4/1/2068","5/1/2068","6/1/2068","7/1/2068","8/1/2068","9/1/2068","10/1/2068","11/1/2068","12/1/2068",
                    "1/1/2069","2/1/2069","3/1/2069","4/1/2069","5/1/2069","6/1/2069","7/1/2069","8/1/2069","9/1/2069","10/1/2069","11/1/2069","12/1/2069",
                    "1/1/2070","2/1/2070","3/1/2070","4/1/2070","5/1/2070","6/1/2070","7/1/2070","8/1/2070","9/1/2070","10/1/2070","11/1/2070","12/1/2070"
                    )
  return(df)
}

dpmonthly <- function(df, time)
{
  df <- merge(df,time, by="HUC4")
  df <-  df[,c(1:70)]
  for (j in 0:55){
    for (n in 59:70){
      k=n + 12 + (j*12)
      m=j+3  
      df[,k] <- df[,m]*df[,n]
    }}
  df <- df[,c(1:2,71:742)]
  colnames(df) <- c("HUC4","sector","1/1/2015","2/1/2015","3/1/2015","4/1/2015","5/1/2015","6/1/2015","7/1/2015","8/1/2015","9/1/2015","10/1/2015","11/1/2015","12/1/2015",
                    "1/1/2016","2/1/2016","3/1/2016","4/1/2016","5/1/2016","6/1/2016","7/1/2016","8/1/2016","9/1/2016","10/1/2016","11/1/2016","12/1/2016",
                    "1/1/2017","2/1/2017","3/1/2017","4/1/2017","5/1/2017","6/1/2017","7/1/2017","8/1/2017","9/1/2017","10/1/2017","11/1/2017","12/1/2017",
                    "1/1/2018","2/1/2018","3/1/2018","4/1/2018","5/1/2018","6/1/2018","7/1/2018","8/1/2018","9/1/2018","10/1/2018","11/1/2018","12/1/2018",
                    "1/1/2019","2/1/2019","3/1/2019","4/1/2019","5/1/2019","6/1/2019","7/1/2019","8/1/2019","9/1/2019","10/1/2019","11/1/2019","12/1/2019",
                    "1/1/2020","2/1/2020","3/1/2020","4/1/2020","5/1/2020","6/1/2020","7/1/2020","8/1/2020","9/1/2020","10/1/2020","11/1/2020","12/1/2020",
                    "1/1/2021","2/1/2021","3/1/2021","4/1/2021","5/1/2021","6/1/2021","7/1/2021","8/1/2021","9/1/2021","10/1/2021","11/1/2021","12/1/2021",
                    "1/1/2022","2/1/2022","3/1/2022","4/1/2022","5/1/2022","6/1/2022","7/1/2022","8/1/2022","9/1/2022","10/1/2022","11/1/2022","12/1/2022",
                    "1/1/2023","2/1/2023","3/1/2023","4/1/2023","5/1/2023","6/1/2023","7/1/2023","8/1/2023","9/1/2023","10/1/2023","11/1/2023","12/1/2023",
                    "1/1/2024","2/1/2024","3/1/2024","4/1/2024","5/1/2024","6/1/2024","7/1/2024","8/1/2024","9/1/2024","10/1/2024","11/1/2024","12/1/2024",
                    "1/1/2025","2/1/2025","3/1/2025","4/1/2025","5/1/2025","6/1/2025","7/1/2025","8/1/2025","9/1/2025","10/1/2025","11/1/2025","12/1/2025",
                    "1/1/2026","2/1/2026","3/1/2026","4/1/2026","5/1/2026","6/1/2026","7/1/2026","8/1/2026","9/1/2026","10/1/2026","11/1/2026","12/1/2026",
                    "1/1/2027","2/1/2027","3/1/2027","4/1/2027","5/1/2027","6/1/2027","7/1/2027","8/1/2027","9/1/2027","10/1/2027","11/1/2027","12/1/2027",
                    "1/1/2028","2/1/2028","3/1/2028","4/1/2028","5/1/2028","6/1/2028","7/1/2028","8/1/2028","9/1/2028","10/1/2028","11/1/2028","12/1/2028",
                    "1/1/2029","2/1/2029","3/1/2029","4/1/2029","5/1/2029","6/1/2029","7/1/2029","8/1/2029","9/1/2029","10/1/2029","11/1/2029","12/1/2029",
                    "1/1/2030","2/1/2030","3/1/2030","4/1/2030","5/1/2030","6/1/2030","7/1/2030","8/1/2030","9/1/2030","10/1/2030","11/1/2030","12/1/2030",
                    "1/1/2031","2/1/2031","3/1/2031","4/1/2031","5/1/2031","6/1/2031","7/1/2031","8/1/2031","9/1/2031","10/1/2031","11/1/2031","12/1/2031",
                    "1/1/2032","2/1/2032","3/1/2032","4/1/2032","5/1/2032","6/1/2032","7/1/2032","8/1/2032","9/1/2032","10/1/2032","11/1/2032","12/1/2032",
                    "1/1/2033","2/1/2033","3/1/2033","4/1/2033","5/1/2033","6/1/2033","7/1/2033","8/1/2033","9/1/2033","10/1/2033","11/1/2033","12/1/2033",
                    "1/1/2034","2/1/2034","3/1/2034","4/1/2034","5/1/2034","6/1/2034","7/1/2034","8/1/2034","9/1/2034","10/1/2034","11/1/2034","12/1/2034",
                    "1/1/2035","2/1/2035","3/1/2035","4/1/2035","5/1/2035","6/1/2035","7/1/2035","8/1/2035","9/1/2035","10/1/2035","11/1/2035","12/1/2035",
                    "1/1/2036","2/1/2036","3/1/2036","4/1/2036","5/1/2036","6/1/2036","7/1/2036","8/1/2036","9/1/2036","10/1/2036","11/1/2036","12/1/2036",
                    "1/1/2037","2/1/2037","3/1/2037","4/1/2037","5/1/2037","6/1/2037","7/1/2037","8/1/2037","9/1/2037","10/1/2037","11/1/2037","12/1/2037",
                    "1/1/2038","2/1/2038","3/1/2038","4/1/2038","5/1/2038","6/1/2038","7/1/2038","8/1/2038","9/1/2038","10/1/2038","11/1/2038","12/1/2038",
                    "1/1/2039","2/1/2039","3/1/2039","4/1/2039","5/1/2039","6/1/2039","7/1/2039","8/1/2039","9/1/2039","10/1/2039","11/1/2039","12/1/2039",
                    "1/1/2040","2/1/2040","3/1/2040","4/1/2040","5/1/2040","6/1/2040","7/1/2040","8/1/2040","9/1/2040","10/1/2040","11/1/2040","12/1/2040",
                    "1/1/2041","2/1/2041","3/1/2041","4/1/2041","5/1/2041","6/1/2041","7/1/2041","8/1/2041","9/1/2041","10/1/2041","11/1/2041","12/1/2041",
                    "1/1/2042","2/1/2042","3/1/2042","4/1/2042","5/1/2042","6/1/2042","7/1/2042","8/1/2042","9/1/2042","10/1/2042","11/1/2042","12/1/2042",
                    "1/1/2043","2/1/2043","3/1/2043","4/1/2043","5/1/2043","6/1/2043","7/1/2043","8/1/2043","9/1/2043","10/1/2043","11/1/2043","12/1/2043",
                    "1/1/2044","2/1/2044","3/1/2044","4/1/2044","5/1/2044","6/1/2044","7/1/2044","8/1/2044","9/1/2044","10/1/2044","11/1/2044","12/1/2044",
                    "1/1/2045","2/1/2045","3/1/2045","4/1/2045","5/1/2045","6/1/2045","7/1/2045","8/1/2045","9/1/2045","10/1/2045","11/1/2045","12/1/2045",
                    "1/1/2046","2/1/2046","3/1/2046","4/1/2046","5/1/2046","6/1/2046","7/1/2046","8/1/2046","9/1/2046","10/1/2046","11/1/2046","12/1/2046",
                    "1/1/2047","2/1/2047","3/1/2047","4/1/2047","5/1/2047","6/1/2047","7/1/2047","8/1/2047","9/1/2047","10/1/2047","11/1/2047","12/1/2047",
                    "1/1/2048","2/1/2048","3/1/2048","4/1/2048","5/1/2048","6/1/2048","7/1/2048","8/1/2048","9/1/2048","10/1/2048","11/1/2048","12/1/2048",
                    "1/1/2049","2/1/2049","3/1/2049","4/1/2049","5/1/2049","6/1/2049","7/1/2049","8/1/2049","9/1/2049","10/1/2049","11/1/2049","12/1/2049",
                    "1/1/2050","2/1/2050","3/1/2050","4/1/2050","5/1/2050","6/1/2050","7/1/2050","8/1/2050","9/1/2050","10/1/2050","11/1/2050","12/1/2050",
                    "1/1/2051","2/1/2051","3/1/2051","4/1/2051","5/1/2051","6/1/2051","7/1/2051","8/1/2051","9/1/2051","10/1/2051","11/1/2051","12/1/2051",
                    "1/1/2052","2/1/2052","3/1/2052","4/1/2052","5/1/2052","6/1/2052","7/1/2052","8/1/2052","9/1/2052","10/1/2052","11/1/2052","12/1/2052",
                    "1/1/2053","2/1/2053","3/1/2053","4/1/2053","5/1/2053","6/1/2053","7/1/2053","8/1/2053","9/1/2053","10/1/2053","11/1/2053","12/1/2053",
                    "1/1/2054","2/1/2054","3/1/2054","4/1/2054","5/1/2054","6/1/2054","7/1/2054","8/1/2054","9/1/2054","10/1/2054","11/1/2054","12/1/2054",
                    "1/1/2055","2/1/2055","3/1/2055","4/1/2055","5/1/2055","6/1/2055","7/1/2055","8/1/2055","9/1/2055","10/1/2055","11/1/2055","12/1/2055",
                    "1/1/2056","2/1/2056","3/1/2056","4/1/2056","5/1/2056","6/1/2056","7/1/2056","8/1/2056","9/1/2056","10/1/2056","11/1/2056","12/1/2056",
                    "1/1/2057","2/1/2057","3/1/2057","4/1/2057","5/1/2057","6/1/2057","7/1/2057","8/1/2057","9/1/2057","10/1/2057","11/1/2057","12/1/2057",
                    "1/1/2058","2/1/2058","3/1/2058","4/1/2058","5/1/2058","6/1/2058","7/1/2058","8/1/2058","9/1/2058","10/1/2058","11/1/2058","12/1/2058",
                    "1/1/2059","2/1/2059","3/1/2059","4/1/2059","5/1/2059","6/1/2059","7/1/2059","8/1/2059","9/1/2059","10/1/2059","11/1/2059","12/1/2059",
                    "1/1/2060","2/1/2060","3/1/2060","4/1/2060","5/1/2060","6/1/2060","7/1/2060","8/1/2060","9/1/2060","10/1/2060","11/1/2060","12/1/2060",
                    "1/1/2061","2/1/2061","3/1/2061","4/1/2061","5/1/2061","6/1/2061","7/1/2061","8/1/2061","9/1/2061","10/1/2061","11/1/2061","12/1/2061",
                    "1/1/2062","2/1/2062","3/1/2062","4/1/2062","5/1/2062","6/1/2062","7/1/2062","8/1/2062","9/1/2062","10/1/2062","11/1/2062","12/1/2062",
                    "1/1/2063","2/1/2063","3/1/2063","4/1/2063","5/1/2063","6/1/2063","7/1/2063","8/1/2063","9/1/2063","10/1/2063","11/1/2063","12/1/2063",
                    "1/1/2064","2/1/2064","3/1/2064","4/1/2064","5/1/2064","6/1/2064","7/1/2064","8/1/2064","9/1/2064","10/1/2064","11/1/2064","12/1/2064",
                    "1/1/2065","2/1/2065","3/1/2065","4/1/2065","5/1/2065","6/1/2065","7/1/2065","8/1/2065","9/1/2065","10/1/2065","11/1/2065","12/1/2065",
                    "1/1/2066","2/1/2066","3/1/2066","4/1/2066","5/1/2066","6/1/2066","7/1/2066","8/1/2066","9/1/2066","10/1/2066","11/1/2066","12/1/2066",
                    "1/1/2067","2/1/2067","3/1/2067","4/1/2067","5/1/2067","6/1/2067","7/1/2067","8/1/2067","9/1/2067","10/1/2067","11/1/2067","12/1/2067",
                    "1/1/2068","2/1/2068","3/1/2068","4/1/2068","5/1/2068","6/1/2068","7/1/2068","8/1/2068","9/1/2068","10/1/2068","11/1/2068","12/1/2068",
                    "1/1/2069","2/1/2069","3/1/2069","4/1/2069","5/1/2069","6/1/2069","7/1/2069","8/1/2069","9/1/2069","10/1/2069","11/1/2069","12/1/2069",
                    "1/1/2070","2/1/2070","3/1/2070","4/1/2070","5/1/2070","6/1/2070","7/1/2070","8/1/2070","9/1/2070","10/1/2070","11/1/2070","12/1/2070"
  )
  return(df)
}

icmonthly <- function(df, time)
{
  df <- merge(df,time, by="HUC4")
  df <-  df[,c(1:58,71:82)]
  for (j in 0:55){
    for (n in 59:70){
      k=n + 12 + (j*12)
      m=j+3  
      df[,k] <- df[,m]*df[,n]
    }}
  df <- df[,c(1:2,71:742)]
  colnames(df) <- c("HUC4","sector","1/1/2015","2/1/2015","3/1/2015","4/1/2015","5/1/2015","6/1/2015","7/1/2015","8/1/2015","9/1/2015","10/1/2015","11/1/2015","12/1/2015",
                    "1/1/2016","2/1/2016","3/1/2016","4/1/2016","5/1/2016","6/1/2016","7/1/2016","8/1/2016","9/1/2016","10/1/2016","11/1/2016","12/1/2016",
                    "1/1/2017","2/1/2017","3/1/2017","4/1/2017","5/1/2017","6/1/2017","7/1/2017","8/1/2017","9/1/2017","10/1/2017","11/1/2017","12/1/2017",
                    "1/1/2018","2/1/2018","3/1/2018","4/1/2018","5/1/2018","6/1/2018","7/1/2018","8/1/2018","9/1/2018","10/1/2018","11/1/2018","12/1/2018",
                    "1/1/2019","2/1/2019","3/1/2019","4/1/2019","5/1/2019","6/1/2019","7/1/2019","8/1/2019","9/1/2019","10/1/2019","11/1/2019","12/1/2019",
                    "1/1/2020","2/1/2020","3/1/2020","4/1/2020","5/1/2020","6/1/2020","7/1/2020","8/1/2020","9/1/2020","10/1/2020","11/1/2020","12/1/2020",
                    "1/1/2021","2/1/2021","3/1/2021","4/1/2021","5/1/2021","6/1/2021","7/1/2021","8/1/2021","9/1/2021","10/1/2021","11/1/2021","12/1/2021",
                    "1/1/2022","2/1/2022","3/1/2022","4/1/2022","5/1/2022","6/1/2022","7/1/2022","8/1/2022","9/1/2022","10/1/2022","11/1/2022","12/1/2022",
                    "1/1/2023","2/1/2023","3/1/2023","4/1/2023","5/1/2023","6/1/2023","7/1/2023","8/1/2023","9/1/2023","10/1/2023","11/1/2023","12/1/2023",
                    "1/1/2024","2/1/2024","3/1/2024","4/1/2024","5/1/2024","6/1/2024","7/1/2024","8/1/2024","9/1/2024","10/1/2024","11/1/2024","12/1/2024",
                    "1/1/2025","2/1/2025","3/1/2025","4/1/2025","5/1/2025","6/1/2025","7/1/2025","8/1/2025","9/1/2025","10/1/2025","11/1/2025","12/1/2025",
                    "1/1/2026","2/1/2026","3/1/2026","4/1/2026","5/1/2026","6/1/2026","7/1/2026","8/1/2026","9/1/2026","10/1/2026","11/1/2026","12/1/2026",
                    "1/1/2027","2/1/2027","3/1/2027","4/1/2027","5/1/2027","6/1/2027","7/1/2027","8/1/2027","9/1/2027","10/1/2027","11/1/2027","12/1/2027",
                    "1/1/2028","2/1/2028","3/1/2028","4/1/2028","5/1/2028","6/1/2028","7/1/2028","8/1/2028","9/1/2028","10/1/2028","11/1/2028","12/1/2028",
                    "1/1/2029","2/1/2029","3/1/2029","4/1/2029","5/1/2029","6/1/2029","7/1/2029","8/1/2029","9/1/2029","10/1/2029","11/1/2029","12/1/2029",
                    "1/1/2030","2/1/2030","3/1/2030","4/1/2030","5/1/2030","6/1/2030","7/1/2030","8/1/2030","9/1/2030","10/1/2030","11/1/2030","12/1/2030",
                    "1/1/2031","2/1/2031","3/1/2031","4/1/2031","5/1/2031","6/1/2031","7/1/2031","8/1/2031","9/1/2031","10/1/2031","11/1/2031","12/1/2031",
                    "1/1/2032","2/1/2032","3/1/2032","4/1/2032","5/1/2032","6/1/2032","7/1/2032","8/1/2032","9/1/2032","10/1/2032","11/1/2032","12/1/2032",
                    "1/1/2033","2/1/2033","3/1/2033","4/1/2033","5/1/2033","6/1/2033","7/1/2033","8/1/2033","9/1/2033","10/1/2033","11/1/2033","12/1/2033",
                    "1/1/2034","2/1/2034","3/1/2034","4/1/2034","5/1/2034","6/1/2034","7/1/2034","8/1/2034","9/1/2034","10/1/2034","11/1/2034","12/1/2034",
                    "1/1/2035","2/1/2035","3/1/2035","4/1/2035","5/1/2035","6/1/2035","7/1/2035","8/1/2035","9/1/2035","10/1/2035","11/1/2035","12/1/2035",
                    "1/1/2036","2/1/2036","3/1/2036","4/1/2036","5/1/2036","6/1/2036","7/1/2036","8/1/2036","9/1/2036","10/1/2036","11/1/2036","12/1/2036",
                    "1/1/2037","2/1/2037","3/1/2037","4/1/2037","5/1/2037","6/1/2037","7/1/2037","8/1/2037","9/1/2037","10/1/2037","11/1/2037","12/1/2037",
                    "1/1/2038","2/1/2038","3/1/2038","4/1/2038","5/1/2038","6/1/2038","7/1/2038","8/1/2038","9/1/2038","10/1/2038","11/1/2038","12/1/2038",
                    "1/1/2039","2/1/2039","3/1/2039","4/1/2039","5/1/2039","6/1/2039","7/1/2039","8/1/2039","9/1/2039","10/1/2039","11/1/2039","12/1/2039",
                    "1/1/2040","2/1/2040","3/1/2040","4/1/2040","5/1/2040","6/1/2040","7/1/2040","8/1/2040","9/1/2040","10/1/2040","11/1/2040","12/1/2040",
                    "1/1/2041","2/1/2041","3/1/2041","4/1/2041","5/1/2041","6/1/2041","7/1/2041","8/1/2041","9/1/2041","10/1/2041","11/1/2041","12/1/2041",
                    "1/1/2042","2/1/2042","3/1/2042","4/1/2042","5/1/2042","6/1/2042","7/1/2042","8/1/2042","9/1/2042","10/1/2042","11/1/2042","12/1/2042",
                    "1/1/2043","2/1/2043","3/1/2043","4/1/2043","5/1/2043","6/1/2043","7/1/2043","8/1/2043","9/1/2043","10/1/2043","11/1/2043","12/1/2043",
                    "1/1/2044","2/1/2044","3/1/2044","4/1/2044","5/1/2044","6/1/2044","7/1/2044","8/1/2044","9/1/2044","10/1/2044","11/1/2044","12/1/2044",
                    "1/1/2045","2/1/2045","3/1/2045","4/1/2045","5/1/2045","6/1/2045","7/1/2045","8/1/2045","9/1/2045","10/1/2045","11/1/2045","12/1/2045",
                    "1/1/2046","2/1/2046","3/1/2046","4/1/2046","5/1/2046","6/1/2046","7/1/2046","8/1/2046","9/1/2046","10/1/2046","11/1/2046","12/1/2046",
                    "1/1/2047","2/1/2047","3/1/2047","4/1/2047","5/1/2047","6/1/2047","7/1/2047","8/1/2047","9/1/2047","10/1/2047","11/1/2047","12/1/2047",
                    "1/1/2048","2/1/2048","3/1/2048","4/1/2048","5/1/2048","6/1/2048","7/1/2048","8/1/2048","9/1/2048","10/1/2048","11/1/2048","12/1/2048",
                    "1/1/2049","2/1/2049","3/1/2049","4/1/2049","5/1/2049","6/1/2049","7/1/2049","8/1/2049","9/1/2049","10/1/2049","11/1/2049","12/1/2049",
                    "1/1/2050","2/1/2050","3/1/2050","4/1/2050","5/1/2050","6/1/2050","7/1/2050","8/1/2050","9/1/2050","10/1/2050","11/1/2050","12/1/2050",
                    "1/1/2051","2/1/2051","3/1/2051","4/1/2051","5/1/2051","6/1/2051","7/1/2051","8/1/2051","9/1/2051","10/1/2051","11/1/2051","12/1/2051",
                    "1/1/2052","2/1/2052","3/1/2052","4/1/2052","5/1/2052","6/1/2052","7/1/2052","8/1/2052","9/1/2052","10/1/2052","11/1/2052","12/1/2052",
                    "1/1/2053","2/1/2053","3/1/2053","4/1/2053","5/1/2053","6/1/2053","7/1/2053","8/1/2053","9/1/2053","10/1/2053","11/1/2053","12/1/2053",
                    "1/1/2054","2/1/2054","3/1/2054","4/1/2054","5/1/2054","6/1/2054","7/1/2054","8/1/2054","9/1/2054","10/1/2054","11/1/2054","12/1/2054",
                    "1/1/2055","2/1/2055","3/1/2055","4/1/2055","5/1/2055","6/1/2055","7/1/2055","8/1/2055","9/1/2055","10/1/2055","11/1/2055","12/1/2055",
                    "1/1/2056","2/1/2056","3/1/2056","4/1/2056","5/1/2056","6/1/2056","7/1/2056","8/1/2056","9/1/2056","10/1/2056","11/1/2056","12/1/2056",
                    "1/1/2057","2/1/2057","3/1/2057","4/1/2057","5/1/2057","6/1/2057","7/1/2057","8/1/2057","9/1/2057","10/1/2057","11/1/2057","12/1/2057",
                    "1/1/2058","2/1/2058","3/1/2058","4/1/2058","5/1/2058","6/1/2058","7/1/2058","8/1/2058","9/1/2058","10/1/2058","11/1/2058","12/1/2058",
                    "1/1/2059","2/1/2059","3/1/2059","4/1/2059","5/1/2059","6/1/2059","7/1/2059","8/1/2059","9/1/2059","10/1/2059","11/1/2059","12/1/2059",
                    "1/1/2060","2/1/2060","3/1/2060","4/1/2060","5/1/2060","6/1/2060","7/1/2060","8/1/2060","9/1/2060","10/1/2060","11/1/2060","12/1/2060",
                    "1/1/2061","2/1/2061","3/1/2061","4/1/2061","5/1/2061","6/1/2061","7/1/2061","8/1/2061","9/1/2061","10/1/2061","11/1/2061","12/1/2061",
                    "1/1/2062","2/1/2062","3/1/2062","4/1/2062","5/1/2062","6/1/2062","7/1/2062","8/1/2062","9/1/2062","10/1/2062","11/1/2062","12/1/2062",
                    "1/1/2063","2/1/2063","3/1/2063","4/1/2063","5/1/2063","6/1/2063","7/1/2063","8/1/2063","9/1/2063","10/1/2063","11/1/2063","12/1/2063",
                    "1/1/2064","2/1/2064","3/1/2064","4/1/2064","5/1/2064","6/1/2064","7/1/2064","8/1/2064","9/1/2064","10/1/2064","11/1/2064","12/1/2064",
                    "1/1/2065","2/1/2065","3/1/2065","4/1/2065","5/1/2065","6/1/2065","7/1/2065","8/1/2065","9/1/2065","10/1/2065","11/1/2065","12/1/2065",
                    "1/1/2066","2/1/2066","3/1/2066","4/1/2066","5/1/2066","6/1/2066","7/1/2066","8/1/2066","9/1/2066","10/1/2066","11/1/2066","12/1/2066",
                    "1/1/2067","2/1/2067","3/1/2067","4/1/2067","5/1/2067","6/1/2067","7/1/2067","8/1/2067","9/1/2067","10/1/2067","11/1/2067","12/1/2067",
                    "1/1/2068","2/1/2068","3/1/2068","4/1/2068","5/1/2068","6/1/2068","7/1/2068","8/1/2068","9/1/2068","10/1/2068","11/1/2068","12/1/2068",
                    "1/1/2069","2/1/2069","3/1/2069","4/1/2069","5/1/2069","6/1/2069","7/1/2069","8/1/2069","9/1/2069","10/1/2069","11/1/2069","12/1/2069",
                    "1/1/2070","2/1/2070","3/1/2070","4/1/2070","5/1/2070","6/1/2070","7/1/2070","8/1/2070","9/1/2070","10/1/2070","11/1/2070","12/1/2070"
  )
  return(df)
}

thmonthly <- function(df, time)
{
  df <- merge(df,time, by="HUC4")
  df <-  df[,c(1:58,83:94)]
  for (j in 0:55){
    for (n in 59:70){
      k=n + 12 + (j*12)
      m=j+3  
      df[,k] <- df[,m]*df[,n]
    }}
  df <- df[,c(1:2,71:742)]
  colnames(df) <- c("HUC4","sector","1/1/2015","2/1/2015","3/1/2015","4/1/2015","5/1/2015","6/1/2015","7/1/2015","8/1/2015","9/1/2015","10/1/2015","11/1/2015","12/1/2015",
                    "1/1/2016","2/1/2016","3/1/2016","4/1/2016","5/1/2016","6/1/2016","7/1/2016","8/1/2016","9/1/2016","10/1/2016","11/1/2016","12/1/2016",
                    "1/1/2017","2/1/2017","3/1/2017","4/1/2017","5/1/2017","6/1/2017","7/1/2017","8/1/2017","9/1/2017","10/1/2017","11/1/2017","12/1/2017",
                    "1/1/2018","2/1/2018","3/1/2018","4/1/2018","5/1/2018","6/1/2018","7/1/2018","8/1/2018","9/1/2018","10/1/2018","11/1/2018","12/1/2018",
                    "1/1/2019","2/1/2019","3/1/2019","4/1/2019","5/1/2019","6/1/2019","7/1/2019","8/1/2019","9/1/2019","10/1/2019","11/1/2019","12/1/2019",
                    "1/1/2020","2/1/2020","3/1/2020","4/1/2020","5/1/2020","6/1/2020","7/1/2020","8/1/2020","9/1/2020","10/1/2020","11/1/2020","12/1/2020",
                    "1/1/2021","2/1/2021","3/1/2021","4/1/2021","5/1/2021","6/1/2021","7/1/2021","8/1/2021","9/1/2021","10/1/2021","11/1/2021","12/1/2021",
                    "1/1/2022","2/1/2022","3/1/2022","4/1/2022","5/1/2022","6/1/2022","7/1/2022","8/1/2022","9/1/2022","10/1/2022","11/1/2022","12/1/2022",
                    "1/1/2023","2/1/2023","3/1/2023","4/1/2023","5/1/2023","6/1/2023","7/1/2023","8/1/2023","9/1/2023","10/1/2023","11/1/2023","12/1/2023",
                    "1/1/2024","2/1/2024","3/1/2024","4/1/2024","5/1/2024","6/1/2024","7/1/2024","8/1/2024","9/1/2024","10/1/2024","11/1/2024","12/1/2024",
                    "1/1/2025","2/1/2025","3/1/2025","4/1/2025","5/1/2025","6/1/2025","7/1/2025","8/1/2025","9/1/2025","10/1/2025","11/1/2025","12/1/2025",
                    "1/1/2026","2/1/2026","3/1/2026","4/1/2026","5/1/2026","6/1/2026","7/1/2026","8/1/2026","9/1/2026","10/1/2026","11/1/2026","12/1/2026",
                    "1/1/2027","2/1/2027","3/1/2027","4/1/2027","5/1/2027","6/1/2027","7/1/2027","8/1/2027","9/1/2027","10/1/2027","11/1/2027","12/1/2027",
                    "1/1/2028","2/1/2028","3/1/2028","4/1/2028","5/1/2028","6/1/2028","7/1/2028","8/1/2028","9/1/2028","10/1/2028","11/1/2028","12/1/2028",
                    "1/1/2029","2/1/2029","3/1/2029","4/1/2029","5/1/2029","6/1/2029","7/1/2029","8/1/2029","9/1/2029","10/1/2029","11/1/2029","12/1/2029",
                    "1/1/2030","2/1/2030","3/1/2030","4/1/2030","5/1/2030","6/1/2030","7/1/2030","8/1/2030","9/1/2030","10/1/2030","11/1/2030","12/1/2030",
                    "1/1/2031","2/1/2031","3/1/2031","4/1/2031","5/1/2031","6/1/2031","7/1/2031","8/1/2031","9/1/2031","10/1/2031","11/1/2031","12/1/2031",
                    "1/1/2032","2/1/2032","3/1/2032","4/1/2032","5/1/2032","6/1/2032","7/1/2032","8/1/2032","9/1/2032","10/1/2032","11/1/2032","12/1/2032",
                    "1/1/2033","2/1/2033","3/1/2033","4/1/2033","5/1/2033","6/1/2033","7/1/2033","8/1/2033","9/1/2033","10/1/2033","11/1/2033","12/1/2033",
                    "1/1/2034","2/1/2034","3/1/2034","4/1/2034","5/1/2034","6/1/2034","7/1/2034","8/1/2034","9/1/2034","10/1/2034","11/1/2034","12/1/2034",
                    "1/1/2035","2/1/2035","3/1/2035","4/1/2035","5/1/2035","6/1/2035","7/1/2035","8/1/2035","9/1/2035","10/1/2035","11/1/2035","12/1/2035",
                    "1/1/2036","2/1/2036","3/1/2036","4/1/2036","5/1/2036","6/1/2036","7/1/2036","8/1/2036","9/1/2036","10/1/2036","11/1/2036","12/1/2036",
                    "1/1/2037","2/1/2037","3/1/2037","4/1/2037","5/1/2037","6/1/2037","7/1/2037","8/1/2037","9/1/2037","10/1/2037","11/1/2037","12/1/2037",
                    "1/1/2038","2/1/2038","3/1/2038","4/1/2038","5/1/2038","6/1/2038","7/1/2038","8/1/2038","9/1/2038","10/1/2038","11/1/2038","12/1/2038",
                    "1/1/2039","2/1/2039","3/1/2039","4/1/2039","5/1/2039","6/1/2039","7/1/2039","8/1/2039","9/1/2039","10/1/2039","11/1/2039","12/1/2039",
                    "1/1/2040","2/1/2040","3/1/2040","4/1/2040","5/1/2040","6/1/2040","7/1/2040","8/1/2040","9/1/2040","10/1/2040","11/1/2040","12/1/2040",
                    "1/1/2041","2/1/2041","3/1/2041","4/1/2041","5/1/2041","6/1/2041","7/1/2041","8/1/2041","9/1/2041","10/1/2041","11/1/2041","12/1/2041",
                    "1/1/2042","2/1/2042","3/1/2042","4/1/2042","5/1/2042","6/1/2042","7/1/2042","8/1/2042","9/1/2042","10/1/2042","11/1/2042","12/1/2042",
                    "1/1/2043","2/1/2043","3/1/2043","4/1/2043","5/1/2043","6/1/2043","7/1/2043","8/1/2043","9/1/2043","10/1/2043","11/1/2043","12/1/2043",
                    "1/1/2044","2/1/2044","3/1/2044","4/1/2044","5/1/2044","6/1/2044","7/1/2044","8/1/2044","9/1/2044","10/1/2044","11/1/2044","12/1/2044",
                    "1/1/2045","2/1/2045","3/1/2045","4/1/2045","5/1/2045","6/1/2045","7/1/2045","8/1/2045","9/1/2045","10/1/2045","11/1/2045","12/1/2045",
                    "1/1/2046","2/1/2046","3/1/2046","4/1/2046","5/1/2046","6/1/2046","7/1/2046","8/1/2046","9/1/2046","10/1/2046","11/1/2046","12/1/2046",
                    "1/1/2047","2/1/2047","3/1/2047","4/1/2047","5/1/2047","6/1/2047","7/1/2047","8/1/2047","9/1/2047","10/1/2047","11/1/2047","12/1/2047",
                    "1/1/2048","2/1/2048","3/1/2048","4/1/2048","5/1/2048","6/1/2048","7/1/2048","8/1/2048","9/1/2048","10/1/2048","11/1/2048","12/1/2048",
                    "1/1/2049","2/1/2049","3/1/2049","4/1/2049","5/1/2049","6/1/2049","7/1/2049","8/1/2049","9/1/2049","10/1/2049","11/1/2049","12/1/2049",
                    "1/1/2050","2/1/2050","3/1/2050","4/1/2050","5/1/2050","6/1/2050","7/1/2050","8/1/2050","9/1/2050","10/1/2050","11/1/2050","12/1/2050",
                    "1/1/2051","2/1/2051","3/1/2051","4/1/2051","5/1/2051","6/1/2051","7/1/2051","8/1/2051","9/1/2051","10/1/2051","11/1/2051","12/1/2051",
                    "1/1/2052","2/1/2052","3/1/2052","4/1/2052","5/1/2052","6/1/2052","7/1/2052","8/1/2052","9/1/2052","10/1/2052","11/1/2052","12/1/2052",
                    "1/1/2053","2/1/2053","3/1/2053","4/1/2053","5/1/2053","6/1/2053","7/1/2053","8/1/2053","9/1/2053","10/1/2053","11/1/2053","12/1/2053",
                    "1/1/2054","2/1/2054","3/1/2054","4/1/2054","5/1/2054","6/1/2054","7/1/2054","8/1/2054","9/1/2054","10/1/2054","11/1/2054","12/1/2054",
                    "1/1/2055","2/1/2055","3/1/2055","4/1/2055","5/1/2055","6/1/2055","7/1/2055","8/1/2055","9/1/2055","10/1/2055","11/1/2055","12/1/2055",
                    "1/1/2056","2/1/2056","3/1/2056","4/1/2056","5/1/2056","6/1/2056","7/1/2056","8/1/2056","9/1/2056","10/1/2056","11/1/2056","12/1/2056",
                    "1/1/2057","2/1/2057","3/1/2057","4/1/2057","5/1/2057","6/1/2057","7/1/2057","8/1/2057","9/1/2057","10/1/2057","11/1/2057","12/1/2057",
                    "1/1/2058","2/1/2058","3/1/2058","4/1/2058","5/1/2058","6/1/2058","7/1/2058","8/1/2058","9/1/2058","10/1/2058","11/1/2058","12/1/2058",
                    "1/1/2059","2/1/2059","3/1/2059","4/1/2059","5/1/2059","6/1/2059","7/1/2059","8/1/2059","9/1/2059","10/1/2059","11/1/2059","12/1/2059",
                    "1/1/2060","2/1/2060","3/1/2060","4/1/2060","5/1/2060","6/1/2060","7/1/2060","8/1/2060","9/1/2060","10/1/2060","11/1/2060","12/1/2060",
                    "1/1/2061","2/1/2061","3/1/2061","4/1/2061","5/1/2061","6/1/2061","7/1/2061","8/1/2061","9/1/2061","10/1/2061","11/1/2061","12/1/2061",
                    "1/1/2062","2/1/2062","3/1/2062","4/1/2062","5/1/2062","6/1/2062","7/1/2062","8/1/2062","9/1/2062","10/1/2062","11/1/2062","12/1/2062",
                    "1/1/2063","2/1/2063","3/1/2063","4/1/2063","5/1/2063","6/1/2063","7/1/2063","8/1/2063","9/1/2063","10/1/2063","11/1/2063","12/1/2063",
                    "1/1/2064","2/1/2064","3/1/2064","4/1/2064","5/1/2064","6/1/2064","7/1/2064","8/1/2064","9/1/2064","10/1/2064","11/1/2064","12/1/2064",
                    "1/1/2065","2/1/2065","3/1/2065","4/1/2065","5/1/2065","6/1/2065","7/1/2065","8/1/2065","9/1/2065","10/1/2065","11/1/2065","12/1/2065",
                    "1/1/2066","2/1/2066","3/1/2066","4/1/2066","5/1/2066","6/1/2066","7/1/2066","8/1/2066","9/1/2066","10/1/2066","11/1/2066","12/1/2066",
                    "1/1/2067","2/1/2067","3/1/2067","4/1/2067","5/1/2067","6/1/2067","7/1/2067","8/1/2067","9/1/2067","10/1/2067","11/1/2067","12/1/2067",
                    "1/1/2068","2/1/2068","3/1/2068","4/1/2068","5/1/2068","6/1/2068","7/1/2068","8/1/2068","9/1/2068","10/1/2068","11/1/2068","12/1/2068",
                    "1/1/2069","2/1/2069","3/1/2069","4/1/2069","5/1/2069","6/1/2069","7/1/2069","8/1/2069","9/1/2069","10/1/2069","11/1/2069","12/1/2069",
                    "1/1/2070","2/1/2070","3/1/2070","4/1/2070","5/1/2070","6/1/2070","7/1/2070","8/1/2070","9/1/2070","10/1/2070","11/1/2070","12/1/2070"
  )
  return(df)
}

lsmonthly <- function(df, time)
{
  df <- merge(df,time, by="HUC4")
  df <-  df[,c(1:58,107:118)]
  for (j in 0:55){
    for (n in 59:70){
      k=n + 12 + (j*12)
      m=j+3  
      df[,k] <- df[,m]*df[,n]
    }}
  df <- df[,c(1:2,71:742)]
  colnames(df) <- c("HUC4","sector","1/1/2015","2/1/2015","3/1/2015","4/1/2015","5/1/2015","6/1/2015","7/1/2015","8/1/2015","9/1/2015","10/1/2015","11/1/2015","12/1/2015",
                    "1/1/2016","2/1/2016","3/1/2016","4/1/2016","5/1/2016","6/1/2016","7/1/2016","8/1/2016","9/1/2016","10/1/2016","11/1/2016","12/1/2016",
                    "1/1/2017","2/1/2017","3/1/2017","4/1/2017","5/1/2017","6/1/2017","7/1/2017","8/1/2017","9/1/2017","10/1/2017","11/1/2017","12/1/2017",
                    "1/1/2018","2/1/2018","3/1/2018","4/1/2018","5/1/2018","6/1/2018","7/1/2018","8/1/2018","9/1/2018","10/1/2018","11/1/2018","12/1/2018",
                    "1/1/2019","2/1/2019","3/1/2019","4/1/2019","5/1/2019","6/1/2019","7/1/2019","8/1/2019","9/1/2019","10/1/2019","11/1/2019","12/1/2019",
                    "1/1/2020","2/1/2020","3/1/2020","4/1/2020","5/1/2020","6/1/2020","7/1/2020","8/1/2020","9/1/2020","10/1/2020","11/1/2020","12/1/2020",
                    "1/1/2021","2/1/2021","3/1/2021","4/1/2021","5/1/2021","6/1/2021","7/1/2021","8/1/2021","9/1/2021","10/1/2021","11/1/2021","12/1/2021",
                    "1/1/2022","2/1/2022","3/1/2022","4/1/2022","5/1/2022","6/1/2022","7/1/2022","8/1/2022","9/1/2022","10/1/2022","11/1/2022","12/1/2022",
                    "1/1/2023","2/1/2023","3/1/2023","4/1/2023","5/1/2023","6/1/2023","7/1/2023","8/1/2023","9/1/2023","10/1/2023","11/1/2023","12/1/2023",
                    "1/1/2024","2/1/2024","3/1/2024","4/1/2024","5/1/2024","6/1/2024","7/1/2024","8/1/2024","9/1/2024","10/1/2024","11/1/2024","12/1/2024",
                    "1/1/2025","2/1/2025","3/1/2025","4/1/2025","5/1/2025","6/1/2025","7/1/2025","8/1/2025","9/1/2025","10/1/2025","11/1/2025","12/1/2025",
                    "1/1/2026","2/1/2026","3/1/2026","4/1/2026","5/1/2026","6/1/2026","7/1/2026","8/1/2026","9/1/2026","10/1/2026","11/1/2026","12/1/2026",
                    "1/1/2027","2/1/2027","3/1/2027","4/1/2027","5/1/2027","6/1/2027","7/1/2027","8/1/2027","9/1/2027","10/1/2027","11/1/2027","12/1/2027",
                    "1/1/2028","2/1/2028","3/1/2028","4/1/2028","5/1/2028","6/1/2028","7/1/2028","8/1/2028","9/1/2028","10/1/2028","11/1/2028","12/1/2028",
                    "1/1/2029","2/1/2029","3/1/2029","4/1/2029","5/1/2029","6/1/2029","7/1/2029","8/1/2029","9/1/2029","10/1/2029","11/1/2029","12/1/2029",
                    "1/1/2030","2/1/2030","3/1/2030","4/1/2030","5/1/2030","6/1/2030","7/1/2030","8/1/2030","9/1/2030","10/1/2030","11/1/2030","12/1/2030",
                    "1/1/2031","2/1/2031","3/1/2031","4/1/2031","5/1/2031","6/1/2031","7/1/2031","8/1/2031","9/1/2031","10/1/2031","11/1/2031","12/1/2031",
                    "1/1/2032","2/1/2032","3/1/2032","4/1/2032","5/1/2032","6/1/2032","7/1/2032","8/1/2032","9/1/2032","10/1/2032","11/1/2032","12/1/2032",
                    "1/1/2033","2/1/2033","3/1/2033","4/1/2033","5/1/2033","6/1/2033","7/1/2033","8/1/2033","9/1/2033","10/1/2033","11/1/2033","12/1/2033",
                    "1/1/2034","2/1/2034","3/1/2034","4/1/2034","5/1/2034","6/1/2034","7/1/2034","8/1/2034","9/1/2034","10/1/2034","11/1/2034","12/1/2034",
                    "1/1/2035","2/1/2035","3/1/2035","4/1/2035","5/1/2035","6/1/2035","7/1/2035","8/1/2035","9/1/2035","10/1/2035","11/1/2035","12/1/2035",
                    "1/1/2036","2/1/2036","3/1/2036","4/1/2036","5/1/2036","6/1/2036","7/1/2036","8/1/2036","9/1/2036","10/1/2036","11/1/2036","12/1/2036",
                    "1/1/2037","2/1/2037","3/1/2037","4/1/2037","5/1/2037","6/1/2037","7/1/2037","8/1/2037","9/1/2037","10/1/2037","11/1/2037","12/1/2037",
                    "1/1/2038","2/1/2038","3/1/2038","4/1/2038","5/1/2038","6/1/2038","7/1/2038","8/1/2038","9/1/2038","10/1/2038","11/1/2038","12/1/2038",
                    "1/1/2039","2/1/2039","3/1/2039","4/1/2039","5/1/2039","6/1/2039","7/1/2039","8/1/2039","9/1/2039","10/1/2039","11/1/2039","12/1/2039",
                    "1/1/2040","2/1/2040","3/1/2040","4/1/2040","5/1/2040","6/1/2040","7/1/2040","8/1/2040","9/1/2040","10/1/2040","11/1/2040","12/1/2040",
                    "1/1/2041","2/1/2041","3/1/2041","4/1/2041","5/1/2041","6/1/2041","7/1/2041","8/1/2041","9/1/2041","10/1/2041","11/1/2041","12/1/2041",
                    "1/1/2042","2/1/2042","3/1/2042","4/1/2042","5/1/2042","6/1/2042","7/1/2042","8/1/2042","9/1/2042","10/1/2042","11/1/2042","12/1/2042",
                    "1/1/2043","2/1/2043","3/1/2043","4/1/2043","5/1/2043","6/1/2043","7/1/2043","8/1/2043","9/1/2043","10/1/2043","11/1/2043","12/1/2043",
                    "1/1/2044","2/1/2044","3/1/2044","4/1/2044","5/1/2044","6/1/2044","7/1/2044","8/1/2044","9/1/2044","10/1/2044","11/1/2044","12/1/2044",
                    "1/1/2045","2/1/2045","3/1/2045","4/1/2045","5/1/2045","6/1/2045","7/1/2045","8/1/2045","9/1/2045","10/1/2045","11/1/2045","12/1/2045",
                    "1/1/2046","2/1/2046","3/1/2046","4/1/2046","5/1/2046","6/1/2046","7/1/2046","8/1/2046","9/1/2046","10/1/2046","11/1/2046","12/1/2046",
                    "1/1/2047","2/1/2047","3/1/2047","4/1/2047","5/1/2047","6/1/2047","7/1/2047","8/1/2047","9/1/2047","10/1/2047","11/1/2047","12/1/2047",
                    "1/1/2048","2/1/2048","3/1/2048","4/1/2048","5/1/2048","6/1/2048","7/1/2048","8/1/2048","9/1/2048","10/1/2048","11/1/2048","12/1/2048",
                    "1/1/2049","2/1/2049","3/1/2049","4/1/2049","5/1/2049","6/1/2049","7/1/2049","8/1/2049","9/1/2049","10/1/2049","11/1/2049","12/1/2049",
                    "1/1/2050","2/1/2050","3/1/2050","4/1/2050","5/1/2050","6/1/2050","7/1/2050","8/1/2050","9/1/2050","10/1/2050","11/1/2050","12/1/2050",
                    "1/1/2051","2/1/2051","3/1/2051","4/1/2051","5/1/2051","6/1/2051","7/1/2051","8/1/2051","9/1/2051","10/1/2051","11/1/2051","12/1/2051",
                    "1/1/2052","2/1/2052","3/1/2052","4/1/2052","5/1/2052","6/1/2052","7/1/2052","8/1/2052","9/1/2052","10/1/2052","11/1/2052","12/1/2052",
                    "1/1/2053","2/1/2053","3/1/2053","4/1/2053","5/1/2053","6/1/2053","7/1/2053","8/1/2053","9/1/2053","10/1/2053","11/1/2053","12/1/2053",
                    "1/1/2054","2/1/2054","3/1/2054","4/1/2054","5/1/2054","6/1/2054","7/1/2054","8/1/2054","9/1/2054","10/1/2054","11/1/2054","12/1/2054",
                    "1/1/2055","2/1/2055","3/1/2055","4/1/2055","5/1/2055","6/1/2055","7/1/2055","8/1/2055","9/1/2055","10/1/2055","11/1/2055","12/1/2055",
                    "1/1/2056","2/1/2056","3/1/2056","4/1/2056","5/1/2056","6/1/2056","7/1/2056","8/1/2056","9/1/2056","10/1/2056","11/1/2056","12/1/2056",
                    "1/1/2057","2/1/2057","3/1/2057","4/1/2057","5/1/2057","6/1/2057","7/1/2057","8/1/2057","9/1/2057","10/1/2057","11/1/2057","12/1/2057",
                    "1/1/2058","2/1/2058","3/1/2058","4/1/2058","5/1/2058","6/1/2058","7/1/2058","8/1/2058","9/1/2058","10/1/2058","11/1/2058","12/1/2058",
                    "1/1/2059","2/1/2059","3/1/2059","4/1/2059","5/1/2059","6/1/2059","7/1/2059","8/1/2059","9/1/2059","10/1/2059","11/1/2059","12/1/2059",
                    "1/1/2060","2/1/2060","3/1/2060","4/1/2060","5/1/2060","6/1/2060","7/1/2060","8/1/2060","9/1/2060","10/1/2060","11/1/2060","12/1/2060",
                    "1/1/2061","2/1/2061","3/1/2061","4/1/2061","5/1/2061","6/1/2061","7/1/2061","8/1/2061","9/1/2061","10/1/2061","11/1/2061","12/1/2061",
                    "1/1/2062","2/1/2062","3/1/2062","4/1/2062","5/1/2062","6/1/2062","7/1/2062","8/1/2062","9/1/2062","10/1/2062","11/1/2062","12/1/2062",
                    "1/1/2063","2/1/2063","3/1/2063","4/1/2063","5/1/2063","6/1/2063","7/1/2063","8/1/2063","9/1/2063","10/1/2063","11/1/2063","12/1/2063",
                    "1/1/2064","2/1/2064","3/1/2064","4/1/2064","5/1/2064","6/1/2064","7/1/2064","8/1/2064","9/1/2064","10/1/2064","11/1/2064","12/1/2064",
                    "1/1/2065","2/1/2065","3/1/2065","4/1/2065","5/1/2065","6/1/2065","7/1/2065","8/1/2065","9/1/2065","10/1/2065","11/1/2065","12/1/2065",
                    "1/1/2066","2/1/2066","3/1/2066","4/1/2066","5/1/2066","6/1/2066","7/1/2066","8/1/2066","9/1/2066","10/1/2066","11/1/2066","12/1/2066",
                    "1/1/2067","2/1/2067","3/1/2067","4/1/2067","5/1/2067","6/1/2067","7/1/2067","8/1/2067","9/1/2067","10/1/2067","11/1/2067","12/1/2067",
                    "1/1/2068","2/1/2068","3/1/2068","4/1/2068","5/1/2068","6/1/2068","7/1/2068","8/1/2068","9/1/2068","10/1/2068","11/1/2068","12/1/2068",
                    "1/1/2069","2/1/2069","3/1/2069","4/1/2069","5/1/2069","6/1/2069","7/1/2069","8/1/2069","9/1/2069","10/1/2069","11/1/2069","12/1/2069",
                    "1/1/2070","2/1/2070","3/1/2070","4/1/2070","5/1/2070","6/1/2070","7/1/2070","8/1/2070","9/1/2070","10/1/2070","11/1/2070","12/1/2070"
  )
  return(df)
}

names(ir)[names(ir) == 'HUC_4'] <- 'HUC4'

ir <- irmonthly(ir,month)

dp.ssp1 <- dpmonthly(dp.ssp1,month)
dp.ssp2 <- dpmonthly(dp.ssp2,month)
dp.ssp3 <- dpmonthly(dp.ssp3,month)
dp.ssp4 <- dpmonthly(dp.ssp4,month)
dp.ssp5 <- dpmonthly(dp.ssp5,month)

ic.ssp1 <- icmonthly(ic.ssp1,month)
ic.ssp2 <- icmonthly(ic.ssp2,month)
ic.ssp3 <- icmonthly(ic.ssp3,month)
ic.ssp4 <- icmonthly(ic.ssp4,month)
ic.ssp5 <- icmonthly(ic.ssp5,month)

th.ssp1 <- thmonthly(th.ssp1,month)
th.ssp2 <- thmonthly(th.ssp2,month)
th.ssp3 <- thmonthly(th.ssp3,month)
th.ssp4 <- thmonthly(th.ssp4,month)
th.ssp5 <- thmonthly(th.ssp5,month)

aq.ssp1 <- lsmonthly(aq.ssp1,month)
aq.ssp2 <- lsmonthly(aq.ssp2,month)
aq.ssp3 <- lsmonthly(aq.ssp3,month)
aq.ssp4 <- lsmonthly(aq.ssp4,month)
aq.ssp5 <- lsmonthly(aq.ssp5,month)

ls.ssp1 <- lsmonthly(ls.ssp1,month)
ls.ssp2 <- lsmonthly(ls.ssp2,month)
ls.ssp3 <- lsmonthly(ls.ssp3,month)
ls.ssp4 <- lsmonthly(ls.ssp4,month)
ls.ssp5 <- lsmonthly(ls.ssp5,month)


#----------------------------------------------------------------------------------------------------------------------
#This section aggregates sector water uses as needed by WEAP. 
#consumption is total demand
#consumption1 is high value use (D, IC, TH)
#consumption2 is low value use (IR, LS, AQ)
#----------------------------------------------------------------------------------------------------------------------


consumption_ssp1 <- rbind(ir, dp.ssp1, ic.ssp1,th.ssp1, aq.ssp1, ls.ssp1)
consumption_ssp1 <- consumption_ssp1[,c(1,3:674)]
consumption_ssp1 <- consumption_ssp1 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption_ssp2 <- rbind(ir, dp.ssp2, ic.ssp2,th.ssp2, aq.ssp2, ls.ssp2)
consumption_ssp2 <- consumption_ssp2[,c(1,3:674)]
consumption_ssp2 <- consumption_ssp2 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption_ssp3 <- rbind(ir, dp.ssp3, ic.ssp3,th.ssp3, aq.ssp3, ls.ssp3)
consumption_ssp3 <- consumption_ssp3[,c(1,3:674)]
consumption_ssp3 <- consumption_ssp3 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption_ssp4 <- rbind(ir, dp.ssp4, ic.ssp4,th.ssp4, aq.ssp4, ls.ssp4)
consumption_ssp4 <- consumption_ssp4[,c(1,3:674)]
consumption_ssp4 <- consumption_ssp4 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption_ssp5 <- rbind(ir, dp.ssp5, ic.ssp5,th.ssp5, aq.ssp5, ls.ssp5)
consumption_ssp5 <- consumption_ssp5[,c(1,3:674)]
consumption_ssp5 <- consumption_ssp5 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption1_ssp1 <- rbind(dp.ssp1, ic.ssp1,th.ssp1)
consumption1_ssp1 <- consumption1_ssp1[,c(1,3:674)]
consumption1_ssp1 <- consumption1_ssp1 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption1_ssp2 <- rbind(dp.ssp2, ic.ssp2,th.ssp2)
consumption1_ssp2 <- consumption1_ssp2[,c(1,3:674)]
consumption1_ssp2 <- consumption1_ssp2 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption1_ssp3 <- rbind(dp.ssp3, ic.ssp3,th.ssp3)
consumption1_ssp3 <- consumption1_ssp3[,c(1,3:674)]
consumption1_ssp3 <- consumption1_ssp3 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption1_ssp4 <- rbind(dp.ssp4, ic.ssp4,th.ssp4)
consumption1_ssp4 <- consumption1_ssp4[,c(1,3:674)]
consumption1_ssp4 <- consumption1_ssp4 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption1_ssp5 <- rbind(dp.ssp5, ic.ssp5,th.ssp5)
consumption1_ssp5 <- consumption1_ssp5[,c(1,3:674)]
consumption1_ssp5 <- consumption1_ssp5 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption2_ssp1 <- rbind(ir, aq.ssp1, ls.ssp1)
consumption2_ssp1 <- consumption2_ssp1[,c(1,3:674)]
consumption2_ssp1 <- consumption2_ssp1 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption2_ssp2 <- rbind(ir, aq.ssp2, ls.ssp2)
consumption2_ssp2 <- consumption2_ssp2[,c(1,3:674)]
consumption2_ssp2 <- consumption2_ssp2 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption2_ssp3 <- rbind(ir, aq.ssp3, ls.ssp3)
consumption2_ssp3 <- consumption2_ssp3[,c(1,3:674)]
consumption2_ssp3 <- consumption2_ssp3 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption2_ssp4 <- rbind(ir, aq.ssp4, ls.ssp4)
consumption2_ssp4 <- consumption2_ssp4[,c(1,3:674)]
consumption2_ssp4 <- consumption2_ssp4 %>% group_by(HUC_4) %>% summarise_all(funs(sum))

consumption2_ssp5 <- rbind(ir, aq.ssp5, ls.ssp5)
consumption2_ssp5 <- consumption2_ssp5[,c(1,3:674)]
consumption2_ssp5 <- consumption2_ssp5 %>% group_by(HUC_4) %>% summarise_all(funs(sum))


#----------------------------------------------------------------------------------------------------------------------
#Transposes dataframes to match WEAP input format. 
#----------------------------------------------------------------------------------------------------------------------



transpose <- function(DF)
{
  DF <- t(DF)
  DF <- as.data.frame(DF)
  
  colnames(DF) = DF[1, ]
  DF = DF[-1, ] 
  return(DF)
}

consumption_ssp1 <- transpose(consumption_ssp1)
consumption_ssp2 <- transpose(consumption_ssp2)
consumption_ssp3 <- transpose(consumption_ssp3)
consumption_ssp4 <- transpose(consumption_ssp4)
consumption_ssp5 <- transpose(consumption_ssp5)

consumption1_ssp1 <- transpose(consumption1_ssp1)
consumption1_ssp2 <- transpose(consumption1_ssp2)
consumption1_ssp3 <- transpose(consumption1_ssp3)
consumption1_ssp4 <- transpose(consumption1_ssp4)
consumption1_ssp5 <- transpose(consumption1_ssp5)

consumption2_ssp1 <- transpose(consumption2_ssp1)
consumption2_ssp2 <- transpose(consumption2_ssp2)
consumption2_ssp3 <- transpose(consumption2_ssp3)
consumption2_ssp4 <- transpose(consumption2_ssp4)
consumption2_ssp5 <- transpose(consumption2_ssp5)

consumption_ssp1 <- consumption_ssp1[,c(length(colnames(consumption_ssp1)):1)]
consumption_ssp2 <- consumption_ssp2[,c(length(colnames(consumption_ssp2)):1)]
consumption_ssp3 <- consumption_ssp3[,c(length(colnames(consumption_ssp3)):1)]
consumption_ssp4 <- consumption_ssp4[,c(length(colnames(consumption_ssp4)):1)]
consumption_ssp5 <- consumption_ssp5[,c(length(colnames(consumption_ssp5)):1)]

consumption1_ssp1 <- consumption1_ssp1[,c(length(colnames(consumption1_ssp1)):1)]
consumption1_ssp2 <- consumption1_ssp2[,c(length(colnames(consumption1_ssp2)):1)]
consumption1_ssp3 <- consumption1_ssp3[,c(length(colnames(consumption1_ssp3)):1)]
consumption1_ssp4 <- consumption1_ssp4[,c(length(colnames(consumption1_ssp4)):1)]
consumption1_ssp5 <- consumption1_ssp5[,c(length(colnames(consumption1_ssp5)):1)]

consumption2_ssp1 <- consumption2_ssp1[,c(length(colnames(consumption2_ssp1)):1)]
consumption2_ssp2 <- consumption2_ssp2[,c(length(colnames(consumption2_ssp2)):1)]
consumption2_ssp3 <- consumption2_ssp3[,c(length(colnames(consumption2_ssp3)):1)]
consumption2_ssp4 <- consumption2_ssp4[,c(length(colnames(consumption2_ssp4)):1)]
consumption2_ssp5 <- consumption2_ssp5[,c(length(colnames(consumption2_ssp5)):1)]

#----------------------------------------------------------------------------------------------------------------------
#Output for WEAP
#----------------------------------------------------------------------------------------------------------------------


write.csv(consumption_ssp1, file="consumption_ssp1.csv")
write.csv(consumption_ssp2, file="consumption_ssp2.csv")
write.csv(consumption_ssp3, file="consumption_ssp3.csv")
write.csv(consumption_ssp4, file="consumption_ssp4.csv")
write.csv(consumption_ssp5, file="consumption_ssp5.csv")

write.csv(consumption1_ssp1, file="consumption1_ssp1.csv")
write.csv(consumption1_ssp2, file="consumption1_ssp2.csv")
write.csv(consumption1_ssp3, file="consumption1_ssp3.csv")
write.csv(consumption1_ssp4, file="consumption1_ssp4.csv")
write.csv(consumption1_ssp5, file="consumption1_ssp5.csv")

write.csv(consumption2_ssp1, file="consumption2_ssp1.csv")
write.csv(consumption2_ssp2, file="consumption2_ssp2.csv")
write.csv(consumption2_ssp3, file="consumption2_ssp3.csv")
write.csv(consumption2_ssp4, file="consumption2_ssp4.csv")
write.csv(consumption2_ssp5, file="consumption2_ssp5.csv")




