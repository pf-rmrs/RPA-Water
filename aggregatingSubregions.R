#Noo!!!!
jjkll
ddd
sss

### conflicts

# this whole file is working with consumptive use
# You should download the ConsumptiveOnly folder to get the input files for this first section

rm(list = ls())

#setwd("C:/Users/twwarziniack/Documents/5_RPA/Subregions")
setwd("D:/Demand model/Demand Results - Supplemental for Publication/ConsumptiveOnly")

library(tidyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(data.table)
library(choroplethr)
library(choroplethrMaps)

################## Read in consumptive use data (all sectors) ###############
# below are bringing in all water data from all sectors
# BELOW ARE ALSO CONSUMPTIVE USE ONLY
cnrm45ssp1 <- read.csv("./Projections_cnrm45/ssp1_all.csv")
cnrm85ssp2 <- read.csv("./Projections_cnrm85/ssp2_all.csv")
cnrm85ssp3 <- read.csv("./Projections_cnrm85/ssp3_all.csv")
cnrm85ssp5 <- read.csv("./Projections_cnrm85/ssp5_all.csv")
hadgem45ssp1 <- read.csv("./Projections_hadgem45/ssp1_all.csv")
hadgem85ssp2 <- read.csv("./Projections_hadgem85/ssp2_all.csv")
hadgem85ssp3 <- read.csv("./Projections_hadgem85/ssp3_all.csv")
hadgem85ssp5 <- read.csv("./Projections_hadgem85/ssp5_all.csv")
ipsl45ssp1 <- read.csv("./Projections_ipsl45/ssp1_all.csv")
ipsl85ssp2 <- read.csv("./Projections_ipsl85/ssp2_all.csv")
ipsl85ssp3 <- read.csv("./Projections_ipsl85/ssp3_all.csv")
ipsl85ssp5 <- read.csv("./Projections_ipsl85/ssp5_all.csv")
mri45ssp1 <- read.csv("./Projections_mri45/ssp1_all.csv")
mri85ssp2 <- read.csv("./Projections_mri85/ssp2_all.csv")
mri85ssp3 <- read.csv("./Projections_mri85/ssp3_all.csv")
mri85ssp5 <- read.csv("./Projections_mri85/ssp5_all.csv")
noresm45ssp1 <- read.csv("./Projections_noresm45/ssp1_all.csv")
noresm85ssp2 <- read.csv("./Projections_noresm85/ssp2_all.csv")
noresm85ssp3 <- read.csv("./Projections_noresm85/ssp3_all.csv")
noresm85ssp5 <- read.csv("./Projections_noresm85/ssp5_all.csv")

# --- Travis adding for paper
# 2015 data is same for all scenarios, so choose one to use as current
current <- cnrm45ssp1

current <- select(current, fips, sector, Y2015)
current[,3][is.na(current[,3])] <- 0

# can I make one large data file?

cnrm45ssp1$model <- "cnrm45-1"
cnrm85ssp2$model <- "cnrm85-2"
cnrm85ssp3$model <- "cnrm85-3"
cnrm85ssp5$model <- "cnrm85-5"
hadgem45ssp1$model <- "hadgem45-1"
hadgem85ssp2$model <- "hadgem85-2"
hadgem85ssp3$model <- "hadgem85-3"
hadgem85ssp5$model <- "hadgem85-5"
ipsl45ssp1$model <- "ipsl45-1"
ipsl85ssp2$model <- "ipsl85-2"
ipsl85ssp3$model <- "ipsl85-3"
ipsl85ssp5$model <- "ipsl85-5"
mri45ssp1$model <- "mri45-1"
mri85ssp2$model <- "mri85-2"
mri85ssp3$model <- "mri85-3"
mri85ssp5$model <- "mri85-5"
noresm45ssp1$model <- "noresm45-1"
noresm85ssp2$model <- "noresm85-2"
noresm85ssp3$model <- "noresm85-3"
noresm85ssp5$model <- "noresm85-5"

df <- rbind(cnrm45ssp1,
            cnrm85ssp2,
            cnrm85ssp3,
            cnrm85ssp5,
            hadgem45ssp1,
            hadgem85ssp2,
            hadgem85ssp3,
            hadgem85ssp5,
            ipsl45ssp1,
            ipsl85ssp2,
            ipsl85ssp3,
            ipsl85ssp5,
            mri45ssp1,
            mri85ssp2,
            mri85ssp3,
            mri85ssp5,
            noresm45ssp1,
            noresm85ssp2,
            noresm85ssp3,
            noresm85ssp5)
            

df$change <- (df$Y2070 / df$Y2015) - 1

dp <- subset(df, sector=='dp')
dp2 <- select(dp, fips, model, change)
dp2 <- dp2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))

ic <- subset(df, sector=='ic')
ic2 <- select(ic, fips, model, change)
ic2 <- ic2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))

ir <- subset(df, sector=='ir')
ir2 <- select(ir, fips, model, change)
ir2 <- ir2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))

th <- subset(df, sector=='th')
th2 <- select(th, fips, model, change)
th2 <- th2 %>% group_by(fips) %>% summarise_each(funs(mean, sd))

## -- missing ls and aq

setwd("D:/Demand model/Demand Results - Supplemental for Publication/Core Scenarios")

cnrm45.1.ls <- read.csv("./RCP 4.5 SSP 1/CNRM-CM5/ls_ssp1.csv")
cnrm45.1.aq <- read.csv("./RCP 4.5 SSP 1/CNRM-CM5/aq_ssp1.csv")
hadgem45.1.ls <- read.csv("./RCP 4.5 SSP 1/hadGEM2-ES365/ls_ssp1.csv")
hadgem45.1.aq <- read.csv("./RCP 4.5 SSP 1/hadGEM2-ES365/aq_ssp1.csv")
ipsl45.1.ls <- read.csv("./RCP 4.5 SSP 1/IPSL-CM5A-M4/ls_ssp1.csv")
ipsl45.1.aq <- read.csv("./RCP 4.5 SSP 1/IPSL-CM5A-M4/aq_ssp1.csv")
mri45.1.ls <- read.csv("./RCP 4.5 SSP 1/MRI-CGCM3/ls_ssp1.csv")
mri45.1.aq <- read.csv("./RCP 4.5 SSP 1/MRI-CGCM3/aq_ssp1.csv")
noresm45.1.ls <- read.csv("./RCP 4.5 SSP 1/NorESM1-M/ls_ssp1.csv")
noresm45.1.aq <- read.csv("./RCP 4.5 SSP 1/NorESM1-M/aq_ssp1.csv")

cnrm85.2.ls <- read.csv("./RCP 8.5 SSP 2/CNRM-CM5/ls_ssp2.csv")
cnrm85.2.aq <- read.csv("./RCP 8.5 SSP 2/CNRM-CM5/aq_ssp2.csv")
hadgem85.2.ls <- read.csv("./RCP 8.5 SSP 2/hadGEM2-ES365/ls_ssp2.csv")
hadgem85.2.aq <- read.csv("./RCP 8.5 SSP 2/hadGEM2-ES365/aq_ssp2.csv")
ipsl85.2.ls <- read.csv("./RCP 8.5 SSP 2/IPSL-CM5A-M4/ls_ssp2.csv")
ipsl85.2.aq <- read.csv("./RCP 8.5 SSP 2/IPSL-CM5A-M4/aq_ssp2.csv")
mri85.2.ls <- read.csv("./RCP 8.5 SSP 2/MRI-CGCM3/ls_ssp2.csv")
mri85.2.aq <- read.csv("./RCP 8.5 SSP 2/MRI-CGCM3/aq_ssp2.csv")
noresm85.2.ls <- read.csv("./RCP 8.5 SSP 2/NorESM1-M/ls_ssp2.csv")
noresm85.2.aq <- read.csv("./RCP 8.5 SSP 2/NorESM1-M/aq_ssp2.csv")

cnrm85.3.ls <- read.csv("./RCP 8.5 SSP 3/CNRM-CM5/ls_ssp3.csv")
cnrm85.3.aq <- read.csv("./RCP 8.5 SSP 3/CNRM-CM5/aq_ssp3.csv")
hadgem85.3.ls <- read.csv("./RCP 8.5 SSP 3/hadGEM2-ES365/ls_ssp3.csv")
hadgem85.3.aq <- read.csv("./RCP 8.5 SSP 3/hadGEM2-ES365/aq_ssp3.csv")
ipsl85.3.ls <- read.csv("./RCP 8.5 SSP 3/IPSL-CM5A-M4/ls_ssp3.csv")
ipsl85.3.aq <- read.csv("./RCP 8.5 SSP 3/IPSL-CM5A-M4/aq_ssp3.csv")
mri85.3.ls <- read.csv("./RCP 8.5 SSP 3/MRI-CGCM3/ls_ssp3.csv")
mri85.3.aq <- read.csv("./RCP 8.5 SSP 3/MRI-CGCM3/aq_ssp3.csv")
noresm85.3.ls <- read.csv("./RCP 8.5 SSP 3/NorESM1-M/ls_ssp3.csv")
noresm85.3.aq <- read.csv("./RCP 8.5 SSP 3/NorESM1-M/aq_ssp3.csv")

cnrm85.5.ls <- read.csv("./RCP 8.5 SSP 5/CNRM-CM5/ls_ssp5.csv")
cnrm85.5.aq <- read.csv("./RCP 8.5 SSP 5/CNRM-CM5/aq_ssp5.csv")
hadgem85.5.ls <- read.csv("./RCP 8.5 SSP 5/hadGEM2-ES365/ls_ssp5.csv")
hadgem85.5.aq <- read.csv("./RCP 8.5 SSP 5/hadGEM2-ES365/aq_ssp5.csv")
ipsl85.5.ls <- read.csv("./RCP 8.5 SSP 5/IPSL-CM5A-M4/ls_ssp5.csv")
ipsl85.5.aq <- read.csv("./RCP 8.5 SSP 5/IPSL-CM5A-M4/aq_ssp5.csv")
mri85.5.ls <- read.csv("./RCP 8.5 SSP 5/MRI-CGCM3/ls_ssp5.csv")
mri85.5.aq <- read.csv("./RCP 8.5 SSP 5/MRI-CGCM3/aq_ssp5.csv")
noresm85.5.ls <- read.csv("./RCP 8.5 SSP 5/NorESM1-M/ls_ssp5.csv")
noresm85.5.aq <- read.csv("./RCP 8.5 SSP 5/NorESM1-M/aq_ssp5.csv")

cnrm45.1.aq$model <- 'cnrm45.1'
cnrm45.1.ls$model <- 'cnrm45.1'
cnrm45.1 <- rbind(cnrm45.1.aq, cnrm45.1.ls)

hadgem45.1.ls$model <- 'hadgem45.1'
hadgem45.1.aq$model <- 'hadgem45.1'
hadgem45.1 <- rbind(hadgem45.1.aq, hadgem45.1.ls)

ipsl45.1.ls$model <- 'ipsl45.1'
ipsl45.1.aq$model <- 'ipsl45.1'
ipsl45.1 <- rbind(ipsl45.1.aq, ipsl45.1.ls)

mri45.1.ls$model <- 'mri45.1'
mri45.1.aq$model <- 'mri45.1'
mri45.1 <- rbind(mri45.1.aq, mri45.1.ls)

noresm45.1.ls$model <- 'noresm45.1'
noresm45.1.aq$model <- 'noresm45.1'
noresm45.1 <- rbind(noresm45.1.aq, noresm45.1.ls)

cnrm85.2.ls$model <- 'cnrm85.2'
cnrm85.2.aq$model <- 'cnrm85.2'
cnrm85.2 <- rbind(cnrm85.2.aq, cnrm85.2.ls)

hadgem85.2.ls$model <- 'hadgem85.2'
hadgem85.2.aq$model <- 'hadgem85.2'
hadgem85.2 <- rbind(hadgem85.2.aq, hadgem85.2.ls)

ipsl85.2.ls$model <- 'ipsl85.2'
ipsl85.2.aq$model <- 'ipsl85.2'
ipsl85.2 <- rbind(ipsl85.2.aq, ipsl85.2.ls)

mri85.2.ls$model <- 'mri85.2'
mri85.2.aq$model <- 'mri85.2'
mri85.2 <- rbind(mri85.2.aq, mri85.2.ls)

noresm85.2.ls$model <- 'noresm85.2'
noresm85.2.aq$model <- 'noresm85.2'
noresm85.2 <- rbind(noresm85.2.aq, noresm85.2.ls)

cnrm85.3.ls$model <- 'cnrm85.3'
cnrm85.3.aq$model <- 'cnrm85.3'
cnrm85.3 <- rbind(cnrm85.3.aq, cnrm85.3.ls)

hadgem85.3.ls$model <- 'hadgem85.3'
hadgem85.3.aq$model <- 'hadgem85.3'
hadgem85.3 <- rbind(hadgem85.3.aq, hadgem85.3.ls)

ipsl85.3.ls$model <- 'ipsl85.3'
ipsl85.3.aq$model <- 'ipsl85.3'
ipsl85.3 <- rbind(ipsl85.3.aq, ipsl85.3.ls)

mri85.3.ls$model <- 'mri85.3'
mri85.3.aq$model <- 'mri85.3'
mri85.3 <- rbind(mri85.3.aq, mri85.3.ls)

noresm85.3.ls$model <- 'noresm85.3'
noresm85.3.aq$model <- 'noresm85.3'
noresm85.3 <- rbind(noresm85.3.aq, noresm85.3.ls)

cnrm85.5.ls$model <- 'cnrm85.5'
cnrm85.5.aq$model <- 'cnrm85.5'
cnrm85.5 <- rbind(cnrm85.5.aq, cnrm85.5.ls)

hadgem85.5.ls$model <- 'hadgem85.5'
hadgem85.5.aq$model <- 'hadgem85.5'
hadgem85.5 <- rbind(hadgem85.5.aq, hadgem85.5.ls)

ipsl85.5.ls$model <- 'ipsl85.5'
ipsl85.5.aq$model <- 'ipsl85.5'
ipsl85.5 <- rbind(ipsl85.5.aq, ipsl85.5.ls)

mri85.5.ls$model <- 'mri85.5'
mri85.5.aq$model <- 'mri85.5'
mri85.5 <- rbind(mri85.5.aq, mri85.5.ls)

noresm85.5.ls$model <- 'noresm85.5'
noresm85.5.aq$model <- 'noresm85.5'
noresm85.5 <- rbind(noresm85.5.aq, noresm85.5.ls)

cnrm45.1 <- cnrm45.1 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
hadgem45.1 <- hadgem45.1 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
ipsl45.1 <- ipsl45.1 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
mri45.1 <- mri45.1 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
noresm45.1 <- noresm45.1 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))

cnrm85.2 <- cnrm85.2 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
hadgem85.2 <- hadgem85.2 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
ipsl85.2 <- ipsl85.2 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
mri85.2 <- mri85.2 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
noresm85.2 <- noresm85.2 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))

cnrm85.3 <- cnrm85.3 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
hadgem85.3 <- hadgem85.3 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
ipsl85.3 <- ipsl85.3 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
mri85.3 <- mri85.3 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
noresm85.3 <- noresm85.3 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))

cnrm85.5 <- cnrm85.5 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
hadgem85.5 <- hadgem85.5 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
ipsl85.5 <- ipsl85.5 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
mri85.5 <- mri85.5 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))
noresm85.5 <- noresm85.5 %>% group_by(fips) %>% summarise_at(vars(Y2015, Y2070),funs(sum(.,na.rm=TRUE)))

# no climate feedbacks in la and aq - above could have been done just for ssp

cnrm45.1$model <- "cnrm45-1"
cnrm85.2$model <- "cnrm85-2"
cnrm85.3$model <- "cnrm85-3"
cnrm85.5$model <- "cnrm85-5"
  
all.la <- rbind(cnrm45.1,
                cnrm85.2,
                cnrm85.3,
                cnrm85.5)
all.la$change <- (all.la$Y2070/all.la$Y2015)-1

all.la2 <- all.la %>% group_by(fips) %>% summarise_each(funs(mean, sd))

dp$dp15 <- dp$Y2015
dp$dp70 <- dp$Y2070

ic$ic15 <- ic$Y2015
ic$ic70 <- ic$Y2070

ir$ir15 <- ir$Y2015
ir$ir70 <- ir$Y2070

th$th15 <- th$Y2015
th$th70 <- th$Y2070

all.la$la15 <- all.la$Y2015
all.la$la70 <- all.la$Y2070

dp <- select(dp, fips, dp15, dp70, model)
ic <- select(ic, fips, ic15, ic70, model)
ir <- select(ir, fips, ir15, ir70, model)
th <- select(th, fips, th15, th70, model)
la <- select(all.la, fips, la15, la70, model)
la$fips <-as.character(la$fips)

joined <- left_join(dp, ir, by = c('fips','model'))
joined <- left_join(joined, ic, by = c('fips','model'))
joined <- left_join(joined, th, by = c('fips','model'))
joined <- left_join(joined, la, by = c('fips','model'))

joined[is.na(joined)]<-0
joined$total15 <- joined$dp15+joined$ic15+joined$ir15+joined$th15+joined$la15
joined$total70 <- joined$dp70+joined$ic70+joined$ir70+joined$th70+joined$la70

joined$change <- (joined$total70/joined$total15)-1
joined <- joined %>% group_by(fips) %>% summarise_each(funs(mean, sd))

# mapping
# install.packages(c("choroplethr", "choroplethrMaps")) 

# livestock - aquaculture figures
all.la2$region <- all.la2$fips
all.la2$value <- all.la2$change_mean
county_choropleth(all.la2, num_colors = 0)

all.la2$value <- all.la2$change_sd
county_choropleth(all.la2, num_colors = 0)

# dp figures
dp2$region <- as.double(dp2$fips)
dp2$value <- dp2$change_mean
county_choropleth(dp2, num_colors = 0)

dp2$value <- dp2$change_sd
county_choropleth(dp2, num_colors = 0)

# ic figures
ic2$region <- as.double(ic2$fips)
ic2$value <- ic2$change_mean
ic2$value[is.na(ic2$value)] <- 0
county_choropleth(ic2, num_colors = 0)

ic2$value <- ic2$change_sd
ic2$value[is.na(ic2$value)] <- 0
county_choropleth(ic2, num_colors = 0)

# ir2 figures
ir2$region <- as.double(ir2$fips)
ir2$value <- ir2$change_mean
ir2$value[is.na(ir2$value)] <- 0
county_choropleth(ir2, num_colors = 0)

ir2$value <- ir2$change_sd
ir2$value[is.na(ir2$value)] <- 0
county_choropleth(ir2, num_colors = 0)

# th figures
th2$region <- as.double(th2$fips)
th2$value <- th2$change_mean
th2$value[is.na(th2$value)] <- 0
county_choropleth(th2, num_colors = 0)

th2$value <- th2$change_sd
th2$value[is.na(th2$value)] <- 0
county_choropleth(th2, num_colors = 0)

joined$region <- as.double(joined$fips)
joined$value <- joined$change_mean
county_choropleth(joined, num_colors = 0)

joined$region <- as.double(joined$fips)
joined$value <- joined$change_sd
county_choropleth(joined, num_colors = 0)


#--------------------------------

# # below is just arbitrarily testing which industries have high use
# great50 <- filter(ipsl85ssp5, Y2070 > 50)%>%
#   group_by(sector) 
 
# great50 %>% 
#   group_by(sector) %>%
#   summarise(no_rows = length(sector))

##### Read in subregion data #####

setwd("D:/Demand model/Demand Results - Supplemental for Publication/Subregions")
subregions <- read.csv("Counties_Subregions.csv")%>%
  select(GEOID, RPA_SubReg)
names(subregions) <- c("fips","subregion")

# -----------------------------------------------------------.
##### Current use #####
# first we can calculate the current use by subregion (year 2015)
# just pulling this for one of the datasets since it is the same for everything

# calculating current use - first merge with subregions
current <- merge(cnrm45ssp1, subregions, by = "fips")

current[, 4:59][is.na(current[, 4:59])] <- 0
current$Current <- current$Y2015
current <- current %>%
  group_by(subregion) %>%
  summarise(Current = sum(Current))
names(current)[2] <- "Current"

# -----------------------------------------------------------.
##### 2070 use #####
# second we find total withdrawals by subregion for the year 2070 for each climate model

##### cnrm climate model: #####
cnrm45ssp1 <- merge(cnrm45ssp1, subregions, by = "fips")
cnrm85ssp2 <- merge(cnrm85ssp2, subregions, by = "fips")
cnrm85ssp3 <- merge(cnrm85ssp3, subregions, by = "fips")
cnrm85ssp5 <- merge(cnrm85ssp5, subregions, by = "fips")
# 
cnrm45ssp1[, 4:59][is.na(cnrm45ssp1[, 4:59])] <- 0
cnrm45ssp1$Total <- rowSums(cnrm45ssp1[,c(4:59)])

cnrm45ssp1 <- cnrm45ssp1 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(cnrm45ssp1)[2] <- "cnrm45ssp1"

cnrm85ssp2[, 4:59][is.na(cnrm85ssp2[, 4:59])] <- 0
cnrm85ssp2$Total <- rowSums(cnrm85ssp2[,c(4:59)])
cnrm85ssp2 <- cnrm85ssp2 %>%
   group_by(subregion) %>%
   summarise(Y2070 = sum(Y2070))
 names(cnrm85ssp2)[2] <- "cnrm85ssp2"
 
 # ignore below idk maybe itll come in handy at some point
 # imwest <- cnrm85ssp2 %>%
 #   filter(subregion == "Inter-mountain")#%>%
 #   #summarise(Total = sum(Total))
 # # total 1174495 for southwest
 # # total 1374838 for im west
 # ag <- filter(imwest, sector == "ir")%>%
 #   summarise(Total = sum(Total))
 # # total 1119036 for ag in southwest
 # # total 1269291 for ag in im west
 # 
 # (1269291 / 1374838)*100
 # 

 cnrm85ssp3[, 4:59][is.na(cnrm85ssp3[, 4:59])] <- 0
# cnrm85ssp3$Total <- rowSums(cnrm85ssp3[,c(4:59)])
 cnrm85ssp3 <- cnrm85ssp3 %>%
   group_by(subregion) %>%
   summarise(Y2070 = sum(Y2070))
 names(cnrm85ssp3)[2] <- "cnrm85ssp3"
#
 cnrm85ssp5[, 4:59][is.na(cnrm85ssp5[, 4:59])] <- 0
# cnrm85ssp5$Total <- rowSums(cnrm85ssp5[,c(4:59)])
 cnrm85ssp5 <- cnrm85ssp5 %>%
   group_by(subregion) %>%
   summarise(Y2070 = sum(Y2070))
 names(cnrm85ssp5)[2] <- "cnrm85ssp5"
 
# merge all the data for the climate scenario
cnrm <- merge(cnrm45ssp1, cnrm85ssp2)
cnrm <- merge(cnrm, cnrm85ssp3)
cnrm <- merge(cnrm, cnrm85ssp5)
cnrm <- merge(cnrm, current)
#write.csv(cnrm, file = "C:/ConsumptiveOnly/cnrm_subregions.csv")

# -----------------------------------------------------------.
##### hadgem climate model: #####
hadgem45ssp1 <- merge(hadgem45ssp1, subregions, by = "fips")
hadgem85ssp2 <- merge(hadgem85ssp2, subregions, by = "fips")
hadgem85ssp3 <- merge(hadgem85ssp3, subregions, by = "fips")
hadgem85ssp5 <- merge(hadgem85ssp5, subregions, by = "fips")

hadgem45ssp1[, 4:59][is.na(hadgem45ssp1[, 4:59])] <- 0
#hadgem45ssp1$Total <- rowSums(hadgem45ssp1[,c(4:59)])
hadgem45ssp1 <- hadgem45ssp1 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(hadgem45ssp1)[2] <- "hadgem45ssp1"

hadgem85ssp2[, 4:59][is.na(hadgem85ssp2[, 4:59])] <- 0
#hadgem85ssp2$Total <- rowSums(hadgem85ssp2[,c(4:59)])
hadgem85ssp2 <- hadgem85ssp2 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(hadgem85ssp2)[2] <- "hadgem85ssp2"

hadgem85ssp3[, 4:59][is.na(hadgem85ssp3[, 4:59])] <- 0
#hadgem85ssp3$Total <- rowSums(hadgem85ssp3[,c(4:59)])
hadgem85ssp3 <- hadgem85ssp3 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(hadgem85ssp3)[2] <- "hadgem85ssp3"

hadgem85ssp5[, 4:59][is.na(hadgem85ssp5[, 4:59])] <- 0
#hadgem85ssp5$Total <- rowSums(hadgem85ssp5[,c(4:59)])
hadgem85ssp5 <- hadgem85ssp5 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(hadgem85ssp5)[2] <- "hadgem85ssp5"

# merge all the data for the climate scenario
hadgem <- merge(hadgem45ssp1, hadgem85ssp2)
hadgem <- merge(hadgem, hadgem85ssp3)
hadgem <- merge(hadgem, hadgem85ssp5)
hadgem <- merge(hadgem, current)
#write.csv(hadgem, file = "C:/ConsumptiveOnly/hadgem_subregions.csv")

# -----------------------------------------------------------.
##### ipsl climate model: #####
ipsl45ssp1 <- merge(ipsl45ssp1, subregions, by = "fips")
ipsl85ssp2 <- merge(ipsl85ssp2, subregions, by = "fips")
ipsl85ssp3 <- merge(ipsl85ssp3, subregions, by = "fips")
ipsl85ssp5 <- merge(ipsl85ssp5, subregions, by = "fips")

ipsl45ssp1[, 4:59][is.na(ipsl45ssp1[, 4:59])] <- 0
#ipsl45ssp1$Total <- rowSums(ipsl45ssp1[,c(4:59)])
ipsl45ssp1 <- ipsl45ssp1 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(ipsl45ssp1)[2] <- "ipsl45ssp1"

ipsl85ssp2[, 4:59][is.na(ipsl85ssp2[, 4:59])] <- 0
ipsl85ssp2$Total <- rowSums(ipsl85ssp2[,c(4:59)])
ipsl85ssp2 <- ipsl85ssp2 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(ipsl85ssp2)[2] <- "ipsl85ssp2"

# some random computing ignore below
# southwest <- ipsl85ssp2 %>%
#   filter(subregion == "Pacific Southwest")%>%
# summarise(Total = sum(Total))
# # total 371825.2 for southwest
# ag <- filter(southwest, sector == "ir")%>%
#   summarise(Total = sum(Total))
# # total 325915.2 for ag in southwest

# (325915.2 / 371825.2)*100

ipsl85ssp3[, 4:59][is.na(ipsl85ssp3[, 4:59])] <- 0
#ipsl85ssp3$Total <- rowSums(ipsl85ssp3[,c(4:59)])
ipsl85ssp3 <- ipsl85ssp3 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(ipsl85ssp3)[2] <- "ipsl85ssp3"

ipsl85ssp5[, 4:59][is.na(ipsl85ssp5[, 4:59])] <- 0
#ipsl85ssp5$Total <- rowSums(ipsl85ssp5[,c(4:59)])
ipsl85ssp5 <- ipsl85ssp5 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(ipsl85ssp5)[2] <- "ipsl85ssp5"

# merge all the data for the climate scenario
ipsl <- merge(ipsl45ssp1, ipsl85ssp2)
ipsl <- merge(ipsl, ipsl85ssp3)
ipsl <- merge(ipsl, ipsl85ssp5)
ipsl <- merge(ipsl, current)
#write.csv(ipsl, file = "C:/ConsumptiveOnly/ipsl_subregions.csv")

# -----------------------------------------------------------.
##### mri climate model: #####
mri45ssp1 <- merge(mri45ssp1, subregions, by = "fips")
mri85ssp2 <- merge(mri85ssp2, subregions, by = "fips")
mri85ssp3 <- merge(mri85ssp3, subregions, by = "fips")
mri85ssp5 <- merge(mri85ssp5, subregions, by = "fips")

mri45ssp1[, 4:59][is.na(mri45ssp1[, 4:59])] <- 0
#mri45ssp1$Total <- rowSums(mri45ssp1[,c(4:59)])
mri45ssp1 <- mri45ssp1 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(mri45ssp1)[2] <- "mri45ssp1"

mri85ssp2[, 4:59][is.na(mri85ssp2[, 4:59])] <- 0
#mri85ssp2$Total <- rowSums(mri85ssp2[,c(4:59)])
mri85ssp2 <- mri85ssp2 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(mri85ssp2)[2] <- "mri85ssp2"

mri85ssp3[, 4:59][is.na(mri85ssp3[, 4:59])] <- 0
#mri85ssp3$Total <- rowSums(mri85ssp3[,c(4:59)])
mri85ssp3 <- mri85ssp3 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(mri85ssp3)[2] <- "mri85ssp3"

mri85ssp5[, 4:59][is.na(mri85ssp5[, 4:59])] <- 0
#mri85ssp5$Total <- rowSums(mri85ssp5[,c(4:59)])
mri85ssp5 <- mri85ssp5 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(mri85ssp5)[2] <- "mri85ssp5"

# merge all the data for the climate scenario
mri <- merge(mri45ssp1, mri85ssp2)
mri <- merge(mri, mri85ssp3)
mri <- merge(mri, mri85ssp5)
mri <- merge(mri, current)
#write.csv(mri, file = "C:/ConsumptiveOnly/mri_subregions.csv")

# -----------------------------------------------------------.
##### noresm climate model: #####
noresm45ssp1 <- merge(noresm45ssp1, subregions, by = "fips")
noresm85ssp2 <- merge(noresm85ssp2, subregions, by = "fips")
noresm85ssp3 <- merge(noresm85ssp3, subregions, by = "fips")
noresm85ssp5 <- merge(noresm85ssp5, subregions, by = "fips")

noresm45ssp1[, 4:59][is.na(noresm45ssp1[, 4:59])] <- 0
#noresm45ssp1$Total <- rowSums(noresm45ssp1[,c(4:59)])
noresm45ssp1 <- noresm45ssp1 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(noresm45ssp1)[2] <- "noresm45ssp1"

noresm85ssp2[, 4:59][is.na(noresm85ssp2[, 4:59])] <- 0
#noresm85ssp2$Total <- rowSums(noresm85ssp2[,c(4:59)])
noresm85ssp2 <- noresm85ssp2 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(noresm85ssp2)[2] <- "noresm85ssp2"

noresm85ssp3[, 4:59][is.na(noresm85ssp3[, 4:59])] <- 0
#noresm85ssp3$Total <- rowSums(noresm85ssp3[,c(4:59)])
noresm85ssp3 <- noresm85ssp3 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(noresm85ssp3)[2] <- "noresm85ssp3"

noresm85ssp5[, 4:59][is.na(noresm85ssp5[, 4:59])] <- 0
#noresm85ssp5$Total <- rowSums(noresm85ssp5[,c(4:59)])
noresm85ssp5 <- noresm85ssp5 %>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(noresm85ssp5)[2] <- "noresm85ssp5"

# merge all the data for the climate scenario
noresm <- merge(noresm45ssp1, noresm85ssp2)
noresm <- merge(noresm, cnrm85ssp3)
noresm <- merge(noresm, cnrm85ssp5)
noresm <- merge(noresm, current)
#write.csv(noresm, file = "C:/ConsumptiveOnly/noresm_subregions.csv")



# -----------------------------------------------------------.
# Below can be ignored, it just aggregated data by RCP / SSP instead of climate model

# HH <- merge(cnrm85ssp5, hadgem85ssp5)
# HH <- merge(HH,ipsl85ssp5)
# HH <- merge(HH,mri85ssp5)
# HH <- merge(HH, noresm85ssp5)
# #write.csv(HH, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/Updated/Scenario Totals/Using2070/HH.csv")
# 
# HM <- merge(cnrm85ssp2, hadgem85ssp2)
# HM <- merge(HM,ipsl85ssp2)
# HM <- merge(HM,mri85ssp2)
# HM <- merge(HM, noresm85ssp2)
# #write.csv(HM, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/Updated/Scenario Totals/Using2070/HM.csv")
# 
# HL <- merge(cnrm85ssp3, hadgem85ssp3)
# HL <- merge(HL,ipsl85ssp3)
# HL <- merge(HL,mri85ssp3)
# HL <- merge(HL, noresm85ssp3)
# #write.csv(HL, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/Updated/Scenario Totals/Using2070/HL.csv")
# 
# LM <- merge(cnrm45ssp1, hadgem45ssp1)
# LM <- merge(LM,ipsl45ssp1)
# LM <- merge(LM,mri45ssp1)
# LM <- merge(LM, noresm45ssp1)
# #write.csv(LM, file = "C:/Users/shaun/Desktop/USFS/WaterDemand/WaterDemand/WEAPInputCreation/Updated/Scenario Totals/Using2070/LM.csv")

# ----------------------------------
# Sector calc for consumptive use
# ----------------------------------

# calculating current use - first merge with subregions
dp.sub <- merge(dp, subregions, by = "fips")

dp.sub.mri45.1 <- subset(dp.sub, dp.sub$model == "mri45-1")
dp.sub.mri45.1 <- dp.sub.mri45.1 %>%
  group_by(subregion) %>%
  summarise(
    dp15 = sum(dp15),
    dp70 = sum(dp70))






# ----------------------------------------------------------------.
##### livestock and aquaculture #####


# livestock and aquaculture are being combined into one sector
# these two sectors do not change based on climate - only SSP

livestock1 <- read.csv("./Projections_noresm45/ls_ssp1.csv")
livestock2 <- read.csv("./Projections_noresm85/ls_ssp2.csv")
livestock3 <- read.csv("./Projections_noresm85/ls_ssp3.csv")
livestock5 <- read.csv("./Projections_noresm85/ls_ssp5.csv")

aqua1 <- read.csv("./Projections_noresm45/aq_ssp1.csv")
aqua2 <- read.csv("./Projections_noresm85/aq_ssp2.csv")
aqua3 <- read.csv("./Projections_noresm85/aq_ssp3.csv")
aqua5 <- read.csv("./Projections_noresm85/aq_ssp5.csv")

fips <- select(aqua1, fips)
aqls1 <- aqua1[,4:59] + livestock1[,4:59]
aqls1$sector <- "LsAq"
aqls1 <- aqls1 %>%
  select(sector, everything())
aqls1 <- cbind(fips, aqls1)

aqls2 <- aqua2[,4:59] + livestock2[,4:59]
aqls2$sector <- "LsAq"
aqls2 <- aqls2 %>%
  select(sector, everything())
aqls2 <- cbind(fips, aqls2)

aqls3 <- aqua3[,4:59] + livestock3[,4:59]
aqls3$sector <- "LsAq"
aqls3 <- aqls3 %>%
  select(sector, everything())
aqls3 <- cbind(fips, aqls3)

aqls5 <- aqua5[,4:59] + livestock5[,4:59]
aqls5$sector <- "LsAq"
aqls5 <- aqls5 %>%
  select(sector, everything())
aqls5 <- cbind(fips, aqls5)

aqlsssp1 <- merge(aqls1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(aqlsssp1) <- c("subregion", "Y2015","Y2070ssp1")
aqlsssp2 <- merge(aqls2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp2) <- c("subregion", "Y2070ssp2")
aqlsssp3 <- merge(aqls3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp3) <- c("subregion", "Y2070ssp3")
aqlsssp5 <- merge(aqls5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp5) <- c("subregion", "Y2070ssp5")

aqlstot <- merge(aqlsssp1, aqlsssp2, by = "subregion")
aqlstot <- merge(aqlstot, aqlsssp3, by = "subregion")
aqlstot <- merge(aqlstot, aqlsssp5, by = "subregion")
#write.csv(aqlstot, file = "/Sector Totals/aqLs.csv")



# -----------------------------------------------------------.

##### thermo #####

# noresm
thermo1 <- read.csv("./Projections_Noresm45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_Noresm85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_Noresm85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_Noresm85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_noresm.csv")



# cnrm
thermo1 <- read.csv("./Projections_cnrm45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_cnrm85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_cnrm85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_cnrm85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_cnrm.csv")



# hadgem
thermo1 <- read.csv("./Projections_hadgem45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_hadgem85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_hadgem85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_hadgem85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_hadgem.csv")



# ipsl
thermo1 <- read.csv("./Projections_ipsl45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_ipsl85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_ipsl85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_ipsl85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_ipsl.csv")



# mri
thermo1 <- read.csv("./Projections_mri45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_mri85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_mri85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_mri85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_mri.csv")















# -----------------------------------------------------------.
##### Sector calculations #####
# Below is basically doing the same as above, but not using consumptive data and 
# separated by each sector instead of combining all sectors.

# in this part below, I am pulling from total withdrawal data (not consumptive use)
setwd("C:wherever you saved it/Subregions")

##### domestic #####
# noresm
domestic1 <- read.csv("./Projections_noresm45/dp_ssp1.csv")
domestic2 <- read.csv("./Projections_noresm85/dp_ssp2.csv")
domestic3 <- read.csv("./Projections_noresm85/dp_ssp3.csv")
domestic5 <- read.csv("./Projections_noresm85/dp_ssp5.csv")

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
  names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
  names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
  names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
#write.csv(domestictot, file = "/Sector Totals/domestic_noresm.csv")




# cnrm
domestic1 <- read.csv("./Projections_cnrm45/dp_ssp1.csv")
domestic2 <- read.csv("./Projections_cnrm85/dp_ssp2.csv")
domestic3 <- read.csv("./Projections_cnrm85/dp_ssp3.csv")
domestic5 <- read.csv("./Projections_cnrm85/dp_ssp5.csv")

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
#write.csv(domestictot, file = "/Sector Totals/domestic_cnrm.csv")




# hadgem
domestic1 <- read.csv("./Projections_hadgem45/dp_ssp1.csv")
domestic2 <- read.csv("./Projections_hadgem85/dp_ssp2.csv")
domestic3 <- read.csv("./Projections_hadgem85/dp_ssp3.csv")
domestic5 <- read.csv("./Projections_hadgem85/dp_ssp5.csv")

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
#write.csv(domestictot, file = "/Sector Totals/domestic_hadgem.csv")




# ipsl
domestic1 <- read.csv("./Projections_ipsl45/dp_ssp1.csv")
domestic2 <- read.csv("./Projections_ipsl85/dp_ssp2.csv")
domestic3 <- read.csv("./Projections_ipsl85/dp_ssp3.csv")
domestic5 <- read.csv("./Projections_ipsl85/dp_ssp5.csv")

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
write.csv(domestictot, file = "/Sector Totals/domestic_ipsl.csv")


# mri
domestic1 <- read.csv("./Projections_mri45/dp_ssp1.csv")
domestic2 <- read.csv("./Projections_mri85/dp_ssp2.csv")
domestic3 <- read.csv("./Projections_mri85/dp_ssp3.csv")
domestic5 <- read.csv("./Projections_mri85/dp_ssp5.csv")

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
write.csv(domestictot, file = "/Sector Totals/domestic_mri.csv")


#----------------------------------------------------------------------------------.
##### industrial #####

# using noresm BUT industrial does not change based on any of the climate scenarios

industrial1 <- read.csv("./Projections_noresm45/i_ssp1.csv")
industrial1[, 4:59][is.na(industrial1[, 4:59])] <- 0
industrial2 <- read.csv("./Projections_noresm85/i_ssp2.csv")
industrial2[, 4:59][is.na(industrial2[, 4:59])] <- 0
industrial3 <- read.csv("./Projections_noresm85/i_ssp3.csv")
industrial3[, 4:59][is.na(industrial3[, 4:59])] <- 0
industrial5 <- read.csv("./Projections_noresm85/i_ssp5.csv")
industrial5[, 4:59][is.na(industrial5[, 4:59])] <- 0

industrialssp1 <- merge(industrial1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(industrialssp1) <- c("subregion", "Y2015","Y2070ssp1")
industrialssp2 <- merge(industrial2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(industrialssp2) <- c("subregion", "Y2070ssp2")
industrialssp3 <- merge(industrial3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(industrialssp3) <- c("subregion", "Y2070ssp3")
industrialssp5 <- merge(industrial5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(industrialssp5) <- c("subregion", "Y2070ssp5")

industrialtot <- merge(industrialssp1, industrialssp2, by = "subregion")
industrialtot <- merge(industrialtot, industrialssp3, by = "subregion")
industrialtot <- merge(industrialtot, industrialssp5, by = "subregion")
#write.csv(industrialtot, file = "/Sector Totals/industrial.csv")




# --------------------------------------------------------------------------------------------.

##### irrigation ##### 

# irrigation does not change based on SSP scenario, only climate & RCP

# noresm
irrigation45<- read.csv("./Projections_noresm45/ir.csv")
irrigation85 <- read.csv("./Projections_noresm85/ir.csv")

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_noresm.csv")



# cnrm
irrigation45<- read.csv("./Projections_cnrm45/ir.csv")
irrigation85 <- read.csv("./Projections_cnrm85/ir.csv")

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_cnrm.csv")



# hadgem 
irrigation45<- read.csv("./Projections_hadgem45/ir.csv")
irrigation85 <- read.csv("./Projections_hadgem85/ir.csv")

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_hadgem.csv")



# ipsl 
irrigation45<- read.csv("./Projections_ipsl45/ir.csv")
irrigation85 <- read.csv("./Projections_ipsl85/ir.csv")

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_ipsl.csv")



# mri 
irrigation45<- read.csv("./Projections_mri45/ir.csv")
irrigation85 <- read.csv("./Projections_mri85/ir.csv")

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
# irrigationtot <- merge(irrigationtot, irrigationssp3, by = "subregion")
# irrigationtot <- merge(irrigationtot, irrigationssp5, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_mri.csv")



# ----------------------------------------------------------------.
##### livestock and aquaculture #####


# livestock and aquaculture are being combined into one sector
# these two sectors do not change based on climate - only SSP

livestock1 <- read.csv("./Projections_noresm45/ls_ssp1.csv")
livestock2 <- read.csv("./Projections_noresm85/ls_ssp2.csv")
livestock3 <- read.csv("./Projections_noresm85/ls_ssp3.csv")
livestock5 <- read.csv("./Projections_noresm85/ls_ssp5.csv")

aqua1 <- read.csv("./Projections_noresm45/aq_ssp1.csv")
aqua2 <- read.csv("./Projections_noresm85/aq_ssp2.csv")
aqua3 <- read.csv("./Projections_noresm85/aq_ssp3.csv")
aqua5 <- read.csv("./Projections_noresm85/aq_ssp5.csv")

fips <- select(aqua1, fips)
aqls1 <- aqua1[,4:59] + livestock1[,4:59]
aqls1$sector <- "LsAq"
aqls1 <- aqls1 %>%
  select(sector, everything())
aqls1 <- cbind(fips, aqls1)

aqls2 <- aqua2[,4:59] + livestock2[,4:59]
aqls2$sector <- "LsAq"
aqls2 <- aqls2 %>%
  select(sector, everything())
aqls2 <- cbind(fips, aqls2)

aqls3 <- aqua3[,4:59] + livestock3[,4:59]
aqls3$sector <- "LsAq"
aqls3 <- aqls3 %>%
  select(sector, everything())
aqls3 <- cbind(fips, aqls3)

aqls5 <- aqua5[,4:59] + livestock5[,4:59]
aqls5$sector <- "LsAq"
aqls5 <- aqls5 %>%
  select(sector, everything())
aqls5 <- cbind(fips, aqls5)

aqlsssp1 <- merge(aqls1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(aqlsssp1) <- c("subregion", "Y2015","Y2070ssp1")
aqlsssp2 <- merge(aqls2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp2) <- c("subregion", "Y2070ssp2")
aqlsssp3 <- merge(aqls3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp3) <- c("subregion", "Y2070ssp3")
aqlsssp5 <- merge(aqls5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp5) <- c("subregion", "Y2070ssp5")

aqlstot <- merge(aqlsssp1, aqlsssp2, by = "subregion")
aqlstot <- merge(aqlstot, aqlsssp3, by = "subregion")
aqlstot <- merge(aqlstot, aqlsssp5, by = "subregion")
#write.csv(aqlstot, file = "/Sector Totals/aqLs.csv")



# -----------------------------------------------------------.

##### thermo #####

# noresm
thermo1 <- read.csv("./Projections_Noresm45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_Noresm85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_Noresm85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_Noresm85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_noresm.csv")



# cnrm
thermo1 <- read.csv("./Projections_cnrm45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_cnrm85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_cnrm85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_cnrm85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_cnrm.csv")



# hadgem
thermo1 <- read.csv("./Projections_hadgem45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_hadgem85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_hadgem85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_hadgem85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_hadgem.csv")



# ipsl
thermo1 <- read.csv("./Projections_ipsl45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_ipsl85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_ipsl85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_ipsl85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_ipsl.csv")



# mri
thermo1 <- read.csv("./Projections_mri45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_mri85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_mri85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_mri85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_mri.csv")


### may not beed -- already done above


#### Aggregating consumptive use

# -----------------------------------------------------------.
##### Sector calculations #####
# Below is basically doing the same as above, but not using consumptive data and 
# separated by each sector instead of combining all sectors.

# in this part below, I am pulling from total withdrawal data (not consumptive use)
setwd("C:wherever you saved it/Subregions")

##### domestic #####
# noresm
domestic1 <- read.csv("./Projections_noresm45/dp_ssp1.csv")
domestic2 <- read.csv("./Projections_noresm85/dp_ssp2.csv")
domestic3 <- read.csv("./Projections_noresm85/dp_ssp3.csv")
domestic5 <- read.csv("./Projections_noresm85/dp_ssp5.csv")

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
#write.csv(domestictot, file = "/Sector Totals/domestic_noresm.csv")




# cnrm
domestic1 <- read.csv("./Projections_cnrm45/dp_ssp1.csv")
domestic2 <- read.csv("./Projections_cnrm85/dp_ssp2.csv")
domestic3 <- read.csv("./Projections_cnrm85/dp_ssp3.csv")
domestic5 <- read.csv("./Projections_cnrm85/dp_ssp5.csv")

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
#write.csv(domestictot, file = "/Sector Totals/domestic_cnrm.csv")




# hadgem
domestic1 <- read.csv("./Projections_hadgem45/dp_ssp1.csv")
domestic2 <- read.csv("./Projections_hadgem85/dp_ssp2.csv")
domestic3 <- read.csv("./Projections_hadgem85/dp_ssp3.csv")
domestic5 <- read.csv("./Projections_hadgem85/dp_ssp5.csv")

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
#write.csv(domestictot, file = "/Sector Totals/domestic_hadgem.csv")




# ipsl
domestic1 <- read.csv("./Projections_ipsl45/dp_ssp1.csv")
domestic2 <- read.csv("./Projections_ipsl85/dp_ssp2.csv")
domestic3 <- read.csv("./Projections_ipsl85/dp_ssp3.csv")
domestic5 <- read.csv("./Projections_ipsl85/dp_ssp5.csv")

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
write.csv(domestictot, file = "/Sector Totals/domestic_ipsl.csv")


# mri
domestic1 <- read.csv("./Projections_mri45/dp_ssp1.csv")
domestic2 <- read.csv("./Projections_mri85/dp_ssp2.csv")
domestic3 <- read.csv("./Projections_mri85/dp_ssp3.csv")
domestic5 <- read.csv("./Projections_mri85/dp_ssp5.csv")

domesticssp1 <- merge(domestic1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(domesticssp1) <- c("subregion", "Y2015","Y2070ssp1")
domesticssp2 <- merge(domestic2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp2) <- c("subregion", "Y2070ssp2")
domesticssp3 <- merge(domestic3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp3) <- c("subregion", "Y2070ssp3")
domesticssp5 <- merge(domestic5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(domesticssp5) <- c("subregion", "Y2070ssp5")

domestictot <- merge(domesticssp1, domesticssp2, by = "subregion")
domestictot <- merge(domestictot, domesticssp3, by = "subregion")
domestictot <- merge(domestictot, domesticssp5, by = "subregion")
write.csv(domestictot, file = "/Sector Totals/domestic_mri.csv")


#----------------------------------------------------------------------------------.
##### industrial #####

# using noresm BUT industrial does not change based on any of the climate scenarios

industrial1 <- read.csv("./Projections_noresm45/i_ssp1.csv")
industrial1[, 4:59][is.na(industrial1[, 4:59])] <- 0
industrial2 <- read.csv("./Projections_noresm85/i_ssp2.csv")
industrial2[, 4:59][is.na(industrial2[, 4:59])] <- 0
industrial3 <- read.csv("./Projections_noresm85/i_ssp3.csv")
industrial3[, 4:59][is.na(industrial3[, 4:59])] <- 0
industrial5 <- read.csv("./Projections_noresm85/i_ssp5.csv")
industrial5[, 4:59][is.na(industrial5[, 4:59])] <- 0

industrialssp1 <- merge(industrial1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(industrialssp1) <- c("subregion", "Y2015","Y2070ssp1")
industrialssp2 <- merge(industrial2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(industrialssp2) <- c("subregion", "Y2070ssp2")
industrialssp3 <- merge(industrial3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(industrialssp3) <- c("subregion", "Y2070ssp3")
industrialssp5 <- merge(industrial5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(industrialssp5) <- c("subregion", "Y2070ssp5")

industrialtot <- merge(industrialssp1, industrialssp2, by = "subregion")
industrialtot <- merge(industrialtot, industrialssp3, by = "subregion")
industrialtot <- merge(industrialtot, industrialssp5, by = "subregion")
#write.csv(industrialtot, file = "/Sector Totals/industrial.csv")




# --------------------------------------------------------------------------------------------.

##### irrigation ##### 

# irrigation does not change based on SSP scenario, only climate & RCP

# noresm
irrigation45<- read.csv("./Projections_noresm45/ir.csv")
irrigation85 <- read.csv("./Projections_noresm85/ir.csv")

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_noresm.csv")



# cnrm
irrigation45<- read.csv("./Projections_cnrm45/ir.csv")
irrigation85 <- read.csv("./Projections_cnrm85/ir.csv")

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_cnrm.csv")



# hadgem 
irrigation45<- read.csv("./Projections_hadgem45/ir.csv")
irrigation85 <- read.csv("./Projections_hadgem85/ir.csv")

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_hadgem.csv")



# ipsl 
irrigation45<- read.csv("./Projections_ipsl45/ir.csv")
irrigation85 <- read.csv("./Projections_ipsl85/ir.csv")

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_ipsl.csv")



# mri 
irrigation45<- read.csv("./Projections_mri45/ir.csv")
irrigation85 <- read.csv("./Projections_mri85/ir.csv")

irrigation45 <- merge(irrigation45, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(irrigation45) <- c("subregion", "Y2015","RCP45")
irrigation85 <- merge(irrigation85, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(irrigation85) <- c("subregion", "RCP85")

irrigationtot <- merge(irrigation45, irrigation85, by = "subregion")
# irrigationtot <- merge(irrigationtot, irrigationssp3, by = "subregion")
# irrigationtot <- merge(irrigationtot, irrigationssp5, by = "subregion")
#write.csv(irrigationtot, file = "/Sector Totals/irrigation_mri.csv")



# ----------------------------------------------------------------.
##### livestock and aquaculture #####


# livestock and aquaculture are being combined into one sector
# these two sectors do not change based on climate - only SSP

livestock1 <- read.csv("./Projections_noresm45/ls_ssp1.csv")
livestock2 <- read.csv("./Projections_noresm85/ls_ssp2.csv")
livestock3 <- read.csv("./Projections_noresm85/ls_ssp3.csv")
livestock5 <- read.csv("./Projections_noresm85/ls_ssp5.csv")

aqua1 <- read.csv("./Projections_noresm45/aq_ssp1.csv")
aqua2 <- read.csv("./Projections_noresm85/aq_ssp2.csv")
aqua3 <- read.csv("./Projections_noresm85/aq_ssp3.csv")
aqua5 <- read.csv("./Projections_noresm85/aq_ssp5.csv")

fips <- select(aqua1, fips)
aqls1 <- aqua1[,4:59] + livestock1[,4:59]
aqls1$sector <- "LsAq"
aqls1 <- aqls1 %>%
  select(sector, everything())
aqls1 <- cbind(fips, aqls1)

aqls2 <- aqua2[,4:59] + livestock2[,4:59]
aqls2$sector <- "LsAq"
aqls2 <- aqls2 %>%
  select(sector, everything())
aqls2 <- cbind(fips, aqls2)

aqls3 <- aqua3[,4:59] + livestock3[,4:59]
aqls3$sector <- "LsAq"
aqls3 <- aqls3 %>%
  select(sector, everything())
aqls3 <- cbind(fips, aqls3)

aqls5 <- aqua5[,4:59] + livestock5[,4:59]
aqls5$sector <- "LsAq"
aqls5 <- aqls5 %>%
  select(sector, everything())
aqls5 <- cbind(fips, aqls5)

aqlsssp1 <- merge(aqls1, subregions, by = "fips")%>%
  select(fips,Y2015,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2015 = sum(Y2015),Y2070 = sum(Y2070))
names(aqlsssp1) <- c("subregion", "Y2015","Y2070ssp1")
aqlsssp2 <- merge(aqls2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp2) <- c("subregion", "Y2070ssp2")
aqlsssp3 <- merge(aqls3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp3) <- c("subregion", "Y2070ssp3")
aqlsssp5 <- merge(aqls5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(aqlsssp5) <- c("subregion", "Y2070ssp5")

aqlstot <- merge(aqlsssp1, aqlsssp2, by = "subregion")
aqlstot <- merge(aqlstot, aqlsssp3, by = "subregion")
aqlstot <- merge(aqlstot, aqlsssp5, by = "subregion")
#write.csv(aqlstot, file = "/Sector Totals/aqLs.csv")



# -----------------------------------------------------------.

##### thermo #####

# noresm
thermo1 <- read.csv("./Projections_Noresm45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_Noresm85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_Noresm85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_Noresm85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_noresm.csv")



# cnrm
thermo1 <- read.csv("./Projections_cnrm45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_cnrm85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_cnrm85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_cnrm85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_cnrm.csv")



# hadgem
thermo1 <- read.csv("./Projections_hadgem45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_hadgem85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_hadgem85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_hadgem85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,Y2070,subregion)%>%
  group_by(subregion) %>%
  summarise(Y2070 = sum(Y2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_hadgem.csv")



# ipsl
thermo1 <- read.csv("./Projections_ipsl45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_ipsl85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_ipsl85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_ipsl85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_ipsl.csv")



# mri
thermo1 <- read.csv("./Projections_mri45/th_ssp1.csv")
thermo1[, 4:59][is.na(thermo1[, 4:59])] <- 0
thermo2 <- read.csv("./Projections_mri85/th_ssp2.csv")
thermo2[, 4:59][is.na(thermo2[, 4:59])] <- 0
thermo3 <- read.csv("./Projections_mri85/th_ssp3.csv")
thermo3[, 4:59][is.na(thermo3[, 4:59])] <- 0
thermo5 <- read.csv("./Projections_mri85/th_ssp5.csv")
thermo5[, 4:59][is.na(thermo5[, 4:59])] <- 0

thermossp1 <- merge(thermo1, subregions, by = "fips")%>%
  select(fips,th2015,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2015 = sum(th2015),th2070 = sum(th2070))
names(thermossp1) <- c("subregion", "Y2015","Y2070ssp1")
thermossp2 <- merge(thermo2, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp2) <- c("subregion", "Y2070ssp2")
thermossp3 <- merge(thermo3, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp3) <- c("subregion", "Y2070ssp3")
thermossp5 <- merge(thermo5, subregions, by = "fips")%>%
  select(fips,th2070,subregion)%>%
  group_by(subregion) %>%
  summarise(th2070 = sum(th2070))
names(thermossp5) <- c("subregion", "Y2070ssp5")

thermotot <- merge(thermossp1, thermossp2, by = "subregion")
thermotot <- merge(thermotot, thermossp3, by = "subregion")
thermotot <- merge(thermotot, thermossp5, by = "subregion")
#write.csv(thermotot, file = "/Sector Totals/thermo_mri.csv")


