#' ---
#' title: "02_Indonesia_TCC_CarbonSankey.R"
#' author: "K Tenneson, karistenneson@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' ---
#'
#'
#' Set working directory to where data is being stored.
#+ setwd

#setwd("~/R/GIA/")

#' ### Required packages
#+ Packages
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)

#+ set working directory
# set working directory ------------------------------------------------------
setwd("C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\Final\\CropCommodityAssessment")

#+ inputdata
# Data input and cleaning ------------------------------------------------------
## sampled the FAO ecofloristic data in ArcMap and exported merged csv.
#rawDataFull <- read.csv('data\\EcoFloristicJoin\\CompiledData.csv')
#rawDataFull <- read.csv('data\\EcoFloristicJoin\\CompiledDatawithPeatlands02162020.csv',stringsAsFactors=FALSE)
#rawDataFull <- read.csv('data\\EcoFloristicJoin\\CompiledDatawithPeatlands02242020PhilERROR.csv')
#rawDataFull <- read.csv('data\\EcoFloristicJoin\\CompiledDatawithPeatlands02252020IndoThaiPhilERROR.csv')
#rawDataFull <- read.csv('data\\EcoFloristicJoin\\Compiled_Processed_withSpatial_03122020.csv')
rawDataFull <- read.csv('data\\EcoFloristicJoin\\Compiled_Processed_withSpatial_04042020.csv')

### Update Strata column
rawDataFull = mutate(rawDataFull, countryFactor = as.numeric(PL_COUNTRY))
rawDataFull = mutate(rawDataFull, PL_StratumAll = countryFactor*10+PL_STRATUM)
head(rawDataFull)
sort(unique(rawDataFull$PL_StratumAll))
unique(rawDataFull[c('PL_COUNTRY','countryFactor')])
length(unique(rawDataFull$PL_PLOTID))

################################################################################################
################################################################################################
unique(rawDataFull$EcoZne_Crop)
unique(rawDataFull$Crop_Agro)
unique(rawDataFull$CropStruct[rawDataFull$CropType == 'Crop_support'])
################################################################################################
################################################################################################
## Set up sample strata parameters.
################################################################################################
################################################################################################
#+ user_supplied_variables
# User needs to supply variables -----------------------------------------------
# source the estimation equations.
source("00_GIA_functionsAllCountries.R")

### Area of strata and sample sizes
# 1. Cambodia
stratumAreasCambodia <- c("Strata1 Area" = 875284, "Strata2 Area" = 922311, "Strata3 Area" = 16329763)
# 2. Indonesia
stratumAreasIndo <- c("Strata1 Area" = 8324441, "Strata2 Area" = 	12512668, "Strata3 Area" = 169619791)
# 3. Laos
stratumAreasLaos <- c("Strata1 Area" = 242460, "Strata2 Area" = 1631055, "Strata3 Area" = 21115898)		
# 4. Myanmar
stratumAreasMyanmar <- c("Strata1 Area" = 599379, "Strata2 Area" = 1863779, "Strata3 Area" = 64255959)
# 5. Philippines
stratumAreasPhil <- c("Strata1 Area" = 166007, "Strata2 Area" = 686867, "Strata3 Area" = 29147220)		
# 6. Thailand
stratumAreasThai <- c("Strata1 Area" = 178405, "Strata2 Area" = 1334277, "Strata3 Area" = 49901653)		
# 7. Vietnam
stratumAreasViet <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799, "Strata3 Area" = 31059507)

stratumAreasRegional<-c(stratumAreasCambodia,
                stratumAreasIndo,
                stratumAreasLaos,
                stratumAreasMyanmar,
                stratumAreasPhil,
                stratumAreasThai,
                stratumAreasViet)

### Sample sizes
# 1. Cambodia
sampSizeCambodia <- c(length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 1 & rawDataFull$PL_COUNTRY == "Cambodia"),]$PL_PLOTID)),
                      length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 2 & rawDataFull$PL_COUNTRY == "Cambodia"),]$PL_PLOTID)),
                      length(rawDataFull[which(rawDataFull$PL_STRATUM == 3 & rawDataFull$PL_COUNTRY == "Cambodia"),]$PL_PLOTID)/24)
# 2. Indonesia
sampSizeIndo <- c(length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 1 & rawDataFull$PL_COUNTRY == "Indonesia"),]$PL_PLOTID)),
                  length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 2 & rawDataFull$PL_COUNTRY == "Indonesia"),]$PL_PLOTID)),
                  length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 3 & rawDataFull$PL_COUNTRY == "Indonesia"),]$PL_PLOTID)))
# 3. Laos
sampSizeLaos <- c(length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 1 & rawDataFull$PL_COUNTRY == "Laos"),]$PL_PLOTID)),
                  length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 2 & rawDataFull$PL_COUNTRY == "Laos"),]$PL_PLOTID)),
                  length(rawDataFull[which(rawDataFull$PL_STRATUM == 3 & rawDataFull$PL_COUNTRY == "Laos"),]$PL_PLOTID)/24)
# 4. Myanmar
sampSizeMyanmar <- c(length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 1 & rawDataFull$PL_COUNTRY == "Myanmar"),]$PL_PLOTID)),
                     length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 2 & rawDataFull$PL_COUNTRY == "Myanmar"),]$PL_PLOTID)),
                     length(rawDataFull[which(rawDataFull$PL_STRATUM == 3 & rawDataFull$PL_COUNTRY == "Myanmar"),]$PL_PLOTID)/24)
# 5. Philippines
sampSizePhil <- c(length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 1 & rawDataFull$PL_COUNTRY == "Philippines"),]$PL_PLOTID)),
                  length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 2 & rawDataFull$PL_COUNTRY == "Philippines"),]$PL_PLOTID)),
                  length(rawDataFull[which(rawDataFull$PL_STRATUM == 3 & rawDataFull$PL_COUNTRY == "Philippines"),]$PL_PLOTID)/24)
# 6. Thailand
sampSizeThai <- c(length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 1 & rawDataFull$PL_COUNTRY == "Thailand"),]$PL_PLOTID)),
                  length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 2 & rawDataFull$PL_COUNTRY == "Thailand"),]$PL_PLOTID)),
                  length(rawDataFull[which(rawDataFull$PL_STRATUM == 3 & rawDataFull$PL_COUNTRY == "Thailand"),]$PL_PLOTID)/24)
# 7. Vietnam
sampSizeViet <- c(length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 1 & rawDataFull$PL_COUNTRY == "Vietnam"),]$PL_PLOTID)),
                  length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 2 & rawDataFull$PL_COUNTRY == "Vietnam"),]$PL_PLOTID)),
                  length(rawDataFull[which(rawDataFull$PL_STRATUM == 3 & rawDataFull$PL_COUNTRY == "Vietnam"),]$PL_PLOTID)/24)

sampSizeRegional<-c(sampSizeCambodia,
            sampSizeIndo,
            sampSizeLaos,
            sampSizeMyanmar,
            sampSizePhil,
            sampSizeThai,
            sampSizeViet)

### Metadata to save, and Grouping Variables for regional analysis.
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_COUNTRY", "PL_StratumAll")
groupList <- c("PL_COUNTRY", "PL_StratumAll", "PL_PLOTID")#, 'Smpl_Strat')
unique(rawDataFull$PL_StratumAll)

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

### Set up filters by country of interest.
stratumAreas<-stratumAreasIndo
stratumAreas
sampSize<-sampSizeIndo
sampSize

# make a copy of full data set before subsetting.
rawData <- rawDataFull
unique(rawData$PL_COUNTRY)
country <- 'Regional'
country<-'Indonesia'

# change strata for country analysis.
strata <- seq(1:21)
rawData$PL_StratumAll<-rawData$PL_STRATUM
strata <- seq(1:3)

unique(rawData$PL_StratumAll)
unique(rawData$PL_STRATUM)

###################################################################################
###################################################################################

### Filter data by country.
rawData$PL_COUNTRY<-as.character(rawData$PL_COUNTRY)
unique(rawData$PL_COUNTRY)
rawData<-rawData[rawData$PL_COUNTRY==country,]
unique(rawData$PL_COUNTRY)
head(rawData)

#+ Analysis
# Analysis ---------------------------------------------------------------------
###########################################################################################
###########################################################################################
## simple tables
###########################################################################################
###########################################################################################
# table = a cleaned table of CEO point data.

# mtdt = a vector of the names of the metadata fields to be included
# strata = a vector of the strata names
# areas = a vector of areas of the strata
# ns = a vector of the strata sample sizes
# qstns = a vector of the names of the CEO questions
# grplst = a vector of the names of the grouping variables
######################################################################################################################
######################################################################################################################
## statistics for Fig 2 and 3.
unique(rawData$Forest_Conv)
unique(rawData$CropStruct)
unique(rawData$EcoZne_Crop)
unique(rawData$Crop_Agro)
unique(rawData$EcoZne_CropAgro)

rawData$Forest_Conv <- as.character(rawData$Forest_Conv)
rawData$CropStruct <- as.character(rawData$CropStruct)
rawData$EcoZne_Crop<-as.character(rawData$EcoZne_Crop)
rawData$Crop_Agro<-as.character(rawData$Crop_Agro)
rawData$EcoZne_CropAgro <- as.character(rawData$EcoZne_CropAgro)
rawData<- as.tibble(rawData)

###########################################################################################
###########################################################################################
# Survey Questions
questions <-  c('Forest_Conv', "CropStruct", 'EcoZne_Crop', 'Crop_Agro', 'EcoZne_CropAgro')
#                "LAND_COVER", "UNDERSTORY", "LAND_USE", "LAND_USE_1", "TreeCover_2000", 
#                "ForestDynamics", "LossType", "CropType",  'AgroSystem')

qstn <- headerOfInterest<-'EcoZne_Crop'

#Step 1 - Build questions table
qTable <- build_question(rawData, mtdt = metaNames, qstn = headerOfInterest)

#Step 2 - Build analysis tables for each question
qcol <- ncol(qTable) # column with question label
answers <- sort(unique(pull(qTable[,qcol]))) # response classes
aTable <- build_yc(qTable, lblfld = qstn, 
                   cmmdtylst = answers)
#Step 3 - Build plot tables
pTable <-  plot_means(aTable, grplst = groupList, 
                      cmmdtylst = answers)

#Step 4 - Build stratum tables, first means then SE
sTable <- stratum_means(aTable, grplst = groupList, 
                        cmmdtylst = answers)

seTable <- stratum_SE(aTable, grplst = groupList, 
                      cmmdtylst = answers)

#Step 5 - Analyze plot tables to produce question overall P_hat_c and V_hat_C
p_hat_sub_c <- overall_prop(sTable, areas =  stratumAreas)
v_hat_sub_c <- overall_SE(pTable, strata, areas =  stratumAreas, ns= sampSize)


#Step 6 - Gather results and output
cover <- rbind(p_hat_sub_c, v_hat_sub_c)
rownames(cover) <-  c("PercentCover", "SE")

coverArea<- t(cover)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

#coverArea <- round(signif(coverArea, digits = 5))
coverArea <- round(coverArea)
coverArea
country
unique(rawData$PL_COUNTRY)
write.csv(coverArea, paste0('Results/Fig3_', country, 'ForesttoCrop.csv'))
remove(coverArea); remove(pTable); remove(qTable); remove(cover)

##############################################################
## statistic 3b
MyQuestion <- 'Crop_Agro'
Crop <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                       ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                       grplst = groupList)

coverArea<- t(Crop$Cover$Crop_Agro)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

#coverArea2 <- round(signif(coverArea2, digits = 5))
coverArea <- round(coverArea)
coverArea
country
unique(rawData$PL_COUNTRY)
write.csv(coverArea, paste0('Results\\Fig3_', country, 'CroptoAgro.csv'))
remove(coverArea); remove(Crop);

##############################################################
## statistic 2a
MyQuestion <- 'Forest_Conv'
Crop <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                       ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                       grplst = groupList)

coverArea<- t(Crop$Cover$Forest_Conv)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

#coverArea2 <- round(signif(coverArea2, digits = 5))
coverArea <- round(coverArea)
coverArea
country
unique(rawData$PL_COUNTRY)
write.csv(coverArea, paste0('Results/Fig2_', country, 'ForesttoOtherAreas.csv'))
remove(coverArea); remove(Crop);

##############################################################
## statistic 2b
MyQuestion <- 'CropStruct'
Crop <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                       ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                       grplst = groupList)

coverArea<- t(Crop$Cover$CropStruct)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

#coverArea2 <- round(signif(coverArea2, digits = 5))
coverArea <- round(coverArea)
coverArea
country
unique(rawData$PL_COUNTRY)
write.csv(coverArea, paste0('Results\\Fig2_', country, 'CropStructure.csv'))
remove(coverArea); remove(Crop)

######################################################################################################################
######################################################################################################################
## statistics for Carbon tables.
###########################################################################################
# Survey Questions

qstn <- headerOfInterest<-'EcoZne_CropAgro'

#Step 1 - Build questions table
qTable <- build_question(rawData, mtdt = metaNames, qstn = headerOfInterest)

#Step 2 - Build analysis tables for each question
qcol <- ncol(qTable) # column with question label
answers <- sort(unique(pull(qTable[,qcol]))) # response classes
aTable <- build_yc(qTable, lblfld = qstn, 
                   cmmdtylst = answers)
#Step 3 - Build plot tables
pTable <-  plot_means(aTable, grplst = groupList, 
                      cmmdtylst = answers)

#Step 4 - Build stratum tables, first means then SE
sTable <- stratum_means(aTable, grplst = groupList, 
                        cmmdtylst = answers)

seTable <- stratum_SE(aTable, grplst = groupList, 
                      cmmdtylst = answers)

#Step 5 - Analyze plot tables to produce question overall P_hat_c and V_hat_C
p_hat_sub_c <- overall_prop(sTable, areas =  stratumAreas)
v_hat_sub_c <- overall_SE(pTable, strata, areas =  stratumAreas, ns= sampSize)


#Step 6 - Gather results and output
cover <- rbind(p_hat_sub_c, v_hat_sub_c)
rownames(cover) <-  c("PercentCover", "SE")

coverArea<- t(cover)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

#coverArea <- round(signif(coverArea, digits = 5))
coverArea <- round(coverArea)
coverArea
country
unique(rawData$PL_COUNTRY)
write.csv(coverArea, paste0('Results/Fig4_', country, 'CarbonForesttoCropAgro.csv'))
remove(coverArea); remove(pTable); remove(qTable); remove(rawData)

######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################

