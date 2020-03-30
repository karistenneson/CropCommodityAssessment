#' ---
#' title: "02_Indonesia_TCC_CarbonSankey.R"
#' author: "K Tenneson, karistenneson@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' ---
#'
#'
#' Set working directory to where data is being stored.
#+ setwd

setwd("C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\GIA")

#' ### Required packages
#+ Packages
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)

#+ inputdata
# Data input and cleaning ------------------------------------------------------

rawDataFull <- read.csv('data\\EcoFloristicJoin\\CompiledData.csv')

unique(rawDataFull$CropStructure)
unique(rawDataFull$CropStructure[rawDataFull$CropType == 'Crop_support'])
################################################################################################
################################################################################################
## Begin analysis
################################################################################################
################################################################################################
#+ user_supplied_variables
# User needs to supply variables -----------------------------------------------
# source the estimation equations.
source("00_GIA_functions.R")
strata <- c(1, 2, 3)

# make a copy of full data set before subsetting.
rawDataFull <- rawData

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


### Sample sizes
# 1. Cambodia
sampSizeCambodia <- c(length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 1 & rawDataFull$PL_COUNTRY == "Cambodia"),]$PL_PLOTID)),
              length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 2 & rawDataFull$PL_COUNTRY == "Cambodia"),]$PL_PLOTID)),
              length(rawDataFull[which(rawDataFull$PL_STRATUM == 3 & rawDataFull$PL_COUNTRY == "Cambodia"),]$PL_PLOTID)/24)
# 2. Indonesia
sampSizeIndo <- c(length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 1 & rawDataFull$PL_COUNTRY == "Indonesia"),]$PL_PLOTID)),
              length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 2 & rawDataFull$PL_COUNTRY == "Indonesia"),]$PL_PLOTID)),
              length(unique(rawDataFull[which(rawDataFull$PL_STRATUM == 3 & rawDataFull$PL_COUNTRY == "Indonesia"),]$PL_PLOTID))
)
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

### Metadata to save, and Grouping Variables
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_COUNTRY", "PL_STRATUM")

groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

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
## statistics for Fig 2.

unique(rawDataFull$PL_COUNTRY)
country<-"Vietnam"
stratumAreas <- stratumAreasViet
sampSize <- sampSizeViet

# subset the data by the country of interest.
rawData <- rawDataFull[rawDataFull$PL_COUNTRY == country,]
rawData$Forest_Conv <- as.character(rawData$Forest_Conv)
rawData$CropStructure <- as.character(rawData$CropStructure)
rawData<- as.tibble(rawData)

###########################################################################################
###########################################################################################
# Survey Questions
questions <-  c("LAND_COVER", "UNDERSTORY", "LAND_USE", "LAND_USE_1", 
                "TreeCover_2000", "ForestDynamics", "LossType", "CropType", "CropStructure", 
                'Crop_Agro', 'AgroSystem', 'EcoZne_Crop', 'Forest_Conv')

qstn <- headerOfInterest<-'Forest_Conv'

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

coverArea <- round(signif(coverArea, digits = 5))
coverArea
write.csv(coverArea, paste0('Results/Fig2_', country, 'ForesttoOtherAreas.csv'))

##############################################################
## statistic 2b
MyQuestion <- 'CropStructure'
Crop <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                       ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                       grplst = groupList)

coverArea<- t(Crop$Cover$CropStructure)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

coverArea <- round(signif(coverArea, digits = 5))
coverArea
write.csv(coverArea, paste0('Results\\Fig2_', country, 'CropStructure.csv'))


