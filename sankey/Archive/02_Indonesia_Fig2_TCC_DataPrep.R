#' ---
#' title: "02_Indonesia_TCC_CarbonSankey.R"
#' author: "K Tenneson, karistenneson@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' ---
#'
#'
#' Set working directory to where data is being stored.
#+ setwd

setwd("C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\GIA\\GIA")

#' ### Required packages
#+ Packages
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)

#+ inputdata
# Data input and cleaning ------------------------------------------------------

## sampled the FAO ecofloristic data in ArcMap and exported merged csv.
#rawDataNoPeatlands <- read.csv('data\\IndonesiaIntermediateFiles\\CleanedEcoFloristic.csv')
rawDataOrig <- read.csv('data\\IndonesiaIntermediateFiles\\CleanedEcoFloristicPeatlands.csv')
colnames(rawDataOrig)

# remove noisy plots.
rawDataOrig <- rawDataOrig[rawDataOrig$FLAGGED == 'FALSE',]

# subset columns.
rawData <- rawDataOrig[,c("PLOT_ID", "PL_ORIGID", "PL_PLOTID", "SAMPLE_ID", "LAT", "LON", 
                          "PL_COUNTRY", "PL_PACKETI", "PL_STRATUM",
                          "USER_ID", "IMAGERY_TI", "LAND_COVER", "UNDERSTORY", "LAND_USE","LAND_USE_Y",
                          "code", "Peatlands")]

############################################################################
## add in columns that aggregate data to use in analysis.

## 1. update ecofloristic zone to include only tree canopy cover
unique(rawData$code)
rawData$Ecofloristic[rawData$LAND_USE_Y == 'Other'] <- 'Other'
rawData$Ecofloristic[rawData$code == 817 & rawData$LAND_USE_Y != 'Other'] <- 'Tropical moist deciduous forest'
rawData$Ecofloristic[rawData$code == 818 & rawData$LAND_USE_Y != 'Other'] <- 'Tropical mountain system'
rawData$Ecofloristic[rawData$code == 819 & rawData$LAND_USE_Y != 'Other'] <- 'Tropical rainforest'
rawData$Ecofloristic[rawData$code == 820 & rawData$LAND_USE_Y != 'Other'] <- 'Tropical shrubland'
unique(rawData$Ecofloristic)

## 1.5 update ecofloristic zone and peatlands where there was tree canopy cover
rawData$EcoflorPeat<-rawData$Ecofloristic
# 'Tropical Rainforest'
rainforest <- 'Rainforest'
rawData$EcoflorPeat[rawData$Ecofloristic == 'Tropical rainforest']<-substr(rainforest, 1,7) 
rawData$EcoflorPeat[rawData$Ecofloristic == 'Tropical rainforest' 
                    & rawData$Peatlands == 'Peatlands']<-paste0(substr(rainforest, 1,5), 'Pt') # 'Peat'

# 'Tropical moist deciduous forest
Decid<-'TrDeciduous' 
rawData$EcoflorPeat[rawData$Ecofloristic == 'Tropical moist deciduous forest']<-substr(Decid, 1,7)
rawData$EcoflorPeat[rawData$Ecofloristic == 'Tropical moist deciduous forest' 
                    & rawData$Peatlands == 'Peatlands']<-paste0(substr(Decid, 1,5), 'Pt') # 'Peat'

# 'Tropical shrubland'
Shrub<-'TrShrub'
rawData$EcoflorPeat[rawData$Ecofloristic == 'Tropical shrubland']<- substr(Shrub, 1,7)
rawData$EcoflorPeat[rawData$Ecofloristic == 'Tropical shrubland' 
                    & rawData$Peatlands == 'Peatlands']<-paste0(substr(Shrub, 1,5), 'Pt') # 'Peat'

# 'Tropical mountain system'
Mountain <- 'TrMountain' 
rawData$EcoflorPeat[rawData$Ecofloristic == 'Tropical mountain system']<-substr(Mountain, 1,7)
rawData$EcoflorPeat[rawData$Ecofloristic == 'Tropical mountain system' 
                    & rawData$Peatlands == 'Peatlands']<-paste0(substr(Mountain, 1,5), 'Pt') # 'Peat'

## 2. Tree canopy cover in 2000?
rawData$TreeCover_2000<-'Tree cover'
rawData$TreeCover_2000[rawData$LAND_USE_Y == 'Other'] <- 'Not tree'
unique(rawData$TreeCover_2000)

## 3. Forest, tree canopy cover dynamics, or other land use/cover
rawData$ForestDynamics[rawData$TreeCover_2000 == 'Tree cover' & rawData$LAND_USE == 'Natural Forest'] <- 'Forest'
rawData$ForestDynamics[rawData$TreeCover_2000 == 'Tree cover' & rawData$LAND_USE != 'Natural Forest'] <- 'TCCdynamics'
rawData$ForestDynamics[rawData$LAND_USE_Y == 'Other'] <- 'Not tree'
unique(rawData$ForestDynamics)

## 4. Identify tcc transition types
rawData$LossType[rawData$LAND_USE_Y == 'Other'] <- 'Not target'
rawData$LossType[rawData$ForestDynamics == 'Forest'] <- rawData$Ecofloristic[rawData$ForestDynamics == 'Forest']
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & 
                   (rawData$LAND_COVER == 'Water' | rawData$LAND_COVER == 'Non-vegetated' | rawData$LAND_COVER == 'Other')
                 ] <- 'TCC_to_Other'
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & rawData$LAND_COVER == 'Built-up'] <- 'TCC_to_BuiltUp'
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & 
                   (rawData$LAND_COVER == 'Other Shrub' | rawData$LAND_COVER == 'Herbaceous' | 
                      rawData$LAND_COVER == 'Bamboo' |rawData$LAND_COVER == 'Other Tree')] <- 'TCC_to_Veg'
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & rawData$LAND_USE == 'Silvopastoral'] <- 'TCC_to_Silvopastoral'
# Ag by crop cover
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' &  
                   (rawData$LAND_COVER == 'Other Crop' | rawData$LAND_COVER == 'Rice' | 
                      rawData$LAND_COVER == 'Aquaculture' | rawData$LAND_COVER == 'Oil Palm')
                 ]<- 'TCC_to_Agriculture'
# Ag by land use
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' &
                   (rawData$LAND_USE == 'Plantation' | rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture'
                    | rawData$LAND_USE == 'Terrace' | rawData$LAND_USE == 'Boundary Agrisilviculture')]<- 'TCC_to_Agriculture'

## 5. Identify crop types
rawData$CropType <- 'None' 
rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Oil Palm']<- 'Oil_palm'
rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Coconut']<- 'Coconut_crop'
rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other Palm']<- 'Palm_crop'

rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Pulpwood']<- 'Pulpwood_crop'
rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Rubber']<- 'Rubber_crop'
rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Fruit/Nut']<- 'Fruit_nut_crop'
rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other Tree']<- 'Tree_crop'

rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Coffee']<- 'Coffee_crop'
rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other Shrub']<- 'Shrub_crop'

rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Rice']<- 'Rice_crop'
rawData$CropType[rawData$LossType == 'TCC_to_Agriculture' & 
                   (rawData$LAND_COVER == 'Other Crop' | rawData$LAND_COVER == 'Herbaceous')]<- 'Herb_crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Aquaculture')]<- 'Aquaculture'

rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & 
                    (rawData$LAND_COVER == 'Herbaceous' | rawData$LAND_COVER == 'Other' | rawData$LAND_COVER == 'Non-vegetated' | 
                       rawData$LAND_COVER == 'Water' | rawData$LAND_COVER == 'Built-up') &
                    (rawData$LAND_USE == 'Mixed Agrisilviculture'| rawData$LAND_USE == 'Agrisiviculture'| 
                       rawData$LAND_USE == 'Boundary Agrisilviculture' | rawData$LAND_USE == 'Plantation'))]<- 'Crop_support'

## 6. Identify crop Structure
rawData$CropStructure <- rawData$CropType 
rawData$CropStructure[rawData$CropType == 'Oil_Palm' | rawData$CropType == 'Palm_Crop' | rawData$CropType == 'Coconut_Crop'] <-'All_palm_crops'
rawData$CropStructure[rawData$CropType == 'Pulpwood_Crop' | rawData$CropType == 'Rubber_Crop' | 
                        rawData$CropType == 'Tree_Crop' | rawData$CropType == 'Rubber_Crop' | 
                        rawData$CropType == 'Fruit_Nut_Crop'] <-'All_tree_crops'
rawData$CropStructure[rawData$CropType == 'Coffee_crop' | rawData$CropType == 'Shrub_crop'] <-'All_Shrub_Crops'
rawData$CropStructure[rawData$CropType == 'Rice_crop' | rawData$CropType == 'Aquaculture' | rawData$CropType == 'Herb_crop'] <-'All_herbaceous_crops'
rawData$CropStructure[rawData$CropType == 'Crop_support'] <-'Crop_support'

## 7.
rawData$AgroSystem<-'None'
rawData$AgroSystem[rawData$LossType=='TCC_to_Agriculture']<-'Trad'
rawData$AgroSystem[rawData$LossType=='TCC_to_Agriculture' & rawData$LAND_USE == 'Agrisilviculture']<-'Agrisil'
rawData$AgroSystem[rawData$LossType=='TCC_to_Agriculture' & rawData$LAND_USE == 'Mixed Agrisilviculture']<-'Agrisil'
rawData$AgroSystem[rawData$LossType=='TCC_to_Agriculture' & rawData$LAND_USE == 'Terrace']<-'Trad'
rawData$AgroSystem[rawData$LossType=='TCC_to_Agriculture' & rawData$LAND_USE == 'Boundary Agrisilviculture']<-'Bndry'
unique(rawData$AgroSystem)

##################################
## Concatenate some columns
##################################
rawData$EcoflorPeat<-as.character(rawData$EcoflorPeat)
rawData$CropType<-as.character(rawData$CropType)
rawData$AgroSystem<-as.character(rawData$AgroSystem)

## 8. 
rawData<-unite(rawData, 'Peat_Conv', c("EcoflorPeat","LossType"), remove = FALSE)
unique(rawData$Peat_Conv)

## 9. 
rawData<-unite(rawData, 'Peat_Crop', c("EcoflorPeat","CropType"), remove = FALSE)
unique(rawData$Peat_Crop)

## 10. 
rawData$CropType <- substr(rawData$CropType, 1,7)
unique(rawData$CropType)
rawData<-unite(rawData, 'Crop_Agro', c("CropType","AgroSystem"), remove = FALSE)
unique(rawData$Crop_Agro)

################################################################################################
################################################################################################
## Begin analysis
################################################################################################
################################################################################################
source("00_GIA_functions.R")

rawData<- as.tibble(rawData)
# Metadata to save, and Grouping Variables
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETI",         
               "PL_COUNTRY", "PL_STRATUM")
groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

#+ user_supplied_variables
# User needs to supply variables -----------------------------------------------

# Area of strata and sample sizes
strata <- c(1, 2, 3)
stratumAreas <- c("Strata1 Area" = 8324441, "Strata2 Area" = 	12512668, 
                  "Strata3 Area" = 169619791)

#Determine actual sample size
sampSize <- c(length(unique(rawData[which(rawData$PL_STRATUM == 1),]$PL_PLOTID)),
              length(unique(rawData[which(rawData$PL_STRATUM == 2),]$PL_PLOTID)),
              length(unique(rawData[which(rawData$PL_STRATUM == 3),]$PL_PLOTID))
              )

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

# Survey Questions
questions <-  c("LAND_COVER", "UNDERSTORY",
                "LAND_USE", "LAND_USE_Y", 
                "TreeCover_2000", "ForestDynamics", "LossType", "CropType", "Structure", 'Peat_Conv', 'Peat_Crop')
######################################################################################################################
######################################################################################################################
## statistics for Fig 2.

qstn <- headerOfInterest<-'Peat_Conv'

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
write.csv(coverArea, 'Results/Fig2_IndopeattoOtherAreas.csv')

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
write.csv(coverArea, 'Results\\Fig2_IndoCropStructure.csv')

