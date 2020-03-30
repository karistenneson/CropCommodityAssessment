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

#+ inputdata
# Data input and cleaning ------------------------------------------------------
## sampled the FAO ecofloristic data in ArcMap and exported merged csv.
setwd("C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\GIA\\GIA")
rawDataFull <- read.csv('data\\EcoFloristicJoin\\CompiledData.csv')

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
stratumAreasPhil <- c("Strata1 Area" = 166007, "Strata2 Area" = 686867, "Strata3 Area" = 11147127)		
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
## statistics for Fig 3.

unique(rawDataFull$PL_COUNTRY)
country<-"Cambodia"
stratumAreas <- stratumAreasCambodia
sampSize <- sampSizeCambodia

# subset the data by the country of interest.
rawData <- rawDataFull[rawDataFull$PL_COUNTRY == country,]
rawData$EcoZne_Crop<-as.character(rawData$EcoZne_Crop)
rawData$Crop_Agro<-as.character(rawData$Crop_Agro)
rawData<- as.tibble(rawData)

###########################################################################################
###########################################################################################
# Survey Questions
questions <-  c("LAND_COVER", "UNDERSTORY", "LAND_USE", "LAND_USE_1", 
                "TreeCover_2000", "ForestDynamics", "LossType", "CropType", "CropStructure", 
                'Crop_Agro', 'AgroSystem', 'EcoZne_Crop', 'Forest_Conv')

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
write.csv(coverArea, paste0('Results/Fig3_', country, 'ForesttoCrop.csv'))

##############################################################
## statistic 3b
MyQuestion <- 'Crop_Agro'
Crop <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                       ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                       grplst = groupList)

coverArea2<- t(Crop$Cover$Crop_Agro)*sum(stratumAreas)
colnames(coverArea2) <- c("Area_ha", "SE_Area_ha")

#coverArea2 <- round(signif(coverArea2, digits = 5))
coverArea2 <- round(coverArea2)
coverArea2
write.csv(coverArea2, paste0('Results\\Fig3_', country, 'CroptoAgro.csv'))

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
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
## statistic 8

qstn <- headerOfInterest<-'Crop_to_Agro'

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

# top level area tables
captionQ1 <- c("Table X. text.")
kable(coverArea, digits = 0, caption = captionQ1)
write.csv(coverArea, 'Results/IndoCropAgroAreas.csv')

######################################################################################################################
######################################################################################################################
## statistic 1
MyQuestion <- 'TreeCover_2000'
cover <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                                 ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                                 grplst = groupList)

coverArea<- t(cover$Cover$TreeCover_2000)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

# top level area tables
captionQ1 <- c("Table X. text.")
kable(coverArea, digits = 0, caption = captionQ1)
write.csv(coverArea, 'Results\\IndoLossT1.csv')

######################################################################################################################
######################################################################################################################
## statistic 2
MyQuestion <- 'ForestDynamics'
Dynamics <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                                 ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                                 grplst = groupList)

coverArea<- t(Dynamics$Cover$ForestDynamics)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

# top level area tables
captionQ1 <- c("Table X. text.")
kable(coverArea, digits = 0, caption = captionQ1)
write.csv(coverArea, 'Results\\IndoLossT2.csv')

######################################################################################################################
######################################################################################################################
## statistic 3
MyQuestion <- 'LossType'
Cause <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                           ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                           grplst = groupList)

coverArea<- t(Cause$Cover$LossType)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

# top level area tables
captionQ1 <- c("Table X. text.")
kable(coverArea, digits = 0, caption = captionQ1)
write.csv(coverArea, 'Results\\IndoCause.csv')

######################################################################################################################
######################################################################################################################
## statistic 4
MyQuestion <- 'Eco_to_Other'
toCause <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                        ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                        grplst = groupList)

coverArea<- t(toCause$Cover$Eco_to_Other)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

# top level area tables
captionQ1 <- c("Table X. text.")
kable(coverArea, digits = 0, caption = captionQ1)
write.csv(coverArea, 'Results\\IndotoCause.csv')

######################################################################################################################
######################################################################################################################
## statistic 5
MyQuestion <- 'CropType'
Crop <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                        ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                        grplst = groupList)

coverArea<- t(Crop$Cover$CropType)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

# top level area tables
captionQ1 <- c("Table X. text.")
kable(coverArea, digits = 0, caption = captionQ1)
write.csv(coverArea, 'Results\\IndoCropComposition.csv')

######################################################################################################################
######################################################################################################################
## statistic 7
MyQuestion <- 'Eco_to_Crop'
Crop2 <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                       ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                       grplst = groupList)

coverArea<- t(Crop2$Cover$Eco_to_Crop)*sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

# top level area tables
captionQ1 <- c("Table X. text.")
kable(coverArea, digits = 0, caption = captionQ1)
write.csv(coverArea, 'Results\\IndoEcoCropType.csv')


######################################################################################################################
######################################################################################################################
## OLD CODE
######################################################################################################################
######################################################################################################################
qstn <- headerOfInterest<-'CropType'


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

# top level area tables
captionQ1 <- c("Table X. text.")
kable(coverArea, digits = 0, caption = captionQ1)
write.csv(coverArea, 'Results/IndoCropConversionAreas.csv')

#+ y_occ
# Former Forest only -----------------------------------------------------------
#' Triple conditional for area of commodities that were in year 2000 
#' natural forest areas

questions2 <- c("LAND_COVER", "LAND_USE", "LAND_USE_YEAR_2000")

crops <- c("Coconut", "Coffee", "Fruit_Nut" ,"Pulpwood", "Rice", "Rubber", 
           "Oil_Palm", "Tea", "Other_Crop")

y_occ <- build_yocc(rawData, mtdt = metaNames, qstns = questions2, 
                     cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                     cndtnfld2 = "LAND_USE_YEAR_2000", covers = crops, 
                     conditions1 = NULL, conditions2 = "Natural_Forest")


y_occResults <- do_yocc_analysis(y_occ, mtdt = metaNames, strata = strata, 
                                 areas = stratumAreas, ns = sampSize, 
                                 qstns = questions2, grplst = groupList)


# calculate areas
cResults <- bind_rows(y_occResults$Cover)

cResults %>% 
  select(cover, condition1, PercentCover) %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas)) %>% 
  spread(cover, PercentCover) %>% 
  kable(., digits = 0, col.names = c("Land Use", sort(crops)), 
        caption = c("Table X. Total area of each commodity crop, by land use, in areas 
              that were forested in the year 2000."))

cResults %>% 
  select(cover, condition1, SE) %>% 
  mutate(SE = SE * sum(stratumAreas)) %>% 
  spread(cover,SE) %>% 
  kable(., digits = 0, col.names = c("Land Use", sort(crops)), 
        caption = c("Table X. Standard error of the area of each commodity crop, by 
        land use, in areas that were forested in the year 2000."))


#+ y_oc
# Object in cover analyses -----------------------------------------------------
coverIn2000 <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                                grplst = groupList, strata = strata, ns = sampSize,
                                areas = stratumAreas, cvrfld = "LAND_COVER",
                                cndtnfld = "LAND_USE_YEAR_2000")

coverInUse <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                               grplst = groupList, strata = strata, ns = sampSize,
                               areas = stratumAreas, cvrfld = "LAND_COVER", 
                               cndtnfld = "LAND_USE")

useinCover <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                               grplst = groupList, strata = strata, ns = sampSize,
                               areas = stratumAreas, cvrfld = "LAND_USE",
                               cndtnfld = "LAND_COVER")

useinUse2000 <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                                 grplst = groupList, strata = strata, ns = sampSize,
                                 areas = stratumAreas, cvrfld = "LAND_USE",
                                 cndtnfld = "LAND_USE_YEAR_2000")

# calculate class interaction areas 
coverIn2000Area <- calcError(coverIn2000, use2000Area, 3, 4, 1, 2)

coverInUseArea <- calcError(coverInUse, useArea, 3, 4, 1, 2)

useInCoverArea <- calcError(useinCover, coverArea, 3, 4, 1, 2)

useInUse2000Area <- calcError(useinUse2000, use2000Area, 3, 4, 1, 2)

#+ prettyTables 
## produce tidy output tables for "object in cover" ----------------------------

# area of covers in 2000 uses
coverIn2000Area %>% 
  select(Cover, Condition, Value) %>% 
  spread(Condition, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each commodity crop, 
                                   in each of the land uses for that were 
                                   labelled in the year 2000."))

coverIn2000Area %>% 
  select(Cover, Condition, Error) %>% 
  spread(Condition, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of the area of each 
                                   commodity crop, in each of the land uses for 
                                   that were labelled in the year 2000."))

coverIn2000Area %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of the
                                       area of each commodity crop, in each of 
                                       the land uses that were labelled in 
                                       the year 2000."))

# area of covers in recent uses
coverInUseArea %>% 
  select(Cover, Condition, Value) %>% 
  spread(Condition, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each commodity crop, 
                                   in each of the land uses for that were 
                                   labelled in the period after 2015."))

coverInUseArea %>% 
  select(Cover, Condition, Error) %>% 
  spread(Condition, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of the area of each 
                                   commodity crop, in each of the land uses for 
                                   that were labelled in the period after 2015."))

coverInUseArea %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of the
                                       area of each commodity crop, in each of 
                                       the land uses that were labelled in the 
                                       period after 2015."))

# area of recent uses in covers
useInCoverArea %>% 
  select(Cover, Condition, Value) %>% 
  spread(Cover, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each land use from
                                   the period after 2015 occuring in each 
                                   land cover type."))

useInCoverArea %>% 
  select(Cover, Condition, Error) %>% 
  spread(Cover, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of each land use from
                                   the period after 2015 occuring in each 
                                   land cover type."))

useInCoverArea %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Cover, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of each 
                                       land use from the period after 2015 
                                       occuring in each land cover type."))

# area of recent uses in 2000 uses
useInUse2000Area %>% 
  select(Cover, Condition, Value) %>% 
  spread(Condition, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each land use from
                                   the period after 2015 occuring in each 
                                   land use type in 2000."))

useInUse2000Area %>% 
  select(Cover, Condition, Error) %>% 
  spread(Condition, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of each land use from
                                   the period after 2015 occuring in each 
                                   land use type in 2000."))

useInUse2000Area %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of each 
                                       land use from the period after 2015 
                                       occuring in each land use type in 2000."))

#' ## Understory analysis  
#' The understory areas are small, but contain a small areas that need to be 
#' accounted for in the analysis. 

#+ understory
questions3 <- c("UNDERSTORY_PRESENT", "UNDERSTORY_COVER")
understoryArea <- t(generalResults$Cover$UNDERSTORY_PRESENT) * sum(stratumAreas)
colnames(understoryArea) <- c("Area_ha", "SE_Area_ha")


coverInUnder <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions3, 
                                grplst = groupList, strata = strata, ns = sampSize,
                                areas = stratumAreas, cvrfld = "UNDERSTORY_COVER", 
                                cndtnfld = "UNDERSTORY_PRESENT")

coverInUnderArea <- calcError(coverInUnder, understoryArea, 3, 4, 1, 2)

coverInUnderArea %>% 
  select(Cover, Condition, Value) %>% 
  spread(Condition, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each land cover from
                                   the period after 2015 occuring in the 
                                   understory."))
coverInUnderArea %>% 
  select(Cover, Condition, Error) %>% 
  spread(Condition, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of each land cover from
                                   the period after 2015 occuring in the understory."))

coverInUnderArea %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of each 
                                       land cover from the period after 2015 
                                       occuring in the understory."))

#understory associations
underCrops <- c("Coffee", "Other_Crop")
treeCrops <- c("Coconut", "Fruit_Nut" ,"Pulpwood", "Rubber", "Oil_Palm", 
               "Other_Tree", "Other_Palm", "Other_Crop")

uy_occ <- build_yocc(rawData, mtdt = metaNames, qstns = questions, 
                     cvrfld = "UNDERSTORY_COVER", cndtnfld1 = "LAND_COVER", 
                     cndtnfld2 = "UNDERSTORY_PRESENT", covers = underCrops, 
                     conditions1 = treeCrops, conditions2 = "Yes")


uy_occResults <- do_yocc_analysis(uy_occ, mtdt = metaNames, strata = strata, 
                                  areas = stratumAreas, ns = sampSize, 
                                  qstns = questions, grplst = groupList)

uResults <- bind_rows(uy_occResults$Cover)

uResults %>% 
  select(cover, condition1, PercentCover) %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas)) %>% 
  spread(cover, PercentCover) %>% 
  kable(., digits = 0, col.names = c("Land Use", sort(underCrops)), 
        caption = c("Table X. Total area of each understory commodity crop, 
        in each of the associated overstory crops."))

uResults %>% 
  select(cover, condition1, SE) %>% 
  mutate(SE = SE * sum(stratumAreas)) %>% 
  spread(cover,SE) %>% 
  kable(., digits = 0, col.names = c("Land Use", sort(underCrops)), 
        caption = c("Table X. Total area of each understory commodity crop, 
        in each of the associated overstory crops."))

#+ Carbon
# # calculate Carbon values for areas --------------------------------------------
# 
# # THESE VALUES NEED UPDATING FOR INDONESIA
# 
# carbonMono <- c("Bamboo" = 63.9, "Coffee" = 5, "Fruit_Nut" = 34.7, 
#                 "Oil_Plam" = 39, "Other_Crop" = 5, "Pulpwood" = 38.2, 
#                 "Rice" = 5, "Rubber" = 38.2, "Tea" = 15.3)
# carbonMonoSE <- c()
# 
# carbonAF <- c("Bamboo" = 63.9, "Coffee" = 31, "Fruit_Nut" = 34.7, 
#               "Oil_Plam" = 39, "Other_Crop" = 20, "Pulpwood" = 38.2, 
#               "Rice" = 5, "Rubber" = 38.2, "Tea" = 22)
# carbonAFSE <- c()
# 
# 
# # Calculate total carbon associated with agroforestry systems
# comA <- coverInUseArea[seq(1, 120, 8), ][c(1, 3, 4, 6, 7, 11, 12, 13, 14),]
# comBA <- coverInUseArea[seq(2, 120, 8), ][c(1, 3, 4, 6, 7, 11, 12, 13, 14),]
# comMA <- coverInUseArea[seq(3, 120, 8), ][c(1, 3, 4, 6, 7, 11, 12, 13, 14),]
# comSP <- coverInUseArea[seq(7, 120, 8), ][c(1, 3, 4, 6, 7, 11, 12, 13, 14),]
# comAA <- comA[,1] + comBA[,1] + comMA[,1] + comSP[,1]
# 
# comAASE <- bind_cols(A = comA[,2]^2, BA = comBA[,2]^2, 
#                      MA = comMA[,2]^2, SP = comSP[,2]^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>% 
#   sqrt()
# 
# allAF <- bind_cols(commodity = names(carbonAF), Area_ha = comAA, 
#                    SE_ha = comAASE)
# 
# draftAF <- transmute(allAF,  commodity = commodity, Mg_C = Area_ha * carbonAF, 
#                      SE = SE_ha * carbonAF)
# 
# captionAF <- c("Table X. Aboveground biomass carbon values associated with 
#              the area of commodities in agroforestry land uses in the period 
#              after 2015.")
# kable(draftAF, caption = captionAF, digits = 0,
#       col.names = c("Commodity", "MgC", "SE"))
# 
# # Calculate carbon in associated with plantation areas
# comO <- coverInUseArea[seq(5, 120, 8), ][c(1, 3, 4, 6, 7, 11, 12, 13, 14),]
# comP <- coverInUseArea[seq(6, 120, 8), ][c(1, 3, 4, 6, 7, 11, 12, 13, 14),]
# comT <- coverInUseArea[seq(8, 120, 8), ][c(1, 3, 4, 6, 7, 11, 12, 13, 14),]
# comoPT <- comO[,1] + comP[,1] + comT[,1]
# comoPTSE <- bind_cols(O = comO[,2], P = comP[,2]^2, T = comT[,2]^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(P, T, na.rm = T)) %>% 
#   sqrt()
# 
# allMono <- bind_cols(commodity = names(carbonMono), Area_ha = comoPT, 
#                      SE_ha = comoPTSE)
# 
# draftMono <- transmute(allMono, commodity = commodity, 
#                        Mg_C = Area_ha * carbonMono, SE = SE_ha * carbonMono)
# 
# 
# captionMono <- c("Table X. Aboveground biomass carbon values associated with 
#              the area of commodities in monoculture plantation and terrace land 
#              uses in the period after 2015.")
# kable(draftMono, caption = captionMono, digits = 0, 
#       col.names = c("Commodity", "MgC", "SE"))

#+ raw_tables ------------------------------------------------------------------
kable(coverIn2000Area)
kable(coverInUseArea)
kable(useInCoverArea)
kable(useInUse2000Area)

