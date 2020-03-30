#' ---
#' title: "Indonesia Analysis Script"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: [github_document, word_document]
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
source("00_GIA_functions.R")

#+ inputdata
# Data input and cleaning ------------------------------------------------------
#dataPath <- c("data/Indonesia")
#files <- dir(dataPath)

#rawData <- files %>% 
#  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))

# clean data
# rawData <- clean_data(rawData, c(15:19))

# remove unneeded columns 
# keep <- c(metaNames, "FLAGGED", questions)
# rawData <- rawData[,keep]

rawData <- read.csv('data\\IndonesiaIntermediateFiles\\Indonesia_carbon_plots.csv')
head(rawData)
colnames(rawData)

rawData <- rawData[rawData$FLAGGED == 'FALSE',]

## update ecofloristic zone
rawData$Ecofloristic <- 'Not forest' #817
rawData$Ecofloristic[rawData$code == 817 & rawData$LAND_USE_Y == 'Natural Forest'] <- 'Tropical moist deciduous forest' #817
rawData$
  Ecofloristic[rawData$code == 818 & rawData$LAND_USE_Y == 'Natural Forest'] <- 'Tropical mountain system' #818
rawData$Ecofloristic[rawData$code == 819 & rawData$LAND_USE_Y == 'Natural Forest'] <- 'Tropical rainforest' #819
rawData$Ecofloristic[rawData$code == 820 & rawData$LAND_USE_Y == 'Natural Forest'] <- 'Tropical shrubland' #820

rawData$TreeCov2000<-'Tree_Cov'
rawData$TreeCov2000[rawData$LAND_USE_Y == 'Other'] <- 'Not tree'
## update forest loss vs. stable
rawData$ForestDynamics[(rawData$LAND_USE_Y == 'Natural Forest' | rawData$LAND_USE_Y == 'Forest Commodity') & 
                         rawData$LAND_USE == 'Natural Forest'] <- 'TCC_current'
rawData$ForestDynamics[(rawData$LAND_USE_Y == 'Natural Forest'| rawData$LAND_USE_Y == 'Forest Commodity') & 
                         rawData$LAND_USE != 'Natural Forest'] <- 'TCCdynamics'
rawData$ForestDynamics[rawData$LAND_USE_Y == 'Other'] <- 'Not tree'

## 
length(unique(rawData$PL_PLOTID[rawData$PL_STRATUM == '3' & rawData$LAND_USE_Y == 'Natural Forest'])) 
length(rawData$PL_PLOTID[rawData$PL_STRATUM == '3' & rawData$LAND_USE_Y == 'Natural Forest']) 

## update forest loss transition type
rawData$LossType <- rawData$ForestDynamics
rawData$Transition2<-rawData$Transition
rawData$Transition<-'None'

unique(rawData[rawData$Transition == 'Forest_to_Water', c('LAND_COVER', 'LAND_USE')])
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & rawData$LAND_COVER == 'Water'] <- 'TCC_to_Water'

unique(rawData[rawData$Transition == 'Forest_to_Barren', c('LAND_COVER', 'LAND_USE')])
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & rawData$LAND_COVER == 'Non-vegetated'] <- 'TCC_to_Barren'

#####################################################################
################## Agriculture ######################################
#####################################################################


# prep to Agriculture Crops
unique(rawData[rawData$Transition == 'Forest_to_Agriculture', c('LAND_COVER', 'LAND_USE')])
unique(rawData[rawData$Transition == 'Forest_to_Agriculture', 'LAND_USE'])

# to Agriculture Crops, Other
unique(rawData$LAND_COVER[(rawData$Transition == 'Forest_to_Agriculture' & rawData$LAND_USE == 'Other')]) 
rawData$LossType[(rawData$ForestDynamics == 'TCCdynamics' &  
                    (rawData$LAND_COVER == 'Other Crop' | rawData$LAND_COVER == 'Rice' | rawData$LAND_COVER == 'Aquaculture')
                  )]<- 'TCC_to_Agriculture'
# to Agriculture Crops
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & 
                   (rawData$LAND_USE == 'Plantation' | rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture'
                    | rawData$LAND_USE == 'Terrace' | rawData$LAND_USE == 'Boundary Agrisilviculture')]<- 'TCC_to_Agriculture'

# to Agriculture Crops, Plantation
unique(rawData$LAND_COVER[(rawData$Transition == 'Forest_to_Agriculture' & rawData$LAND_USE == 'Plantation')]) 
# to Agriculture Crops, Mixed Agrisilviculture
unique(rawData$LAND_COVER[(rawData$Transition == 'Forest_to_Agriculture' & rawData$LAND_USE == 'Mixed Agrisilviculture')]) 
# to Agriculture Crops, Agrisiviculture
unique(rawData$LAND_COVER[(rawData$Transition == 'Forest_to_Agriculture' & rawData$LAND_USE == 'Agrisiviculture')]) 
# to Agriculture Crops, Terrace
unique(rawData$LAND_COVER[(rawData$Transition == 'Forest_to_Agriculture' & rawData$LAND_USE == 'Terrace')]) 
# to Agriculture Crops, Boundary Agrisilviculture
unique(rawData$LAND_COVER[(rawData$Transition == 'Forest_to_Agriculture' & rawData$LAND_USE == 'Boundary Agrisilviculture')]) 


########## update crop type column
rawData$CropType <- 'None' 
rawData$CropType[rawData$ForestDynamics == 'stable'] <- 'Stable Forest' 
rawData$CropType[rawData$ForestDynamics == 'Not Forest'] <- 'not_crop_or_forest' 

rawData$LossType[rawData$ForestDynamics == 'stable'] <- 'Stable Forest' 
rawData$LossType[rawData$ForestDynamics == 'Not Forest'] <- 'not_crop_or_forest' 

unique(rawData$LAND_COVER[(rawData$Transition == 'Forest_to_Agriculture' & rawData$LAND_USE == 'Plantation')]) 
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Oil Palm' 
                  & (rawData$LAND_USE == 'Plantation' | rawData$LAND_USE == 'Mixed Agrisilviculture' | 
                       rawData$LAND_USE == 'Agrisiviculture' | rawData$LAND_USE == 'Boundary Agrisilviculture')
                  )]<- 'Oil_Palm'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Rubber' 
                  & rawData$LAND_USE == 'Plantation')]<- 'Rubber_Crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other Tree' 
                  & (rawData$LAND_USE == 'Plantation' | rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture')
                  )]<- 'Tree_Crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other Tree' 
                  & (rawData$LAND_USE == 'Boundary Agrisilviculture'))]<- 'Boundary_Tree_Crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other Palm' 
                  & (rawData$LAND_USE == 'Plantation' | rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture')
                  )]<- 'Palm_Crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Coconut' 
                  & (rawData$LAND_USE == 'Plantation' | rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture')
                  )]<- 'Coconut_Crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Pulpwood' 
                  & (rawData$LAND_USE == 'Plantation' | rawData$LAND_USE == 'Mixed Agrisilviculture'))]<- 'Pulpwood_Crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Fruit/Nut' 
                  & (rawData$LAND_USE == 'Plantation' | rawData$LAND_USE == 'Mixed Agrisilviculture' | 
                       rawData$LAND_USE == 'Agrisiviculture' | rawData$LAND_USE == 'Boundary Agrisilviculture')
                  )]<- 'Fruit_Nut_Crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Herbaceous' 
                  & rawData$LAND_USE == 'Plantation')]<- 'Plantation_ground_Cover'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other Crop' 
                  & rawData$LAND_USE == 'Plantation')]<- 'Other_crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other' 
                  & rawData$LAND_USE == 'Plantation')]<- 'Plantation_support'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Non-vegetated' 
                  & rawData$LAND_USE == 'Plantation')]<- 'Plantation_support'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Water' 
                  & rawData$LAND_USE == 'Plantation')]<- 'Plantation_support'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Built-up' 
                  & rawData$LAND_USE == 'Plantation')]<- 'Plantation_support'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other Shrub' 
                  & rawData$LAND_USE == 'Plantation')]<- 'Shrub_crop'
unique(rawData[(rawData$Transition == 'Forest_to_Agriculture' & rawData$LAND_USE == 'Plantation'),c('CropType', 'LAND_COVER')])


########## update crop type column
unique(rawData$LAND_COVER[(rawData$Transition == 'Forest_to_Agriculture' & rawData$LAND_USE == 'Mixed Agrisilviculture')]) 
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Herbaceous' 
                  & (rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture'| rawData$LAND_USE == 'Boundary Agrisilviculture')
                  )]<- 'Other_crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other Crop' 
                  & (rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture' 
                     | rawData$LAND_USE == 'Terrace' | rawData$LAND_USE == 'Boundary Agrisilviculture' | rawData$LAND_USE == 'Other')
                  )]<- 'Other_crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other' 
                  & rawData$LAND_USE == 'Mixed Agrisilviculture')]<- 'Crop_support'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Non-vegetated' 
                  & (rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture'| rawData$LAND_USE == 'Boundary Agrisilviculture')
                  )]<- 'Crop_support'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Built-up' 
                  & (rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture'))]<- 'Crop_support'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Other Shrub' 
                  & (rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture' | rawData$LAND_USE == 'Boundary Agrisilviculture')
                  )]<- 'Shrub_crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Coffee' 
                  & (rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture' | rawData$LAND_USE == 'Terrace')
                  )]<- 'Coffee_crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Rice' 
                  & (rawData$LAND_USE == 'Mixed Agrisilviculture' | rawData$LAND_USE == 'Agrisiviculture' | rawData$LAND_USE == 'Terrace' 
                     | rawData$LAND_USE == 'Other')
                  )]<- 'Rice_crop'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Aquaculture')]<- 'Aquaculture'

# to built up
unique(rawData[rawData$Transition == 'Forest_to_BuiltUp', c('LAND_COVER', 'LAND_USE')])
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & rawData$LAND_COVER == 'Built-up'& 
                   rawData$LossType != 'TCC_to_Agriculture'] <- 'TCC_to_BuiltUp'
# to other
unique(rawData[(rawData$Transition == 'TCC_to_Other'), c('LAND_COVER', 'LAND_USE')])
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & rawData$LAND_COVER == 'Other' & 
                   rawData$LossType != 'TCC_to_Agriculture'] <- 'TCC_to_Other'
# to other shrub
unique(rawData[rawData$Transition == 'Forest_to_Shrub', c('LAND_COVER', 'LAND_USE')])
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & rawData$LAND_COVER == 'Other Shrub' & 
                   rawData$LossType != 'TCC_to_Agriculture'] <- 'TCC_to_Veg'
# to other herb
unique(rawData[rawData$Transition == 'Forest_to_Herb', c('LAND_COVER', 'LAND_USE')])
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & rawData$LAND_COVER == 'Herbaceous'& 
                   rawData$LossType != 'TCC_to_Agriculture'] <- 'TCC_to_Veg'
# to other bamboo or tree
unique(rawData[rawData$Transition == 'Forest_to_OtherTree', c('LAND_COVER', 'LAND_USE')])
rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & (rawData$LAND_COVER == 'Bamboo' |rawData$LAND_COVER == 'Other Tree') & 
                   rawData$LossType != 'TCC_to_Agriculture'] <- 'TCC_to_Veg'

# to Silvopastoral 
unique(rawData[rawData$Transition == 'Forest_to_Agriculture' & rawData$LAND_USE == 'Silvopastoral', c('LAND_COVER', 'LAND_USE')])
rawData$LossType[(rawData$ForestDynamics == 'TCCdynamics' & rawData$LAND_USE == 'Silvopastoral')] <- 'TCC_to_Silvopastoral'

unique(rawData[rawData$Transition =='Forest_to_OtherTree', c("LAND_USE","LAND_COVER", 'ForestDynamics')])
unique(rawData[rawData$Transition =='Forest_to_Agriculture', c("LAND_USE","LAND_COVER", 'ForestDynamics')])

unique(rawData[,c('Transition2','LossType')])
unique(rawData[,c('LossType', 'CropType')])
unique(rawData[(rawData$LossType == 'TCC_to_Agriculture' & rawData$CropType == 'None'),])
unique(rawData[(rawData$LossType == 'TCC_to_Agriculture' & rawData$CropType == 'None'),c('LAND_COVER')])
unique(rawData[(rawData$LossType == 'TCCdynamics'),c('LAND_COVER', 'LAND_USE')])

rawData$LossType[rawData$ForestDynamics == 'TCCdynamics' & rawData$LAND_COVER == 'Oil Palm'] <- 'TCC_to_Agriculture'
rawData$CropType[(rawData$LossType == 'TCC_to_Agriculture' & rawData$LAND_COVER == 'Oil Palm' 
                  & rawData$LAND_USE == 'Other')]<- 'Oil_Palm'

################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
############################# START HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##########################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################

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
                "Ecofloristic", "ForestDynamics", "LossType", "CropType")
######################################################################################################################
######################################################################################################################
## statistic 1
MyQuestion <- 'TreeCov2000'
cover <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                                 ns = sampSize, areas = stratumAreas, qstns = MyQuestion, 
                                 grplst = groupList)

coverArea<- t(cover$Cover$TreeCov2000)*sum(stratumAreas)
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

