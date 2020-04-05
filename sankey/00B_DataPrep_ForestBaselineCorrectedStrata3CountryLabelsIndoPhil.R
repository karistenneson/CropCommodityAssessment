#' ---
#' title: "02_Indonesia_TCC_CarbonSankey.R"
#' author: "K Tenneson, karistenneson@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' ---
#'
#'
#' Set working directory to where data is being stored.
#+ setwd

setwd("C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\Final\\CropCommodityAssessment")

#' ### Required packages
#+ Packages
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)

#+ inputdata
rawData <- read.csv('data\\EcoFloristicJoin\\Compiled03112020withSpatialData.csv', stringsAsFactors=FALSE)
head(rawData)
rawData[1,]
############################################################################
# Indonesia overwrite.
IndoSubset <- read.csv('data\\Corrected\\ceo-indonesia-forest-loss-strata-3-qaqc-2020-04-04.csv', stringsAsFactors=FALSE)
IndoSubset <- IndoSubset[
  (IndoSubset$LAND.USE.2000 == 'Natural Forest' & IndoSubset$LAND.USE == 'Other'), ]
sort(unique(IndoSubset$LON))

sort(unique(IndoSubset$LAT[IndoSubset$LON >= 100.7798 & IndoSubset$LON <= 100.7804]))
sort(unique(IndoSubset$LAT[IndoSubset$LON >= 101.0490 & IndoSubset$LON <= 101.0497]))

test<-rawData[rawData$PL_COUNTRY=='Indonesia' & 
          (
            (
              (rawData$LON >= 100.7798 & rawData$LON <= 100.7804) & (rawData$LAT >= 1.386466 & rawData$LAT <= 1.387139)
              )
            |
              (
              (rawData$LON >= 101.0490 & rawData$LON <= 101.0497) & (rawData$LAT >= 0.1565666 & rawData$LAT <= 0.1572403)
            )
            
          ),]
key<-unique(test$PLOT_ID)

IndoDataSubset<-rawData[rawData$PL_COUNTRY=='Indonesia' & 
                      rawData$PL_STRATUM ==3 &
                        (rawData$PLOT_ID != key[1]) &
                        (rawData$PLOT_ID != key[2]),
                      ]

rawData$LAND_USE_Y[rawData$PL_COUNTRY=='Indonesia' & 
                          rawData$PL_STRATUM==3 &
                          (rawData$PLOT_ID != key[1]) &
                          (rawData$PLOT_ID != key[2])] <- 'Other_LAND_USE_YEAR_2000' 

rawData$LAND_USE[rawData$PL_COUNTRY=='Indonesia' & 
                     rawData$PL_STRATUM==3 &
                     (rawData$PLOT_ID != key[1]) &
                     (rawData$PLOT_ID != key[2])] <- 'Other_LAND_USE'  

rawData$LAND_COVER[rawData$PL_COUNTRY=='Indonesia' & 
                     rawData$PL_STRATUM==3 &
                     (rawData$PLOT_ID != key[1]) &
                     (rawData$PLOT_ID != key[2])] <- 'Other_LAND_COVER' 
############################################################################
# Philippines overwrite.
PhilSubset <- read.csv('data\\Corrected\\ceo-philippines-forest-loss-strata-3-qaqc-2020-04-04.csv', stringsAsFactors=FALSE)
rawData$LAND_USE_Y[rawData$PL_COUNTRY=='Philippines' & 
                     rawData$PL_STRATUM==3] <- 'Other_LAND_USE_YEAR_2000' 

rawData$LAND_USE[rawData$PL_COUNTRY=='Philippines' & 
                   rawData$PL_STRATUM==3] <- 'Other_LAND_USE'  

rawData$LAND_COVER[rawData$PL_COUNTRY=='Philippines' & 
                     rawData$PL_STRATUM==3] <- 'Other_LAND_COVER' 
############################################################################
## Quality Checks

## I. check land use of fruit and nut classifications
head(rawData[(rawData$LAND_COVER == 'Fruit_Nut') & (rawData$LAND_USE == 'Other_LAND_USE'),])
rawData <- mutate(rawData, LAND_USE = case_when(
  # Assign not tree cover
  (LAND_COVER == 'Fruit_Nut') & (LAND_USE == 'Other_LAND_USE') ~ 'Plantation',
  # Others are tree cover
  TRUE ~ as.character(LAND_USE)
)
)
head(rawData[(rawData$LAND_COVER == 'Fruit_Nut') & (rawData$LAND_USE == 'Other_LAND_USE'),])

## II. check land cover of agriculture land uses
Ag<-rawData[rawData$LAND_USE == 'Plantation' | rawData$LAND_USE == 'Mixed_Agrisilviculture' | 
          rawData$LAND_USE == 'Agrisiviculture' | rawData$LAND_USE == 'Terrace' | 
          rawData$LAND_USE == 'Boundary_Agrisilviculture', ] 
unique(Ag$LAND_COVER)        
remove(Ag)

## III. check if all samples have an associated ecofloristic zone.
noEco <- rawData[rawData$ecofloristic=="",]
head(noEco)
remove(noEco)

############################################################################
## add in columns that aggregate data to use in analysis.

################################################
## 1a. update ecoflorForest to include only tree canopy cover
unique(rawData$LAND_USE_Y)
unique(rawData$ecoflorist)
rawData <- mutate(rawData, EcoForestBase = case_when(
  # Assign not tree cover
  (LAND_USE_Y != 'Natural_Forest') ~ 'NotForest',
  # Others are tree cover
  TRUE ~ as.character(ecoflorist)
)
)
unique(rawData$EcoForestBase)

## 1b. Abbr Ecofloristic
# EcoForestBaseAbb
unique(rawData$EcoForestBase)
rawData <- mutate(rawData, EcoForestBaseAbb = case_when(
  # Not Forest
  (EcoForestBase == "NotForest") ~ 'NotFo',
  # Tropical dry forest
  (EcoForestBase == "Tropical dry forest") ~ 'TDryF',
  # Tropical rainforest
  (EcoForestBase == 'Tropical rainforest') ~ 'TRain',
  # Tropical moist deciduous forest
  (EcoForestBase == "Tropical moist deciduous forest") ~ 'TMDec',
  # Tropical shrubland
  (EcoForestBase == "Tropical shrubland") ~ 'TShrb',
  # Tropical mountain system
  (EcoForestBase == "Tropical mountain system") ~ 'TMtnS',
  # Subtropical humid forest
  (EcoForestBase == "Subtropical humid forest") ~ 'SHumF',
  # All else
  TRUE ~ "FixMe"
)
)
unique(rawData$EcoForestBaseAbb)
head(rawData)

## 1c. Add in indicator for peatlands.
# EcoForestBaseAbb
head(rawData)
rawData$EcoForestBaseAbb[rawData$Peatlands == 1] <- paste0(rawData$EcoForestBaseAbb[rawData$Peatlands == 1], '_pt')
rawData$EcoForestBaseAbb[rawData$Peatlands == 0] <- paste0(rawData$EcoForestBaseAbb[rawData$Peatlands == 0], '___')
unique(rawData$EcoForestBaseAbb)
head(rawData)

################################################
## 2. Add in indicator for protected areas.
# EcoProtForestBaseAbb
head(rawData)
rawData$EcoProtForestBaseAbb[rawData$WDProtArea == 1] <- paste0(rawData$EcoForestBaseAbb[rawData$WDProtArea == 1], '_WDPA')
rawData$EcoProtForestBaseAbb[rawData$WDProtArea == 0] <- paste0(rawData$EcoForestBaseAbb[rawData$WDProtArea == 0], '_noPA')
unique(rawData$EcoProtForestBaseAbb)
head(rawData)

################################################
## 3. Tree canopy cover in 2000?
rawData <- mutate(rawData, TreeCover_2000 = case_when(
  # Assign not tree cover
  (LAND_USE_Y == 'Other_LAND_USE_YEAR_2000') ~ 'Not tree',
  # Assign not tree cover
  (LAND_USE_Y == 'Forest_Commodity') ~ 'Tree crop',
  # Others are tree cover
  TRUE ~ 'Tree cover'
)
)
unique(rawData$TreeCover_2000)
head(rawData)

################################################
## 4. Forest, tree canopy cover dynamics, or other land use/cover
rawData <- mutate(rawData, ForestDynamics = case_when(
  # Stable forest
  (TreeCover_2000 == 'Tree cover') & (LAND_USE == 'Natural_Forest') ~ 'No change',
  # Tree cover dynamics
  (TreeCover_2000 == 'Tree cover') & (LAND_USE != 'Natural_Forest') ~ 'TCCdynamics',
  TRUE ~ 'No change'
)
)
unique(rawData$ForestDynamics)
head(rawData)

################################################
## 5. LossType:
#a) Identify tcc transition types for non-commodity transitions.
# crop transitions are included in some of these cases, others are labeled as 'issues to check'.
unique(rawData$LAND_COVER)
rawData <- mutate(rawData, LossType = case_when(
  # Not forest in base year
  (ForestDynamics != 'TCCdynamics') ~ 'Not target',
  # TCC to other
  (ForestDynamics == 'TCCdynamics') & 
    (LAND_COVER == 'Water' | LAND_COVER == 'Non_vegetated' | LAND_COVER == 'Other_LAND_COVER') ~ 
    'TCC_to_Other',
  # TCC to Builtup
  (ForestDynamics == 'TCCdynamics') & (LAND_COVER == 'Built_up') ~ 'TCC_to_BuiltUp',
  # TCC to vegetation
  (ForestDynamics == 'TCCdynamics') & 
    (LAND_COVER == 'Other_Shrub' | LAND_COVER == 'Herbaceous' | LAND_COVER == 'Bamboo' | 
       LAND_COVER == 'Other_Tree') ~ 'TCC_to_Veg',
  # TCC to Silvopastoral
  (ForestDynamics == 'TCCdynamics') & (LAND_USE == 'Silvopastoral') ~ 'TCC_to_Silvopastoral',
  # Leftover
  TRUE ~ 'Issues here to check'
)
)
unique(rawData$LossType)
head(rawData)

## 5. LossType:
#b) Identify tcc transition types for crop and crop commodity transitions.
# this check some of the previous labels (based on lc) to update based on land use
# and updates all records marked as 'issues to check'.
rawData <- mutate(rawData, LossType = case_when(
  # TCC to Agriculture
  (ForestDynamics == 'TCCdynamics') & 
    (LAND_COVER == 'Aquaculture' | LAND_COVER == 'Banana' | LAND_COVER == 'Coffee' | 
       LAND_COVER == 'Coconut' | LAND_COVER =='Fruit_Nut' | LAND_COVER == 'Oil_Palm' | 
       LAND_COVER == 'Other_Crop' | LAND_COVER == 'Other_Palm' | LAND_COVER == 'Pulpwood' | 
       LAND_COVER == 'Rice' |  LAND_COVER == 'Rubber' | LAND_COVER == 'Tea' | 
       LAND_USE == 'Plantation' | LAND_USE == 'Mixed_Agrisilviculture' | LAND_USE == 'Agrisiviculture' |
       LAND_USE == 'Terrace' | LAND_USE == 'Boundary_Agrisilviculture') ~ 'TCC_to_Agriculture',
  # Leftover
  TRUE ~ LossType
)
)

## 5. LossType:
#c) safety checks.
unique(rawData$LossType)
unique(rawData$LossType[rawData$LAND_USE_Y == 'Forest_Commodity'])
unique(rawData$LossType[rawData$LAND_USE_Y == 'Other_LAND_USE_YEAR_2000'])
unique(rawData$LossType[rawData$LAND_USE_Y == 'Natural_Forest'])
head(rawData[rawData$LAND_USE_Y == 'Natural_Forest' & rawData$LossType == "Not target",])

checkData <- rawData[rawData$LossType == 'Issues here to check', c("ForestDynamics", "LAND_COVER","LAND_USE")]
head(rawData[rawData$LossType == 'Issues here to check', c("ForestDynamics", "LAND_COVER","LAND_USE")])
unique(rawData[rawData$LossType == 'Issues here to check', c("ForestDynamics", "LAND_COVER","LAND_USE")])
head(rawData)
remove(checkData)
###################################################
## 5d. abbreviate loss type.
# 'LossTypeAbb'
unique(rawData$LossType)
rawData <-  mutate(rawData, LossTypeAbb = case_when(
  LossType == "TCC_to_Agriculture" ~ 'ToCrop', 
  LossType == "TCC_to_BuiltUp" ~ 'ToBuUp', 
  LossType == "TCC_to_Other" ~ 'ToOthr', 
  LossType == "TCC_to_Silvopast" ~ 'ToSilv',
  LossType == "TCC_to_Veg" ~ 'ToVegn',
  LossType == "Not target" ~ 'NoLoss',
  TRUE ~ 'Houston, we have a problem'
))
unique(rawData$LossTypeAbb)
head(rawData)

## 5e. concatenate baseline forest type with loss type.
# 'Forest_Conv'
rawData<-unite(rawData, 'Forest_Conv', c("EcoForestBaseAbb", "LossTypeAbb"), remove = FALSE)
unique(rawData$Forest_Conv)

## 5f. concatenate baseline forest type with loss type, with protected areas.
# 'Forest_Conv_PA'
rawData<-unite(rawData, 'Forest_Conv_PA', c("EcoProtForestBaseAbb", "LossTypeAbb"), remove = FALSE)
unique(rawData$Forest_Conv_PA)
table(rawData$Forest_Conv_PA[rawData$WDProtArea == 1])

################################################
## 6a. Identify crop types, 'CropType'
unique(rawData$LAND_COVER[rawData$LossType == 'TCC_to_Agriculture'])

rawData <- mutate(rawData, CropType = case_when(
  # rubber,  
  # Oil Palm
  # tea, coffee,
  (LossType == 'TCC_to_Agriculture') & 
    (LAND_COVER == 'Rubber' |  LAND_COVER == 'Fruit_Nut' | LAND_COVER == 'Pulpwood' |
       LAND_COVER == 'Oil_Palm' | LAND_COVER == 'Banana' | LAND_COVER == 'Coconut' |
       LAND_COVER == 'Tea' | LAND_COVER == 'Coffee' |
       LAND_COVER == 'Rice' | 
       LAND_COVER == 'Aquaculture') ~ as.character(LAND_COVER),
  # Other tree
  (LossType == 'TCC_to_Agriculture') & (LAND_COVER == 'Other_Tree') ~ 'Tree_crop',
  # Other palm
  (LossType == 'TCC_to_Agriculture') & (LAND_COVER == 'Other_Palm') ~ 'Palm_crop',
  # Other shrub
  (LossType == 'TCC_to_Agriculture') & (LAND_COVER == 'Other_Shrub') ~ 'Shrub_crop',
  # Other crop
  (LossType == 'TCC_to_Agriculture') & 
    (LAND_COVER == 'Other_Crop' | LAND_COVER == 'Herbaceous') ~ 'Herb_crop',
  # Crop support
  (LossType == 'TCC_to_Agriculture') & 
    (LAND_COVER == 'Other_LAND_COVER' | LAND_COVER == 'Non_vegetated' | 
       LAND_COVER == 'Built_up' | LAND_COVER == 'Water') ~ 'Crop_support',
  # All else
    TRUE ~ "None"
)
)

unique(rawData$CropType)

## 6b. convert variables to characters before concatenating.
# CropTypeAbb
rawData$CropTypeAbb <- substr(rawData$CropType, 1,4)
unique(rawData$CropTypeAbb)

###################################################
## 6c. concatenate baseline forst type with current crop type.
# EcoZne_Crop
rawData<-unite(rawData, 'EcoZne_Crop', c("EcoForestBaseAbb", "CropTypeAbb"), remove = FALSE)
unique(rawData$EcoZne_Crop)

## 6d. concatenate baseline forst type with current crop type, with protected areas.
# EcoZne_Crop_PA
rawData<-unite(rawData, 'EcoZne_Crop_PA', c("EcoProtForestBaseAbb", "CropTypeAbb"), remove = FALSE)
unique(rawData$EcoZne_Crop_PA)

table(rawData$EcoZne_Crop_PA[rawData$WDProtArea == 1])


###################################################
## 7. Identify crop Structure, 'CropStructure'
rawData <- mutate(rawData, CropStructure = case_when(
  # Tree
  (CropType == 'Rubber' | CropType == 'Fruit_Nut' | CropType == 'Pulpwood' | 
     CropType == 'Tree_crop') ~ 'All_tree_crops',
  # Palm
  (CropType == 'Oil_Palm' | CropType == 'Banana' | CropType == 'Coconut'| 
     CropType == 'Palm_crop') ~ 'All_palm_crops',
  # Shrub
  (CropType == 'Tea' | CropType == 'Coffee' | CropType == 'Shrub_crop') ~ 'All_Shrub_Crops',
  # Herb
  (CropType == 'Rice' | CropType == 'Herb_crop' | CropType == 'Aquaculture') ~ 'All_herbaceous_crops',
  # Crop support
  (CropType == 'Crop_support') ~ 'Crop_support',
  # All else
  TRUE ~ "None"
)
)
unique(rawData$CropStructure)

###################################################
## 8a. Identify agroforestry system, 'AgroSystem'
rawData <- mutate(rawData, AgroSystem = case_when(
  # Traditional
  (LossType == 'TCC_to_Agriculture') & 
    (LAND_USE != 'Agrisiviculture' & LAND_USE != 'Mixed_Agrisilviculture' & 
       LAND_USE != 'Strip_Agrisilviculture' & LAND_USE != 'Boundary_Agrisilviculture') ~ 'Trad',
  # Agrisilv
  (LossType == 'TCC_to_Agriculture') &
    (LAND_USE == 'Agrisiviculture' | LAND_USE == 'Mixed_Agrisilviculture' | 
       LAND_USE == 'Strip_Agrisilviculture') ~ 'Agrisil',
  # Bndry
  (LossType == 'TCC_to_Agriculture' & LAND_USE == 'Boundary_Agrisilviculture') ~ 'Bndry',
  # pastoral
  (LossType == 'TCC_to_Agriculture' & LAND_USE == 'Silvopastoral') ~ 'S_Pastoral',
  # All else
  TRUE ~ "None"
)
)
unique(rawData$AgroSystem)

###################################################
## 8b. Concatenate crop type with agroforestry system.
# Crop_Agro
unique(rawData$CropTypeAbb)
rawData<-unite(rawData, 'Crop_Agro', c("CropTypeAbb","AgroSystem"), remove = FALSE)
unique(rawData$Crop_Agro)

###################################################
## 9a. concatenate baseline forst type with current crop type and agroforestry.
# EcoZne_CropAgro
rawData<-unite(rawData, 'EcoZne_CropAgro', c("EcoForestBaseAbb", "Crop_Agro"), remove = FALSE)
unique(rawData$EcoZne_CropAgro)

## 9b. concatenate baseline forst type with current crop type and agroforestry, with protected areas.
# EcoZne_CropAgro_PA
rawData<-unite(rawData, 'EcoZne_CropAgro_PA', c("EcoProtForestBaseAbb", "Crop_Agro"), remove = FALSE)
unique(rawData$EcoZne_CropAgro_PA)

table(rawData$EcoZne_Crop_PA[rawData$WDProtArea == 1])


###################################################
###################################################
## Export Data
head(rawData)
write.csv(rawData, 'data\\EcoFloristicJoin\\Compiled_Processed_withSpatial_04042020.csv', row.names = F)

