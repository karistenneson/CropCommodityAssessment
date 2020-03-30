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

## sampled the FAO ecofloristic data in ArcMap and exported merged csv.
#rawDataNoPeatlands <- read.csv('data\\IndonesiaIntermediateFiles\\CleanedEcoFloristic.csv')
# rawDataOrigC <- read.csv('data\\EcoFloristicJoin\\cambodia_code_loss.csv')
# rawDataOrigI <- read.csv('data\\EcoFloristicJoin\\indonesia_code_loss.csv')
# rawDataOrigL <- read.csv('data\\EcoFloristicJoin\\laos_code_loss.csv')
# rawDataOrigM <- read.csv('data\\EcoFloristicJoin\\myanmar_code_loss.csv')
# rawDataOrigP <- read.csv('data\\EcoFloristicJoin\\philippines_code_loss.csv')
# rawDataOrigT <- read.csv('data\\EcoFloristicJoin\\thailand_code_loss.csv')
# rawDataOrigV <- read.csv('data\\EcoFloristicJoin\\vietnam_code_loss.csv')

rawDataOrig <- rbind(read.csv('data\\EcoFloristicJoin\\cambodia_code_loss.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\EcoFloristicJoin\\indonesia_code_loss.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\EcoFloristicJoin\\laos_code_loss.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\EcoFloristicJoin\\myanmar_code_loss.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\EcoFloristicJoin\\philippines_code_loss.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\EcoFloristicJoin\\thailand_code_loss.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\EcoFloristicJoin\\vietnam_code_loss.csv', stringsAsFactors=FALSE)
                     )
# subset columns.
rawData <- rawDataOrig[,c("PLOT_ID", "PL_ORIGID", "SAMPLE_ID", "LAT", "LON", 
                          "PL_COUNTRY", "PL_STRATUM", "USER_ID", "IMAGERY_TI", 
                          "LAND_COVER", "UNDERSTORY", "LAND_USE","LAND_USE_1",
                          "code_name","PL_PLOTID")]
head(rawData)

############################################################################
## Quality Checks

## 1. check land use of fruit and nut classifications
rawData[(rawData$LAND_COVER == 'Fruit_Nut') & (rawData$LAND_USE == 'Other_LAND_USE'),]
rawData <- mutate(rawData, LAND_USE = case_when(
  # Assign not tree cover
  (LAND_COVER == 'Fruit_Nut') & (LAND_USE == 'Other_LAND_USE') ~ 'Plantation',
  # Others are tree cover
  TRUE ~ as.character(LAND_USE)
)
)

## 2. check land cover of agriculture land uses
Ag<-rawData[rawData$LAND_USE == 'Plantation' | rawData$LAND_USE == 'Mixed_Agrisilviculture' | 
          rawData$LAND_USE == 'Agrisiviculture' | rawData$LAND_USE == 'Terrace' | 
          rawData$LAND_USE == 'Boundary_Agrisilviculture', ] 
unique(Ag$LAND_COVER)        

## 3. check if all samples have an associated ecofloristic zone.
noEco <- rawData[rawData$Ecofloristic=="",]

############################################################################
## add in columns that aggregate data to use in analysis.

## 1. update ecofloristic zone to include only tree canopy cover
unique(rawData$LAND_USE_1)
unique(rawData$code_name)
rawData <- mutate(rawData, Ecofloristic = case_when(
  # Assign not tree cover
  (LAND_USE_1 == 'Other_LAND_USE_YEAR_2000') ~ 'NotForest',
  # Others are tree cover
  TRUE ~ as.character(code_name)
)
)
unique(rawData$Ecofloristic)

## 2. Tree canopy cover in 2000?
rawData <- mutate(rawData, TreeCover_2000 = case_when(
  # Assign not tree cover
  (LAND_USE_1 == 'Other_LAND_USE_YEAR_2000') ~ 'Not tree',
  # Others are tree cover
  TRUE ~ 'Tree cover'
)
)
unique(rawData$TreeCover_2000)

## 3. Forest, tree canopy cover dynamics, or other land use/cover
rawData <- mutate(rawData, ForestDynamics = case_when(
  # Stable forest
  (TreeCover_2000 == 'Tree cover') & (LAND_USE == 'Natural_Forest') ~ 'Forest',
  # Tree cover dynamics
  (TreeCover_2000 == 'Tree cover') & (LAND_USE != 'Natural_Forest') ~ 'TCCdynamics',
  TRUE ~ 'Not tree'
)
)
unique(rawData$ForestDynamics)


## 4. Identify tcc transition types
unique(rawData$LAND_COVER)
rawData <- mutate(rawData, LossType = case_when(
  # Not forest in base year
  (LAND_USE_1 == 'Other_LAND_USE_YEAR_2000') ~ 'Not target',
  # stable forest
  (ForestDynamics == 'Forest') ~ Ecofloristic,
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

unique(rawData$LossType)
checkData <- rawData[rawData$LossType == 'Issues here to check', c("ForestDynamics", "LAND_COVER","LAND_USE")]
head(rawData[rawData$LossType == 'Issues here to check', c("ForestDynamics", "LAND_COVER","LAND_USE")])
unique(rawData[rawData$LossType == 'Issues here to check', c("ForestDynamics", "LAND_COVER","LAND_USE")])

## 5. Identify crop types
unique(rawData$LAND_COVER[rawData$LossType == 'TCC_to_Agriculture'])

rawData <- mutate(rawData, CropType = case_when(
  # Oil Palm
  (LossType == 'TCC_to_Agriculture') & 
    (LAND_COVER == 'Rubber' | LAND_COVER == 'Tea' | LAND_COVER == 'Coffee' | 
       LAND_COVER == 'Fruit_Nut' | LAND_COVER == 'Oil_Palm' | LAND_COVER == 'Pulpwood' |
       LAND_COVER == 'Rice' | LAND_COVER == 'Banana' | LAND_COVER == 'Coconut' |
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

## 6. Identify crop Structure
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
  (CropType == 'Crop_Support') ~ 'Crop_support',
  # All else
  TRUE ~ "None"
)
)
unique(rawData$CropStructure)

## 7.
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
  # All else
  TRUE ~ "None"
)
)
unique(rawData$AgroSystem)

##################################
## Concatenate some columns
##################################
## 8. 
unique(rawData$Ecofloristic)
rawData <- mutate(rawData, EcofloristicAbb = case_when(
  # Not Forest
  (Ecofloristic == "NotForest") ~ 'NotFo',
  # Tropical dry forest
  (Ecofloristic == "Tropical dry forest") ~ 'TDryF',
  # Tropical rainforest
  (Ecofloristic == 'Tropical rainforest') ~ 'TRain',
  # Tropical moist deciduous forest
  (Ecofloristic == "Tropical moist deciduous forest") ~ 'TMDec',
  # Tropical shrubland
  (Ecofloristic == "Tropical shrubland") ~ 'TShrb',
  # Tropical mountain system
  (Ecofloristic == "Tropical mountain system") ~ 'TMtnS',
  # Subtropical humid forest
  (Ecofloristic == "Subtropical humid forest") ~ 'SHumF',
  # All else
  TRUE ~ "FixMe"
)
)
unique(rawData$EcofloristicAbb)

## 9. convert variables to characters before concatenating.
rawData$CropType<-as.character(rawData$CropType)
rawData$AgroSystem<-as.character(rawData$AgroSystem)
rawData$CropTypeAbb <- substr(rawData$CropType, 1,4)

unique(rawData$CropTypeAbb)
unique(rawData$LossType)
###################################################
## For Fig 2. 
#############################
## 10. concatenate before forst type with current crop type.
rawData<-unite(rawData, 'Forest_Conv', c("EcofloristicAbb","LossType"), remove = FALSE)
unique(rawData$Forest_Conv)

###################################################
## For Fig 3. 
#############################
## 11. concatenate before forst type with current crop type.
rawData<-unite(rawData, 'EcoZne_Crop', c("EcofloristicAbb","CropTypeAbb"), remove = FALSE)
unique(rawData$EcoZne_Crop)

## 12. Concatenate crop type with agroforestry system.
unique(rawData$CropTypeAbb)
rawData<-unite(rawData, 'Crop_Agro', c("CropTypeAbb","AgroSystem"), remove = FALSE)
unique(rawData$Crop_Agro)

###################################################
###################################################
## Export Data
write.csv(rawData, 'data\\EcoFloristicJoin\\CompiledData.csv')

