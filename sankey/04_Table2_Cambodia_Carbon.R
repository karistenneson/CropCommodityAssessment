#' ---
#' title: "02_Indonesia_TCC_CarbonSankey.R"
#' author: "K Tenneson, karistenneson@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' ---
#'
#'
#' ### Required packages
#+ Packages
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)
library(networkD3)
library(tidyverse)

setwd("C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\Final\\CropCommodityAssessment")

######################################################
##### Set up the country #####
######################################################
#"Cambodia"    "Indonesia"   "Laos"        "Myanmar"     "Philippines" "Thailand"    "Vietnam"

country <- 'Vietnam'

#######################################################
##### Crop Carbon Key #####
#######################################################

# add in crop Carbon key.
cropCarbon <- read.csv(paste0('data/CarbonKey/CropCarbonKey.csv'), stringsAsFactors=FALSE)
cropCarbon <- cropCarbon[cropCarbon$Country == country,]
colnames(cropCarbon)[1] <- "CropLabel"
head(cropCarbon)

#######################################################
##### Forest Carbon Key #####
#######################################################

# load forest carbon factors.
forestCarbon <- read.csv(paste0('data/CarbonKey/ForestCarbonKey.csv'), stringsAsFactors=FALSE)
colnames(forestCarbon)[1] <- "Forest"
if (country == 'Philippines' | country == 'Indonesia'){
  forestCarbon <- forestCarbon[forestCarbon$Region == 'Marine',]
} else {
  forestCarbon <- forestCarbon[forestCarbon$Region == 'Mekong',]
}
head(forestCarbon)

#######################################################
##### Crop Information #####
#######################################################

# load crop conversion area estimates.
cropArea <- read.csv(paste0('Results/Fig4_', country, 'CarbonForesttoCropAgro.csv'), stringsAsFactors=FALSE)
head(cropArea)
colnames(cropArea) <- c("FullTransition","Area_ha","SE_ha")

###################################################
##### change formatting of area information

# Update forest type.
cropArea$ForestType <- substr(cropArea$FullTransition,1,5)
if (country == 'Indonesia'){
cropArea$ForestType <- substr(cropArea$FullTransition,1,7)
cropArea$ForestType[substr(cropArea$ForestType,7,7) != 'p'] <- 
  substr(cropArea$FullTransition[substr(cropArea$ForestType,7,7) != 'p'],1,5)
}
cropArea <- cropArea[cropArea$ForestType != 'NotFo' & cropArea$ForestType != 'NotFo_p',]
unique(cropArea$ForestType)

# Update crop and agroforestry types.
cropArea$CropType <- substr(cropArea$FullTransition,10,13)
cropArea$AgroType <- substr(cropArea$FullTransition,15,18)
cropArea$AgroType[cropArea$CropType == 'Tea_'] <- 
  substr(cropArea$FullTransition[cropArea$CropType == 'Tea_'],14,17)
cropArea <- cropArea[cropArea$CropType != 'None',]
unique(cropArea$CropType)
unique(cropArea$AgroType[cropArea$AgroType!='Trad'])
cropArea$AgroType[cropArea$AgroType!='Trad']<-'Agro'
unique(cropArea$AgroType)

###################################################
##### merge carbon and area estimates

# merge crop carbon factors with area estimates.
dataAgro <- merge(x = cropArea, y = cropCarbon,
                  by.x = 'CropType', by.y = 'CropAbb')
head(dataAgro)
dataAgro$CropCFactor[dataAgro$AgroType == 'Trad'] <-
  dataAgro$CFactorTrad[dataAgro$AgroType=='Trad']
dataAgro$CropCFactor[dataAgro$AgroType != 'Trad'] <- 
  dataAgro$CFactorAgro[dataAgro$AgroType!='Trad']
head(dataAgro)
dataAgro<-dataAgro[,c("FullTransition", "Area_ha", "SE_ha", 
                      "ForestType","CropType", "CropLabel", "AgroType", "Country",       
                       "CropCFactor")]
head(dataAgro)
## remove input files.
rm(cropArea)
rm(cropCarbon)

###################################################
##### merge carbon and area estimates

# merge forest carbon factors with area estimates.
head(dataAgro)
head(forestCarbon)
fullCarbon <- merge(x = dataAgro, y = forestCarbon,
                  by.x = 'ForestType', by.y = 'Forest')
head(fullCarbon)
fullCarbon<-fullCarbon[,c("FullTransition", "Area_ha", "SE_ha", 
                      "ForestType","CropType", "CropLabel", "AgroType", "Country",       
                      "CropCFactor", "ForestCFactor")]
head(fullCarbon)

## remove input files.
rm(dataAgro)
rm(forestCarbon)


#######################################################
##### Table 2 #####
#######################################################
head(fullCarbon)
fullCarbon$ForestCarbon <- fullCarbon$Area_ha * fullCarbon$ForestCFactor
fullCarbon$CropCarbon <- fullCarbon$Area_ha * fullCarbon$CropCFactor

##### calculate forest carbon by forest type.
ForestTotalsAll = aggregate(fullCarbon$ForestCarbon, 
                            by=list(Category = fullCarbon$ForestType), FUN=sum)
colnames(ForestTotalsAll)<- c('Forest','Forest_tonnes_C')
#ForestTotalsAll$Forest_tonnes_C <- round(signif(ForestTotalsAll$Forest_tonnes_C, digits = 5))
ForestTotalsAll$Forest_tonnes_C_Label <- format(ForestTotalsAll$Forest_tonnes_C, big.mark=",", trim=TRUE)
ForestTotalsAll

##### calculate crop carbon by forest type.
CropTotalsAll = aggregate(fullCarbon$CropCarbon, 
                            by=list(Category = fullCarbon$ForestType), FUN=sum)
colnames(CropTotalsAll)<- c('Forest','Crop_tonnes_C')
#CropTotalsAll$Crop_tonnes_C <- round(signif(CropTotalsAll$Crop_tonnes_C, digits = 5))
CropTotalsAll$Crop_tonnes_C_Label <- format(CropTotalsAll$Crop_tonnes_C, big.mark=",", trim=TRUE)
CropTotalsAll

##### merge tables.
Table2All <- merge(ForestTotalsAll, CropTotalsAll, by.x ="Forest", by.y = "Forest") 
head(Table2All)

rm(ForestTotalsAll, CropTotalsAll)
##### Calculate difference between 2000 and current carbon storage.
Table2All$Diff_tonnes_C<-Table2All$Forest_tonnes_C-Table2All$Crop_tonnes_C
Table2All$Diff_tonnes_C_Label <- format(Table2All$Diff_tonnes_C, big.mark=",", trim=TRUE)

##### Format Table 2.
Table2All<-Table2All[, c("Forest", 
                                   "Forest_tonnes_C_Label", 
                                   "Crop_tonnes_C_Label", 
                                   "Diff_tonnes_C_Label")]
colnames(Table2All)<-c("Forest", 
                       "Forest_tonnes_C", 
                       "Crop_tonnes_C", 
                       "Diff_tonnes_C")
Table2All

Table2All$Forest <- recode(Table2All$Forest, 
                               "TRain" = 'Tropical rainforest', 
                               "TRain_p" = 'Tropical rainforest, peatlands', 
                               "TDryF" = 'Tropical dry forest', 
                               "TMDec" = 'Tropical moist deciduous forest',
                               "TMtn" = 'Tropical mountain system',
                               "SHum" = 'Subtropical humid forest',
                               "TDry" = 'Tropical dry forest',
                               "TShr" = 'Tropical shrubland')
Table2All

##### Export table 2.
write.csv(Table2All, paste0('Results/CarbonTables/Table2_', country, '_Forest_Tables.csv'), row.names=F)
rm(Table2All)

#######################################################
##### Table 3 #####
#######################################################
head(fullCarbon)
fullCarbon$CropAgro<-paste0(fullCarbon$CropType, '_',fullCarbon$AgroType)
unique(fullCarbon$CropAgro)

##### calculate crop carbon by agro type.
CropAgroTotalsAll = aggregate(fullCarbon$CropCarbon, 
                          by=list(Category = fullCarbon$CropAgro), FUN=sum)
colnames(CropAgroTotalsAll)<- c('CropAgro','Crop_tonnes_C')
CropAgroTotalsAll$Crop_tonnes_C <- round(CropAgroTotalsAll$Crop_tonnes_C)
CropAgroTotalsAll$Crop_tonnes_C_Label <- format(CropAgroTotalsAll$Crop_tonnes_C, big.mark=",", trim=TRUE)
#CropAgroTotalsAll<-CropAgroTotalsAll[,c(1,3)]
CropAgroTotalsAll

##### Store carbon factor by crop type.
CropAgroCFactorAll = aggregate(fullCarbon$CropCFactor, 
                              by=list(Category = fullCarbon$CropAgro), FUN=mean)
colnames(CropAgroCFactorAll)<- c('CropAgro','CropCFactor_tonnes_ha_C')
#CropAgroCFactorAll$Crop_tonnes_C <- round(signif(CropAgroCFactorAll$Crop_tonnes_C, digits = 5))
#CropAgroCFactorAll$Crop_tonnes_C_Label <- format(CropAgroCFactorAll$Crop_tonnes_C, big.mark=",", trim=TRUE)
CropAgroCFactorAll

##### merge tables.
head(CropAgroCFactorAll)
Table3All <- merge(CropAgroCFactorAll, CropAgroTotalsAll, by.x ="CropAgro", by.y = "CropAgro") 
head(Table3All)

rm(CropAgroCFactorAll, CropAgroTotalsAll)
##### Add keys to table 3.
Table3All$CropAbb <- substr(Table3All$CropAgro,1,4)
Table3All$Agro <- substr(Table3All$CropAgro,6,10)
head(Table3All)

##### Format Table 3 into agro and trad colums.
Table3_2Cols <- merge(x= Table3All[Table3All$Agro == 'Trad',], y = Table3All[Table3All$Agro == 'Agro',],
      by.x = 'CropAbb', by.y = 'CropAbb', all=T)
head(Table3_2Cols)

colnames(Table3_2Cols)<-c("CropAbb", "TradCropAgro", "TradCropCFactor_tonnes_ha_C",  
                          "TradCrop_tonnes_C","TradCrop_tonnes_C_Label", "TradAgro", 
                          "AgroCropAgro", "AgroCropCFactor_tonnes_ha_C",  
                          "AgroCrop_tonnes_C", "AgroCrop_tonnes_C_Label", "Agro.Agro")
head(Table3_2Cols)

##### Set up information for last table.
Table4_2Cols<-Table3_2Cols[, c('CropAbb', "TradCrop_tonnes_C","AgroCrop_tonnes_C")]

head(Table4_2Cols)

##### Remove extra columns for table 3.
Table3_2Cols<-Table3_2Cols[, c('CropAbb', 
                               "TradCropCFactor_tonnes_ha_C","TradCrop_tonnes_C_Label",
                               "AgroCropCFactor_tonnes_ha_C","AgroCrop_tonnes_C_Label")]
head(Table3_2Cols)

##### Format information for table 3.
Table3_2Cols$Crop <- recode(Table3_2Cols$CropAbb,
                         "Aqua" = 'Aquaculture',
                         "Bana" = 'Banana',
                         "Coco" = 'Coconut',
                         "Coff" = 'Coffee',
                         "Crop" = 'Crop support',
                         "Frui" = 'Fruit & nut',
                         "Herb" = 'Herbaceous crops',
                         "Oil_" = 'Oil palm',
                         "Palm" = 'Palm crops',
                         "Pulp" = 'Pulpwood',
                         "Rice" = 'Rice',
                         "Rubb" = 'Rubber',
                         "Shru" = 'Shrub crops',
                         "Tea_" = 'Tea',
                         "Tree" = 'Tree crops'
)

##### Format Table 3.
Table3 <- Table3_2Cols[, c("Crop", 
                                 "TradCropCFactor_tonnes_ha_C","TradCrop_tonnes_C_Label",
                                 "AgroCropCFactor_tonnes_ha_C","AgroCrop_tonnes_C_Label")]
Table3

##### Export table 3.
write.csv(Table3, paste0('Results/CarbonTables/Table3_', country, '_Crop_Tables.csv'), row.names=F)
rm(Table3_2Cols, Table3)

#######################################################
##### Table 4 #####
#######################################################
Table4_2Cols$AgroCrop_tonnes_C <- Table4_2Cols$AgroCrop_tonnes_C %>% replace_na(0)
Table4_2Cols$TradCrop_tonnes_C <- Table4_2Cols$TradCrop_tonnes_C %>% replace_na(0)
Table4_2Cols

##### add plant form
Table4_2Cols$CropForm <- recode(Table4_2Cols$CropAbb,
                            "Aqua" = 'Aqua',
                            "Bana" = 'Palm',
                            "Coco" = 'Palm',
                            "Coff" = 'Shrub',
                            "Crop" = 'Crop',
                            "Frui" = 'Tree',
                            "Herb" = 'Herb',
                            "Oil_" = 'Palm',
                            "Palm" = 'Palm',
                            "Pulp" = 'Tree',
                            "Rice" = 'Herb',
                            "Rubb" = 'Tree',
                            "Shru" = 'Shrub',
                            "Tea_" = 'Shrub',
                            "Tree" = 'Tree'
)
head(Table4_2Cols)

##### calculate crop carbon by agro type.
CropFormTradTotalsAll = aggregate(Table4_2Cols$TradCrop_tonnes_C, 
                              by=list(Category = Table4_2Cols$CropForm), FUN=sum)
colnames(CropFormTradTotalsAll)<- c('CropAgro','Crop_tonnes_C')
#CropFormTradTotalsAll$Crop_tonnes_C <- round(CropFormTradTotalsAll$Crop_tonnes_C)
CropFormTradTotalsAll$Crop_tonnes_C_Label <- format(CropFormTradTotalsAll$Crop_tonnes_C, big.mark=",", trim=TRUE)
CropFormTradTotalsAll<-CropFormTradTotalsAll[,c(1,3)]
CropFormTradTotalsAll

##### calculate crop carbon by agro type.
CropFormAgroTotalsAll = aggregate(Table4_2Cols$AgroCrop_tonnes_C, 
                                  by=list(Category = Table4_2Cols$CropForm), FUN=sum)
colnames(CropFormAgroTotalsAll)<- c('CropAgro','Crop_tonnes_C')
#CropFormAgroTotalsAll$Crop_tonnes_C <- round(CropFormAgroTotalsAll$Crop_tonnes_C)
CropFormAgroTotalsAll$Crop_tonnes_C_Label <- format(CropFormAgroTotalsAll$Crop_tonnes_C, big.mark=",", trim=TRUE)
CropFormAgroTotalsAll<-CropFormAgroTotalsAll[,c(1,3)]
CropFormAgroTotalsAll

Table4_2Cols <- merge(CropFormTradTotalsAll, CropFormAgroTotalsAll,
                      by.x = 'CropAgro', by.y = 'CropAgro')

colnames(Table4_2Cols) <- c( "CropAgro", "TradCrop_tonnes_C_Label", "AgroCrop_tonnes_C_Label")
Table4_2Cols

rm(CropFormTradTotalsAll, CropFormAgroTotalsAll)
##### Export table 4.
write.csv(Table4_2Cols, paste0('Results/CarbonTables/Table4_', country, '_CropForm_Tables.csv'), row.names=F)

rm(Table4_2Cols, fullCarbon, country)
rm(Table3All)
