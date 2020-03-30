setwd('C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\GIA')

#+ Packages
library(tidyverse)

#+ inputdata
# Data input and cleaning ------------------------------------------------------
dataPath <- c("data/Indonesia")
files <- dir(dataPath)

data <- files %>% 
  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))

columnNames<-colnames(data)

columnNames[18]<-'LAND_USE'
columnNames[15]<-'LAND_COVER'

colnames(data)<-columnNames

#PL_PACKETID = packet id (from Seth)
#PL_PACKETID = packet id (from Seth)
#PLOT_ID = CEO plot ID
#SAMPLE_ID = CEO sample ID

# if land cover == and land use ==
unique(data$LAND_COVER)
# Other_Tree, Bamboo, Rubber, Pulpwood, Fruit_Nut
# Tea, Coffee, Other_Shrub
# Herbaceous, Other_Crop, Water, Non_vegetated, Built_up, Other_LAND_COVER, Rice

unique(data$LAND_USE)
# Natural Forest, Terrace, Other, Mixed_Agrisilviculture, Plantation               
# Boundary Agrisilviculture, Agrisiviculture, Silvopastoral, Mixed Agrisilviculture
data<-as.matrix(data)
######### RULES FOR TREES
#Rubber, Pulpwood, Fruit_Nut, Oil Palm
dataTree1 <- filter(data, (LAND_COVER == 'Rubber' | LAND_COVER == 'Pulpwood') & 
                      (LAND_USE == 'Silvopastoral' | LAND_USE == 'Natural Forest' | LAND_USE == 'Other'))

dataTree2 <- filter(data, 
                    (LAND_COVER == 'Fruit/Nut' | LAND_COVER == 'Oil Palm' | LAND_COVER == 'Coconut' | LAND_COVER == 'Banana') & 
  (LAND_USE == 'Natural Forest'))

dataTeaCoff <- filter(data, (LAND_COVER == 'Tea' | LAND_COVER == 'Coffee') & 
                     (LAND_USE == 'Silvopastoral' | LAND_USE == 'Natural Forest' | LAND_USE == 'Plantation'))

dataRice <- filter(data, (LAND_COVER == 'Rice') & 
  (LAND_USE == 'Silvopastoral' | LAND_USE == 'Natural Forest' | LAND_USE == 'Plantation'| LAND_USE == 'Other'))

dataOther <- filter(data, (LAND_COVER == 'Other Crop') & 
  (LAND_USE == 'Silvopastoral' | LAND_USE == 'Natural Forest' | LAND_USE == 'Plantation'))

dataBU <- filter(data, (LAND_COVER == 'Built-up') & 
  (LAND_USE == 'Terrace' | LAND_USE == 'Natural Forest' | LAND_USE == 'Plantation'))

dataIncomp <- rbind(dataTree1, dataTree2, dataTeaCoff, dataRice, dataOther, dataBU)
write.csv(dataIncomp, file = 'INDincompatibleUsesSubset.csv', row.names = F)

dataIncomp2 <- distinct(dataIncomp, PLOT_ID, PL_PACKETID)
dataIncomp3 <- filter(data, PLOT_ID %in% dataIncomp2$PLOT_ID)
head(dataIncomp3)

write.csv(dataIncomp3, file = '\\data\\Indonesia\\INDincompatibleUses.csv', row.names = F)

# Other_Palm, aquaculture
# Other_Tree, Bamboo, Other_Shrub
# Herbaceous, Water, Non_vegetated, Other_LAND_COVER
#dataSubset <- data[(data$LAND_COVER == 'Bamboo'|data$LAND_COVER == 'Other_Tree' | data$LAND_COVER == 'Other_Shrub') & 
#                     (data$LAND_USE == 'Silvopastoral' | data$LAND_USE == 'Natural_Forest' | data$LAND_USE == 'Other_LAND_USE'), 
#                   c('PLOT_ID', 'SAMPLE_ID', 'PL_PACKETID')]
