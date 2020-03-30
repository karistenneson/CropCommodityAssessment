setwd('C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\GIA')

data<-read.csv('data/Compiled/Vietnam_points.csv')
colnames(data)

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
# Natural_Forest            Terrace                   Other_LAND_USE            Mixed_Agrisilviculture    Plantation               
# Boundary_Agrisilviculture Agrisiviculture           Silvopastoral

######### RULES FOR TREES
#Rubber, Pulpwood, Fruit_Nut, Oil Palm
dataTree1 <- data[(data$LAND_COVER == 'Rubber' | data$LAND_COVER == 'Pulpwood') & 
                  (data$LAND_USE == 'Silvopastoral' | data$LAND_USE == 'Natural_Forest' | data$LAND_USE == 'Other_LAND_USE'), 
                  c('LON', 'LAT', 'PLOT_ID', 'SAMPLE_ID', 'PL_PACKETID')]

dataTree2 <- data[
  (data$LAND_COVER == 'Fruit_Nut' | data$LAND_COVER == 'Oil_Palm' | data$LAND_COVER == 'Coconut' | data$LAND_COVER == 'Banana') & 
  (data$LAND_USE == 'Natural_Forest'), 
  c('LON', 'LAT', 'PLOT_ID', 'SAMPLE_ID', 'PL_PACKETID')]

dataTeaCoff <- data[(data$LAND_COVER == 'Tea' | data$LAND_COVER == 'Coffee') & 
                     (data$LAND_USE == 'Silvopastoral' | data$LAND_USE == 'Natural_Forest' | data$LAND_USE == 'Plantation'), 
                   c('LON', 'LAT', 'PLOT_ID', 'SAMPLE_ID', 'PL_PACKETID')]

dataRice <- data[
  (data$LAND_COVER == 'Rice') & 
  (data$LAND_USE == 'Silvopastoral' | data$LAND_USE == 'Natural_Forest' | data$LAND_USE == 'Plantation'| data$LAND_USE == 'Other_LAND_USE'), 
  c('LON', 'LAT', 'PLOT_ID', 'SAMPLE_ID', 'PL_PACKETID')]

dataOther <- data[
  (data$LAND_COVER == 'Other_Crop') & 
  (data$LAND_USE == 'Silvopastoral' | data$LAND_USE == 'Natural_Forest' | data$LAND_USE == 'Plantation'), 
  c('LON', 'LAT', 'PLOT_ID', 'SAMPLE_ID', 'PL_PACKETID')]

dataBU <- data[
  (data$LAND_COVER == 'Built_up') & 
  (data$LAND_USE == 'Terrace' | data$LAND_USE == 'Natural_Forest' | data$LAND_USE == 'Plantation'), 
  c('LON', 'LAT', 'PLOT_ID', 'SAMPLE_ID', 'PL_PACKETID')]

dataIncomp <- rbind(dataTree1, dataTree2, dataTeaCoff, dataRice, dataOther, dataBU)
write.csv(dataIncomp, file = 'VNincompatibleUsesSubset.csv', row.names = F)

dataIncomp2 <- unique(dataIncomp[ , c('PLOT_ID', 'PL_PACKETID')])
dataIncomp3 <- data[data[,'PLOT_ID'] %in% dataIncomp2$PLOT_ID,]
head(dataIncomp3)

write.csv(dataIncomp3, file = '\\data\\Vietnam\\VNincompatibleUses.csv', row.names = F)

# Other_Tree, Bamboo, Other_Shrub
# Herbaceous, Water, Non_vegetated, Other_LAND_COVER
#dataSubset <- data[(data$LAND_COVER == 'Bamboo'|data$LAND_COVER == 'Other_Tree' | data$LAND_COVER == 'Other_Shrub') & 
#                     (data$LAND_USE == 'Silvopastoral' | data$LAND_USE == 'Natural_Forest' | data$LAND_USE == 'Other_LAND_USE'), 
#                   c('PLOT_ID', 'SAMPLE_ID', 'PL_PACKETID')]
