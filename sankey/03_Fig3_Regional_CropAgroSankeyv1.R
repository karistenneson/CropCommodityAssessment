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

######################################################
################## Regional Sankey ##################
######################################################
setwd("C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\Final\\GIA")

country <- 'Regional'

# load crop conversion data.
#cropType <- read.csv('Results/Fig3_IndoForesttoCrop.csv')
cropConv <- read.csv(paste0('Results/Fig3_', country, 'ForesttoCrop.csv'))
cropConv$X<-as.character(cropConv$X)
cropConv$level<-'Crop'
head(cropConv)

# add in crop to agroforestry
#peat <- read.csv('Results/Fig3_IndoCroptoAgro.csv')
cropAgro <- read.csv(paste0('Results/Fig3_', country, 'CroptoAgro.csv'))
cropAgro$X <- as.character(cropAgro$X)
cropAgro$level<-'Agro'
head(cropAgro)

dataAll<- rbind(cropConv, cropAgro)
dataAll$X<-as.character(dataAll$X)
#######
head(dataAll)

## remove input files.
rm(cropConv)
rm(cropAgro)

######################################################
###########################
## remove non-crops
dataAll$source <- substr(dataAll$X,1,7)
dataAll$source[dataAll$source != 'TMtnS_p' & dataAll$source != 'TRain_p'] <- 
  substr(dataAll$X[dataAll$source != 'TMtnS_p' & dataAll$source != 'TRain_p'],1,4)

unique(dataAll$source)

###########################
## remove non crops
dataAll<-dataAll[dataAll$source != 'NotF',]
dataAll<-dataAll[dataAll$source != 'None',]
unique(dataAll$source)

###########################
## remove non crops
dataAll$target <- substr(dataAll$X,6,10)
dataAll$target[dataAll$target != 'Agris' & dataAll$target != 'Bndry' & dataAll$target != 'Trad'] <- 
  substr(dataAll$X[dataAll$target != 'Agris' & dataAll$target != 'Bndry' & dataAll$target != 'Trad'],
         13,24)
dataAll$target[substr(dataAll$X,1,3)=='Tea'] <- substr(dataAll$X[substr(dataAll$X,1,3)=='Tea'],5,10)
unique(dataAll$target)

dataAll<-dataAll[dataAll$target != 'None',]
unique(dataAll$target)
unique(dataAll$X)

###########################
## calculate source total areas and add to the chart labels.
#dataAll$source <- substr(dataAll$X,1,4)
#unique(dataAll$source)

SourceTotalsAll = aggregate(dataAll$Area_ha, by=list(Category = dataAll$source), FUN=sum)
colnames(SourceTotalsAll) <- c("name", "Source_Total")
SourceTotalsAll

SourceTotalsAll$Source_Total[SourceTotalsAll$name == 'Coff']<-74985
SourceTotalsAll$Source_Total[SourceTotalsAll$name == 'Bana']<-1896
SourceTotalsAll$Source_Total[SourceTotalsAll$name == 'Rice']<-33541
SourceTotalsAll$Source_Total[SourceTotalsAll$name == 'Tea_']<-85742

SourceTotalsAll$Source_Total <- round(signif(SourceTotalsAll$Source_Total, digits = 5))
SourceTotalsAll$Source_Total <- format(SourceTotalsAll$Source_Total, big.mark=",", trim=TRUE)
SourceTotalsAll

# Merge the source total column with the database.
dataAll <- merge(dataAll, SourceTotalsAll, by.x ="source", by.y = "name") 
head(dataAll)

###########################
## format source label names
unique(dataAll$source)

dataAll$sourceLabels <- recode(dataAll$source, 
                            "Aqua" = paste0('Aquaculture', ' (', dataAll$Source_Total, ')'), 
                            "Coco" = paste0('Coconut', ' (', dataAll$Source_Total, ')'), 
                            "Coff" = paste0('Coffee', ' (', dataAll$Source_Total, ')'), 
                            "Crop" = paste0('Crop support', ' (', dataAll$Source_Total, ')'), 
                            "Frui" = paste0('Fruit & nut', ' (', dataAll$Source_Total, ')'), 
                            "Herb" = paste0('Herbaceous crops', ' (', dataAll$Source_Total, ')'), 
                            "Oil_" = paste0('Oil palm', ' (', dataAll$Source_Total, ')'), 
                            "Palm" = paste0('Palm crops', ' (', dataAll$Source_Total, ')'), 
                            "Pulp" = paste0('Pulpwood', ' (', dataAll$Source_Total, ')'), 
                            "Rice" = paste0('Rice', ' (', dataAll$Source_Total, ')'), 
                            "Rubb" = paste0('Rubber', ' (', dataAll$Source_Total, ')'), 
                            "Shru" = paste0('Shrub crops', ' (', dataAll$Source_Total, ')'), 
                            "TMDe" = paste0('Tropical moist deciduous forest', ' (', dataAll$Source_Total, ')'), 
                            "TMtn" = paste0('Tropical mountain system', ' (', dataAll$Source_Total, ')'), 
                            "TMtnS_p" = paste0('Tropical mountain system, peatlands', ' (', dataAll$Source_Total, ')'),
                            "TRai" = paste0('Tropical rainforest', ' (', dataAll$Source_Total, ')'), 
                            "TRain_p" = paste0('Tropical rainforest, peatlands', ' (', dataAll$Source_Total, ')'), 
                            "Tree" = paste0('Tree crops', ' (', dataAll$Source_Total, ')'), 
                            
                            "Bana" = paste0('Banana', ' (', dataAll$Source_Total, ')'), 
                            "SHum" = paste0('Subtropical humid forest', ' (', dataAll$Source_Total, ')'), 
                            "TDry" = paste0('Tropical dry forest', ' (', dataAll$Source_Total, ')'), 
                            "Tea_" = paste0('Tea', ' (', dataAll$Source_Total, ')'), 
                            "TShr" = paste0('Tropical shrubland', ' (', dataAll$Source_Total, ')')
                            )

unique(dataAll$sourceLabels)
head(dataAll)

######################################################
###########################
## create target
dataAll$target[dataAll$level == 'Crop'] <- substr(dataAll$X[dataAll$level == 'Crop'],13,24)
unique(dataAll$target[dataAll$level == 'Crop'])

dataAll$target[dataAll$level == 'Agro'] <- dataAll$X[dataAll$level == 'Agro']
unique(dataAll$target[dataAll$level == 'Agro'])
unique(dataAll$target)

## Recode the agroforestry labels into human readable labels.
dataAll$target[dataAll$level == 'Agro'] <- recode(dataAll$target[dataAll$level == 'Agro'],
                                            "Aqua_Trad" = 'Aquaculture, monoculture',
                                            "Coco_Agrisil" = 'Coconut, agrisilviculture',
                                            "Coco_Trad" = 'Coconut, monoculture',
                                            "Coff_Agrisil" = 'Coffee, agrisilviculture',
                                            "Coff_Bndry" = 'Coffee, boundary tree',
                                            "Coff_Trad" = 'Coffee, monoculture',
                                            "Crop_Agrisil" = 'Crop support, agrisilviculture',
                                            "Crop_Bndry"   = 'Crop support, boundary tree',
                                            "Crop_Trad" = 'Crop support, monoculture',
                                            "Frui_Agrisil" = 'Fruit & nut, agrisilviculture',
                                            "Frui_Bndry" = 'Fruit & nut, boundary tree',
                                            "Frui_Trad" = 'Fruit & nut, monoculture',
                                            "Herb_Agrisil" = 'Herbaceous crops, agrisilviculture',
                                            "Herb_Bndry" = 'Herbaceous crops, boundary tree',
                                            "Herb_Trad"    = 'Herbaceous crops, monoculture',
                                            "Oil__Agrisil" = 'Oil palm, agrisilviculture',
                                            "Oil__Bndry" = 'Oil palm, boundary tree',
                                            "Oil__Trad" = 'Oil palm, monoculture',
                                            "Palm_Agrisil" = 'Palm crop, agrisilviculture',
                                            "Palm_Trad" = 'Palm crop, monoculture',
                                            "Pulp_Agrisil" = 'Pulpwood, agrisilviculture',
                                            "Pulp_Trad" = 'Pulpwood, monoculture',
                                            "Rice_Trad" = 'Rice, monoculture',
                                            "Rubb_Agrisil" = 'Rubber, agrisilviculture',
                                            "Rubb_Bndry" = 'Rubber, boundary tree',
                                            "Rubb_Trad" = 'Rubber, monoculture',
                                            "Shru_Agrisil" = 'Shrub crop, agrisilviculture',
                                            "Shru_Bndry" = 'Shrub crop, boundary tree',
                                            "Shru_Trad" = 'Shrub crop, monoculture',
                                            
                                            "Bana_Agrisil" = 'Banana, agrisilviculture',
                                            "Bana_Trad" = 'Banana, monoculture',
                                            "Bana_Bndry" = 'Banana, boundary tree',
#                                            "Shrub_Agrisil" = 'Shrub crops, agrisilviculture',
#                                            "Shrub_Bndry" = 'Shrub crops, boundary tree',
#                                            "Shrub_Trad" = 'Shrub crops, monoculture',
                                            "Tea_Agrisil" = 'Tea, agrisilviculture',
                                            "Tea_Bndry" = 'Tea, boundary tree',
                                            "Tea_Trad" = 'Tea, monoculture',
                                            "Tree_Agrisil" = 'Tree crops, agrisilviculture',
                                            "Tree_Bndry" = 'Tree crops, boundary tree',
                                            "Tree_Trad" = "Tree crops, monoculture"
)

unique(dataAll$target)

###########################
## calculate target total areas
TargetTotalsAll = aggregate(dataAll$Area_ha, by=list(Category = dataAll$target), FUN=sum)
colnames(TargetTotalsAll) <- c("name", "Target_Total")
TargetTotalsAll
TargetTotalsAll$Target_Total <- round(signif(TargetTotalsAll$Target_Total, digits = 5))
TargetTotalsAll$Target_Total <- format(TargetTotalsAll$Target_Total, big.mark=",", trim=TRUE)
TargetTotalsAll

# create link group to use to color the sankey chart links
TargetTotalsAll$LinkGroup<-TargetTotalsAll$name
TargetTotalsAll$LinkGroup[substr(TargetTotalsAll$name,1,3) %in% c('Cro')]<-'Crop_support'
TargetTotalsAll$LinkGroup[substr(TargetTotalsAll$name,1,3) %in% c('Shr','Cof', 'Tea')]<-'Shrub'
TargetTotalsAll$LinkGroup[substr(TargetTotalsAll$name,1,3) %in% c('Aqu',"Her", "Ric")]<-'Herb'
TargetTotalsAll$LinkGroup[substr(TargetTotalsAll$name,1,3) %in% c('Coc','Ban',"Oil", "Pal")]<-'Palm'
TargetTotalsAll$LinkGroup[substr(TargetTotalsAll$name,1,3) %in% c("Fru", "Pul", "Rub", "Tre")]<-'Tree'
unique(TargetTotalsAll$LinkGroup)

# Merge the target total column with the database.
dataAll <- merge(dataAll, TargetTotalsAll, by.x ="target", by.y = "name") 
## write in the agrisilv label to link group.
dataAll$LinkGroup[dataAll$level == 'Agro'] <- substr(dataAll$X[dataAll$level == 'Agro'],6,24)
dataAll$LinkGroup[dataAll$level == 'Agro' & dataAll$LinkGroup == 'grisil']<-'Agrisil'
dataAll$LinkGroup[dataAll$level == 'Agro' & dataAll$LinkGroup == 'rad']<-'Trad'
dataAll$LinkGroup[dataAll$level == 'Agro' & dataAll$LinkGroup == 'ndry']<-'Bndry'
unique(dataAll$LinkGroup)

head(dataAll)
unique(dataAll$LinkGroup[dataAll$level != 'Crop'])
unique(dataAll$LinkGroup[dataAll$level == 'Crop'])
dataAll$LinkGroup<-as.factor(dataAll$LinkGroup)

###########################
## format target label names
unique(dataAll$target)
dataAll$targetLabels <- paste0(str_replace_all(dataAll$target, "_", " "), ' (', dataAll$Target_Total, ')')
dataAll$targetLabels[dataAll$level == 'Agro']<-paste0(dataAll$target[dataAll$level == 'Agro'], 
                                                ' (', dataAll$Target_Total[dataAll$level == 'Agro'], ')')
dataAll$targetLabels[dataAll$level != 'Agro'] <- recode(dataAll$target[dataAll$level != 'Agro'], 
                                                  "Aqua" = paste0('Aquaculture', ' (', 
                                                                         dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Bana" = paste0('Banana', ' (', 
                                                                  dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Coco" = paste0('Coconut', ' (', 
                                                                          dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Coff" = paste0('Coffee', ' (', 
                                                                  dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Crop" = paste0('Crop support', ' (', 
                                                                    dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Frui" = paste0('Fruit & nut', ' (', 
                                                                            dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Herb" = paste0('Herbaceous crops', ' (', 
                                                                       dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Oil_" = paste0('Oil palm', ' (', 
                                                                      dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Palm" = paste0('Palm crops', ' (', 
                                                                       dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Pulp" = paste0('Pulpwood', ' (', 
                                                                           dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Rice" = paste0('Rice', ' (', 
                                                                       dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Rubb" = paste0('Rubber', ' (', 
                                                                         dataAll$Target_Total[dataAll$level != 'Agro'], ')'),
                                                  "Shru" = paste0('Shrub crops', ' (', 
                                                                        dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Tea" = paste0('Tea', ' (', 
                                                                  dataAll$Target_Total[dataAll$level != 'Agro'], ')'), 
                                                  "Tree" = paste0('Tree crops', ' (', 
                                                                       dataAll$Target_Total[dataAll$level != 'Agro'], ')')
                                                  )

sort(unique(dataAll$targetLabels))
head(dataAll)

###########################
## Export table for Appendices.
write.csv(dataAll[,c('sourceLabels','targetLabels','Area_ha', 'SE_Area_ha', 'LinkGroup')],paste0('Results/Fig3_Final/Fig3_', country, '_Formatted_Table.csv'), row.names=F)

#####################################################################################
################################################################################
## Create Sankey
################################################################################
## Resources on how to customize Sankey colors:
## https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram/
## https://stackoverflow.com/questions/49294056/how-to-set-the-color-of-nodes-in-the-sankey-plot-using-the-networkd3-library?rq=1
################################################################################

###########################
## Create nodesAll and connect key to data frame.
nodesAll = data.frame(
  name=c(as.character(dataAll$sourceLabels), as.character(dataAll$targetLabels)) %>% unique())
dataAll$IDsource=match(dataAll$sourceLabels, nodesAll$name)-1 
dataAll$IDtarget=match(dataAll$targetLabels, nodesAll$name)-1
sort(nodesAll$name)

###########################
## Add a 'group' column to the nodes data frame for the crop structure:
nodesAll$group <- recode(substr(nodesAll$name, 1, 6), 
                      "Agrisi" = "AgSil",
                      "Aquacu" = "HCrop",
                      "Banana" = "PCrop",
                      "Bounda" = "Bndry",
                      "Coconu" = "PCrop",
                      "Coffee" = "SCrop",
                      "Crop s" = "CropSup",
                      "Fruit " = "TCrop",
                      "Herbac" = "HCrop",
                      "Monocu" = "Mono",
                      "Oil pa" = "PCrop",
                      "Palm c" = "PCrop",
                      "Pulpwo" = "TCrop",
                      "Rice (" = "HCrop",
                      "Rice, " = "HCrop",
                      "Rubber" = "TCrop",
                      "Shrub " = "SCrop",
                      "Tea (8" = "SCrop",
                      "Tea (1" = "SCrop",
                      "Tea, b" = "SCrop",
                      "Tea, a" = "SCrop",
                      "Tea, m" = "SCrop",
                      "Tree c" = "TCrop",
                      "Tropic" = "Dec",
                      "Subtro" = "SubT")
nodesAll$group[substr(nodesAll$name, 1, 12)=="Tropical mou"] <- "TMS"
nodesAll$group[substr(nodesAll$name, 1, 12)=="Tropical rai"] <- "TR"
nodesAll$group[substr(nodesAll$name, 1, 12)=="Tropical dry"] <- "TDry"
unique(nodesAll$group)

## Add a 'group' column to the nodesAll data frame for the agroforestry system:
hits<-grep("agrisilviculture", nodesAll$name)
nodesAll$group[hits] <- 'Silvi'
hits2<-grep("mono", nodesAll$name)
nodesAll$group[hits2] <- 'Mono'
hits3<-grep("boundary", nodesAll$name)
nodesAll$group[hits3] <- 'Boundary'

nodesAll$group<-as.factor(nodesAll$group)

unique(nodesAll$group)

#################################################################################
#################################################################################
### sync, sync
#################################################################################
#################################################################################


###########################
unique(dataAll$LinkGroup)
unique(nodesAll$group)

###########################
##  Give a color for each group:
my_colorNoLink <- paste0('d3.scaleOrdinal() .domain([', 
                         # Now add in node labels
                         '"TR", "TMS", "Dec", "SubT", "TShrb",   "TDry",   
                          "TCrop", "PCrop", "SCrop", "HCrop", "CropSup",  
                         "Boundary", "Mono", "Silvi",
                         "Herb", "Palm","Trad", "Agrisil", "Shrub", "Bndry", "Crop_support", "Tree"',
                         ']) .range([', 
                         #add in node colors
                         #'"TR",      "TMS",     "Dec",   "SubT",   "TShrb",   "TDryF",   
                         '"darkgreen","#F4A460","#FFFDD0","#E3FF00", "#B76E79",   "#D28386",',   
                         #'"TCrop","PCrop","SCrop","HCrop", "CropSup",     
                         '"#5E1914","#BE5504","gold","yellow","#996515",',
                         #"Boundary", "Mono", "Silvi"
                         '"purple", "grey",   "darkgreen",',
                         #"Herb", 'Palm',  "Trad",      "Agrisil","Shrub", "Bndry", "Crop_support","Tree"',
                         '"#FFD300","#BE5504", "lightgrey","#B8BC86","gold",  "#E54ED0", "#996515",     "#A45A52"',
                         '])')

###########################
## Make the network with color code links:
sankeyPlot<- sankeyNetwork(Links = dataAll, Nodes = nodesAll, Source = "IDsource", Target = "IDtarget", Value = "Area_ha", 
                           NodeID = "name", colourScale=my_colorNoLink,
                           LinkGroup = "LinkGroup", 
                           NodeGroup="group", fontSize = 14, nodeWidth = 20,fontFamily = "Arial")
sankeyPlot

