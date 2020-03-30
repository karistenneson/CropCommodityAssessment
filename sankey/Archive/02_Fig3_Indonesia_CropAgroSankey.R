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

setwd("C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\GIA\\GIA")

#' ### Required packages
#+ Packages
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)
library(networkD3)
library(tidyverse)

######################################################
################## Indonesia Sankey ##################
######################################################

peat <- read.csv('Results/Fig3_IndopeattoCrop.csv')
peat$level<-'ecofloristic'
cropType <- read.csv('Results/Fig3_CroptoAgro.csv')
cropType$level<-'crop'

data<- rbind(peat, cropType)

## check X categories transfered
data$X<-as.character(data$X)
peat$X<-as.character(peat$X)
cropType$X <- as.character(cropType$X)

## another data check.
head(data)
head(peat)
head(cropType)

## remove input files.
rm(peat)
rm(cropType)

######################################################
###########################
## create source
data$source <- substr(data$X,1,7)
unique(data$source)
unique(data$X)

###########################
## remove non crops
data<-data[data$source != 'Other_N',]
data<-data[data$source != 'TrShrub',]
data<-data[data$source != 'None_No',]
unique(data$source)

###########################
data$target <- substr(data$X,9,24)
unique(data$target)
## remove non crops
data<-data[data$target != 'None',]

###########################
## calculate source total areas and add to the chart labels.
unique(data$source)

SourceTotals = aggregate(data$Area_ha, by=list(Category = data$source), FUN=sum)
colnames(SourceTotals) <- c("name", "Source_Total")
SourceTotals
SourceTotals$Source_Total <- round(signif(SourceTotals$Source_Total, digits = 5)/1000)
SourceTotals$Source_Total <- format(SourceTotals$Source_Total, big.mark=",", trim=TRUE)
SourceTotals

# Merge the source total column with the data base.
data <- merge(data, SourceTotals, by.x ="source", by.y = "name") 
head(data)

###########################
## format source label names
unique(data$source)

data$sourceLabels <- recode(data$source, 
                            "Crop_su" = paste0('Crop support', ' (', data$Source_Total, ')'), 
                            "Herb_cr" = paste0('Herbaceous crops', ' (', data$Source_Total, ')'), 
                            "Aquacul" = paste0('Aquaculture', ' (', data$Source_Total, ')'), 
                            "Rice_cr" = paste0('Rice', ' (', data$Source_Total, ')'), 
                            "Shrub_c" = paste0('Shrub crops', ' (', data$Source_Total, ')'), 
                            "Coffee_" = paste0('Coffee', ' (', data$Source_Total, ')'), 
                            "Palm_cr" = paste0('Palm crops', ' (', data$Source_Total, ')'), 
                            "Coconut" = paste0('Coconut', ' (', data$Source_Total, ')'), 
                            "Oil_pal" = paste0('Oil palm', ' (', data$Source_Total, ')'), 
                            "Tree_cr" = paste0('Tree crops', ' (', data$Source_Total, ')'), 
                            "Fruit_n" = paste0('Fruit & nut orchards', ' (', data$Source_Total, ')'), 
                            "Pulpwoo" = paste0('Pulpwood', ' (', data$Source_Total, ')'), 
                            "Rubber_" = paste0('Rubber', ' (', data$Source_Total, ')'), 
                            "Rainfor" = paste0('Tropical rainforest', ' (', data$Source_Total, ')'), 
                            "RainfPt" = paste0('Tropical rainforest, peatlands', ' (', data$Source_Total, ')'), 
                            "TrDecid" = paste0('Tropical moist deciduous forest', ' (', data$Source_Total, ')'), 
                            "TrMount" = paste0('Tropical mountain system', ' (', data$Source_Total, ')'), 
                            "TrMouPt" = paste0('Tropical mountain system, peatlands', ' (', data$Source_Total, ')')
)
unique(data$sourceLabels)
head(data)

######################################################
###########################
## create target
data$target <- substr(data$X,9,24)
data$target[data$level == 'crop'] <- substr(data$X[data$level == 'crop'],1,24)
unique(data$target)
data$target[data$level == 'crop'] <- recode(data$target[data$level == 'crop'],
                      "Aquacul_Trad" = 'Aquaculture, monoculture',
                      "Coconut_Agrisil" = 'Coconut, agrisilviculture',
                      "Coconut_Trad" = 'Coconut, monoculture',
                      "Coffee__Agrisil" = 'Coffee, agrisilviculture',
                      "Coffee__Trad" = 'Coffee, monoculture',
                      "Crop_su_Agrisil" = 'Crop support, agrisilviculture',
                      "Crop_su_Bndry"   = 'Crop support, boundary tree',
                      "Crop_su_Trad" = 'Crop support, monoculture',
                      "Fruit_n_Agrisil" = 'Fruit & nut, agrisilviculture',
                      "Fruit_n_Bndry" = 'Fruit & nut, boundary tree',
                      "Fruit_n_Trad" = 'Fruit & nut, monoculture',
                      "Herb_cr_Agrisil" = 'Herbaceous crops, agrisilviculture',
                      "Herb_cr_Bndry" = 'Herbaceous crops, boundary tree',
                      "Herb_cr_Trad"    = 'Herbaceous crops, monoculture',
                      "Oil_pal_Agrisil" = 'Oil palm, agrisilviculture',
                      "Oil_pal_Bndry" = 'Oil palm, boundary tree',
                      "Oil_pal_Trad" = 'Oil palm, monoculture',
                      "Palm_cr_Agrisil" = 'Palm crop, agrisilviculture',
                      "Palm_cr_Trad" = 'Palm crop, monoculture',
                      "Pulpwoo_Agrisil" = 'Pulpwood, agrisilviculture',
                      "Pulpwoo_Trad" = 'Pulpwood, monoculture',
                      "Rice_cr_Trad" = 'Rice, monoculture',
                      "Rubber__Trad" = 'Rubber, monoculture',
                      "Shrub_c_Agrisil" = 'Shrub crops, agrisilviculture',
                      "Shrub_c_Bndry" = 'Shrub crops, boundary tree',
                      "Shrub_c_Trad" = 'Shrub crops, monoculture',
                      "Tree_cr_Agrisil" = 'Tree crops, agrisilviculture',
                      "Tree_cr_Bndry" = 'Tree crops, boundary tree',
                      "Tree_cr_Trad" = "Tree crops, monoculture"
                      )
unique(data$target)

###########################
## calculate target total areas
TargetTotals = aggregate(data$Area_ha, by=list(Category = data$target), FUN=sum)
colnames(TargetTotals) <- c("name", "Target_Total")
TargetTotals
TargetTotals$Target_Total <- round(signif(TargetTotals$Target_Total, digits = 5)/1000)
TargetTotals$Target_Total <- format(TargetTotals$Target_Total, big.mark=",", trim=TRUE)
TargetTotals

# create link group to use to color the sankey chart links
TargetTotals$LinkGroup<-TargetTotals$name
TargetTotals$LinkGroup[substr(TargetTotals$name,1,3) %in% c('Cro')]<-'Crop_support'
TargetTotals$LinkGroup[substr(TargetTotals$name,1,3) %in% c('Shr','Cof')]<-'Shrub'
TargetTotals$LinkGroup[substr(TargetTotals$name,1,3) %in% c('Aqu',"Her", "Ric")]<-'Herb'
TargetTotals$LinkGroup[substr(TargetTotals$name,1,3) %in% c('Coc',"Oil", "Pal")]<-'Palm'
TargetTotals$LinkGroup[substr(TargetTotals$name,1,3) %in% c("Fru", "Pul", "Rub", "Tre")]<-'Tree'
unique(TargetTotals$LinkGroup)

# Merge the target total column with the data base.
data <- merge(data, TargetTotals, by.x ="target", by.y = "name") 
## write in the agrisilv label to link group.
data$LinkGroup[data$level == 'crop'] <- substr(data$X[data$level == 'crop'],9,24)
head(data)
unique(data$LinkGroup)

unique(data$LinkGroup[data$level != 'crop'])
unique(data$LinkGroup[data$level == 'crop'])
data$LinkGroup<-as.factor(data$LinkGroup)

###########################
## format target label names
unique(data$target)
#data$targetLabels <- paste0(str_replace_all(data$target, "_", " "), ' (', data$Target_Total, ')')
data$targetLabels[data$level == 'crop']<-paste0(data$target[data$level == 'crop'], 
                                                ' (', data$Target_Total[data$level == 'crop'], ')')
unique(data$targetLabels)
data$targetLabels[data$level != 'crop'] <- recode(data$target[data$level != 'crop'], 
                            "Crop_support" = paste0('Crop support', ' (', 
                                                    data$Target_Total[data$level != 'crop'], ')'), 
                            "Herb_crop" = paste0('Herbaceous crops', ' (', 
                                                 data$Target_Total[data$level != 'crop'], ')'), 
                            "Aquaculture" = paste0('Aquaculture', ' (', 
                                                   data$Target_Total[data$level != 'crop'], ')'), 
                            "Rice_crop" = paste0('Rice', ' (', 
                                                 data$Target_Total[data$level != 'crop'], ')'), 
                            "Shrub_crop" = paste0('Shrub crops', ' (', 
                                                  data$Target_Total[data$level != 'crop'], ')'), 
                            "Coffee_crop" = paste0('Coffee', ' (', 
                                                   data$Target_Total[data$level != 'crop'], ')'), 
                            "Palm_crop" = paste0('Palm crops', ' (', 
                                                 data$Target_Total[data$level != 'crop'], ')'), 
                            "Coconut_crop" = paste0('Coconut', ' (', 
                                                    data$Target_Total[data$level != 'crop'], ')'), 
                            "Oil_palm" = paste0('Oil palm', ' (', 
                                                data$Target_Total[data$level != 'crop'], ')'), 
                            "Tree_crop" = paste0('Tree crops', ' (', 
                                                 data$Target_Total[data$level != 'crop'], ')'), 
                            "Fruit_nut_crop" = paste0('Fruit & nut orchards', ' (', 
                                                      data$Target_Total[data$level != 'crop'], ')'), 
                            "Pulpwood_crop" = paste0('Pulpwood', ' (', 
                                                     data$Target_Total[data$level != 'crop'], ')'), 
                            "Rubber_crop" = paste0('Rubber', ' (', 
                                                   data$Target_Total[data$level != 'crop'], ')')
                            )

sort(unique(data$targetLabels))
head(data)

###########################
## Export table for Appendices.
write.csv(data[,c('sourceLabels','targetLabels','Area_ha', 'SE_Area_ha')],'Results/Fig3_Final/Fig3_Indonesia_Formatted_Table.csv', row.names=F)

#####################################################################################
################################################################################
## Create Sankey
################################################################################
## Resources on how to customize Sankey colors:
## https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram/
## https://stackoverflow.com/questions/49294056/how-to-set-the-color-of-nodes-in-the-sankey-plot-using-the-networkd3-library?rq=1
################################################################################

###########################
## Create nodes and connect key to data frame.
nodes = data.frame(
  name=c(as.character(data$sourceLabels), as.character(data$targetLabels)) %>% unique())
data$IDsource=match(data$sourceLabels, nodes$name)-1 
data$IDtarget=match(data$targetLabels, nodes$name)-1
sort(nodes$name)

###########################
## Add a 'group' column to the nodes data frame for the crop structure:
nodes$group <- recode(substr(nodes$name, 1, 6), 
                            "Agrisi" = "AgSil",
                            "Aquacu" = "HCrop",
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
                            "Tree c" = "TCrop",
                            "Tropic" = "Dec")
nodes$group[substr(nodes$name, 1, 12)=="Tropical mou"] <- "TMS"
nodes$group[substr(nodes$name, 1, 12)=="Tropical rai"] <- "TR"

## Add a 'group' column to the nodes data frame for the agroforestry system:
hits<-grep("agrisilviculture", nodes$name)
nodes$group[hits] <- 'Silvi'
hits2<-grep("mono", nodes$name)
nodes$group[hits2] <- 'Mono'
hits3<-grep("boundary", nodes$name)
nodes$group[hits3] <- 'Boundary'

nodes$group<-as.factor(nodes$group)

unique(nodes$group)

#################################################################################
#################################################################################
### sync, sync
#################################################################################
#################################################################################

###########################
unique(data$LinkGroup)
unique(nodes$group)

###########################
##  Give a color for each group:
my_colorNoLink <- paste0('d3.scaleOrdinal() .domain([', 
                   # Now add in node labels
                    '"TR", "TMS", "Dec", "TCrop", "PCrop", "SCrop", "HCrop", "CropSup",  
                    "Boundary", "Mono", "Silvi",
                   "Herb", "Trad", "Agrisil", "Palm", "Shrub", "Bndry", "Crop_support", "Tree"',
                   ']) .range([', 
                   #add in node colors
                   #'"TR",     "TMS",     "Dec",    "TCrop","PCrop","SCrop","HCrop", "CropSup",     
                   '"darkgreen","#93CB56","#03BB85","#5E1914","#BE5504","gold","yellow","#996515",',
                   #"Boundary", "Mono", "Silvi"
                   '"purple", "grey",   "darkgreen",',
                   #"Herb",  "Trad",      "Agrisil","Palm",  "Shrub", "Bndry", "Crop_support","Tree"',
                   '"#FFD300","lightgrey","#B8BC86","orange","gold",  "#E54ED0", "#996515",     "#A45A52"',
                   '])')

###########################
## Make the network with color code links:
sankeyPlot<- sankeyNetwork(Links = data, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "Area_ha", 
              NodeID = "name", colourScale=my_colorNoLink,
              LinkGroup = "LinkGroup", 
              NodeGroup="group", fontSize = 14, nodeWidth = 20,fontFamily = "Arial")
sankeyPlot

###########################
# Make the Network (grey links)
sankeyNetwork(Links = data, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "Area_ha", 
              NodeID = "name", colourScale=my_colorNoLink,
              #LinkGroup = "LinkGroup", 
              NodeGroup="group", fontSize = 14, nodeWidth = 20,fontFamily = "Arial")

