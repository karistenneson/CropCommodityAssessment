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
library(networkD3)
library(tidyverse)

######################################################
################## Sankey data ##################
######################################################
setwd("C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\GIA")

country <- 'Cambodia'
country <- 'Indonesia'
country <- 'Laos'
country <- 'Myanmar'
country <- 'Philippines'
country <- 'Thailand'
country <- 'Vietnam'

#####################################################################
### Forest Conversion.
#####################################################################

# load conversion data.
forConv <- read.csv(paste0('Results/Fig2_', country, 'ForesttoOtherAreas.csv'))
forConv$X<-as.character(forConv$X)
head(forConv)

## remove non tree
forConv <- forConv[forConv$X != 'NotFo_Not target',]

###########################
## create source.
forConv$source <- substr(forConv$X,1,5)
unique(forConv$source)

## Calculate source totals.
SourceTotalsConv = aggregate(forConv$Area_ha, by=list(Category = forConv$source), FUN=sum)
colnames(SourceTotalsConv) <- c("name", "Source_Total")
SourceTotalsConv

SourceTotalsConv$Source_Total <- round(signif(SourceTotalsConv$Source_Total, digits = 5))
SourceTotalsConv$Source_Total <- format(SourceTotalsConv$Source_Total, big.mark=",", trim=TRUE)
SourceTotalsConv

# Merge the source total column with the database.
forConv <- merge(forConv, SourceTotalsConv, by.x ="source", by.y = "name") 
head(forConv)

## format names
unique(forConv$source)
forConv$sourceLabels <- recode(forConv$source, 
                      "TRain" = paste0('Tropical rainforest', ' (', forConv$Source_Total, ')'), 
                      "TDryF" = paste0('Tropical dry forest', ' (', forConv$Source_Total, ')'), 
                      "TMDec" = paste0('Tropical moist deciduous forest', ' (', forConv$Source_Total, ')'), 
                      "TMtnS" = paste0('Tropical mountain system', ' (', forConv$Source_Total, ')'), 
                      "TShrb" = paste0('Tropical shrubland', ' (', forConv$Source_Total, ')'),
                      "SHumF" = paste0('Subtropical humid forest', ' (', forConv$Source_Total, ')')
                      )
unique(forConv$sourceLabels)

###########################
## Set up target
forConv$target <- substr(forConv$X,7,24)
unique(forConv$target)

forConv$target[forConv$target=='Not target']<-paste0(forConv$source[forConv$target=='Not target'],'_stable')
unique(forConv$target)

###########################
## calculate target total areas
TargetTotalsConv = aggregate(forConv$Area_ha, by=list(Category = forConv$target), FUN=sum)
colnames(TargetTotalsConv) <- c("name", "Target_Total")
TargetTotalsConv
TargetTotalsConv$Target_Total <- round(signif(TargetTotalsConv$Target_Total, digits = 5))
TargetTotalsConv$Target_Total <- format(TargetTotalsConv$Target_Total, big.mark=",", trim=TRUE)
TargetTotalsConv

# create link group to use to color the sankey chart links
TargetTotalsConv <- mutate(TargetTotalsConv, LinkGroup = case_when(
  name == "TCC_to_Agriculture" ~ 'ToCrop', 
  name == "TCC_to_BuiltUp" ~ 'ToBU', 
  name == "TCC_to_Other" ~ 'ToOther', 
  name == "TCC_to_Silvopast" ~ 'ToSp',
  name == "TCC_to_Veg" ~ 'ToVeg',
  name == "SHumF_stable" ~ 'Forest', 
  name == "TMDec_stable" ~ 'Forest', 
  name == "TDryF_stable" ~ 'Forest', 
  name == "TMtnS_stable" ~ 'Forest',
  name == "TRain_stable" ~ 'Forest',
  name == "TShrb_stable" ~ 'Forest',
  TRUE ~ 'Houston, we have a problem'
)
)

TargetTotalsConv[TargetTotalsConv$LinkGroup =='Houston, we have a problem', ]
unique(TargetTotalsConv$LinkGroup)

# Merge the target total column with the database.
unique(TargetTotalsConv$name)
unique(forConv$target)

forConv <- merge(forConv, TargetTotalsConv, by.x ="target", by.y = "name") 
head(forConv)

###########################
unique(forConv$target)
forConv$targetLabels <- recode(forConv$target, 
                            "TRain_stable" = paste0('Forest, tropical rainforest', ' (', forConv$Target_Total, ')'), 
                            "TMDec_stable" = paste0('Forest, tropical moist deciduous forest', ' (', forConv$Target_Total, ')'), 
                            "TMtnS_stable" = paste0('Forest, tropical mountain system', ' (', forConv$Target_Total, ')'),
                            "TDryF_stable" = paste0('Forest, tropical dry forest', ' (', forConv$Target_Total, ')'),
                            "TShrb_stable" = paste0('Forest, tropical shrubland', ' (', forConv$Target_Total, ')'),
                            "SHumF_stable" = paste0('Forest, subtropical humid forest', ' (', forConv$Target_Total, ')'),
                            "TCC_to_Agriculture" = paste0("Croplands", ' (', forConv$Target_Total, ')'), 
                            "TCC_to_BuiltUp" = paste0("Built-up", ' (', forConv$Target_Total, ')'), 
                            "TCC_to_Other" = paste0("Other", ' (', forConv$Target_Total, ')'), 
                            "TCC_to_Silvopast" = paste0('Silvopastoral', ' (', forConv$Target_Total, ')'),
                            "TCC_to_Veg" = paste0("Shrub, grassland, or herbaceous", ' (', forConv$Target_Total, ')')
                            )
unique(forConv$targetLabels)


#####################################################################
### Crop structure.
#####################################################################
# add in crop structure
cropStruct <- read.csv(paste0('Results/Fig2_', country, 'CropStructure.csv'))
head(cropStruct)
cropStruct$X <- as.character(cropStruct$X)
cropStruct$X[cropStruct$X == 'All_Shrub_Crops']<-'All_shrub_crops' 
## remove non tree
cropStruct<-cropStruct[cropStruct$X != 'None',]

#######################
## create source
cropStruct$source<- "Agriculture"
cropStruct$sourceLabels <- unique(forConv$targetLabels[forConv$target == 'TCC_to_Agriculture'])
  
## Set up target
cropStruct$target <- cropStruct$X
cropStruct$targetLabels <- paste0(str_replace_all(cropStruct$X, "_", " "), " (", 
                                  format(round(signif(cropStruct$Area_ha, digits = 5)), big.mark=",", trim=TRUE), 
                                  ")")
unique(cropStruct$target)
unique(cropStruct$targetLabels)

cropStruct$Source_Total<-999
cropStruct$Target_Total<-cropStruct$Area_ha
cropStruct$LinkGroup <- 'ToCrop'
#######################
# merge data sets
cropStruct2<-cropStruct[,c('target', 'source', 'X','Area_ha','SE_Area_ha','Source_Total','sourceLabels',
              'Target_Total','LinkGroup','targetLabels')]
forConv<-rbind(forConv,cropStruct2)

head(forConv)
tail(forConv)
forConv$LinkGroup
unique(forConv$LinkGroup)

forConv$LinkGroup
unique(forConv$LinkGroup)

write.csv(forConv[,c('sourceLabels','targetLabels','Area_ha', 'SE_Area_ha', 'LinkGroup')],
          paste0('Results/Fig2_Final/Fig2_',country,'_Formatted_Table.csv'), row.names=F)

head(forConv)
#######################
## Look up how to customize colors:
## https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram/

## https://stackoverflow.com/questions/49294056/how-to-set-the-color-of-nodes-in-the-sankey-plot-using-the-networkd3-library?rq=1

################################################################################
## with link groups & no shrub
################################################################################
#####################################################################################
forConv<-read.csv(paste0('Results/Fig2_Final/Fig2_',country,'_Formatted_Table.csv'))
forConv$sourceLabels<-as.character(forConv$sourceLabels)
forConv$targetLabels<-as.character(forConv$targetLabels)
forConv$LinkGroup<-as.character(forConv$LinkGroup)

head(forConv)

nodes = data.frame(
  name=c(as.character(forConv$sourceLabels), as.character(forConv$targetLabels)) %>% unique())
forConv$IDsource=match(forConv$sourceLabels, nodes$name)-1 
forConv$IDtarget=match(forConv$targetLabels, nodes$name)-1

## Add a 'group' column to the nodes data frame:
sort(nodes$name)
nodes$group<-'update'
hits<-grep("All herbaceous crops", nodes$name); nodes$group[hits] <- 'HCrop'
hits<-grep("All shrub crops", nodes$name); nodes$group[hits] <- 'SCrop'
hits<-grep("All palm crops", nodes$name); nodes$group[hits] <- 'PCrop'
hits<-grep("All tree crops", nodes$name); nodes$group[hits] <- 'TCrop'
hits<-grep("uilt-up", nodes$name); nodes$group[hits] <- 'BU'
hits<-grep("Cropland", nodes$name); nodes$group[hits] <- 'Crop'
hits<-grep("ubtropical humid", nodes$name); nodes$group[hits] <- 'SHumF'
hits<-grep("dry", nodes$name); nodes$group[hits] <- 'TDryF'
hits<-grep("decid", nodes$name); nodes$group[hits] <- 'TMDec'
hits<-grep("mountain", nodes$name); nodes$group[hits] <- 'TMtnS'
hits<-grep("rainforest", nodes$name); nodes$group[hits] <- 'TRain'
hits<-grep("shrubland", nodes$name); nodes$group[hits] <- 'TShrb'
hits<-grep("Other", nodes$name); nodes$group[hits] <- 'Other'
hits<-grep("grassland", nodes$name); nodes$group[hits] <- 'Veg'

nodes$group<-as.factor(nodes$group)

# Give a color for each group:
my_color <- paste0('d3.scaleOrdinal() .domain([
"ToCrop", "ToBU", "ToOther", "ToSP", "ToVeg", "Forest", "sub", 
"TRain",      "TMtnS",     "TMDec",   "SHumF",   "TShrb",   "TDryF",   
"Crop", "BU", "Other", "SP", "Veg",
"HCrop","PCrop","SCrop","TCrop","CropSup"]) 

.range([',
# "ToCrop", "ToBU", "ToOther",  "ToSP",   "ToVeg",  "Forest", "sub", 
' "#FFD300","grey", "lightgrey","#B399D4","FCF4A3", "#B3E2CD","#FFD300",',

# "TRain",    "TMtnS",  "TMDec",  "SHumF",   "TShrb",   "TDryF",   
' "darkgreen","#F4A460","#FFFDD0","#E3FF00", "#B76E79", "#D28386",',

# "Crop", "BU",     "Other", "SP",     "Veg",
'"orange", "black", "grey",  "purple", "#FFFFE0",',

# "HCrop", "PCrop",  "SCrop", "TCrop",   "CropSup"]) 
'"yellow", "#BE5504","gold",  "#5E1914", "#996515"])')


# Make the Network. I call my colour scale with the colourScale argument
sankeyNetwork(Links = forConv, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "Area_ha", NodeID = "name", colourScale=my_color,
              LinkGroup = "LinkGroup", NodeGroup="group", fontSize = 14, nodeWidth = 20,fontFamily = "Arial")

################################################################################
#####################################################################################
################################################################################
#####################################################################################

