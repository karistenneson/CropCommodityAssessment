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
################## Indonesia Sankey ##################
######################################################

peat <- read.csv('Results/Fig2_IndopeattoOtherAreas.csv')
peat$X<-as.character(peat$X)
head(peat)

###########################
## create source
peat$source <- substr(peat$X,1,7)
unique(peat$source)
## remove non tree
peat<-peat[peat$source != 'Other_N',]

## format names
unique(peat$source)
peat$sourceLabels <- recode(peat$source, 
                      "Rainfor" = 'Tropical rainforest (132,236)', 
                      "RainfPt" = 'Tropical rainforest, peatlands (9,551)', 
                      "TrDecid" = 'Tropical moist deciduous forest (3,355)', 
                      "TrMount" = 'Tropical mountain system (15,975)', 
                      "TrMouPt" = 'Tropical mountain system, peatlands (421)', 
                      "TrShrub" = 'Tropical shrubland (NA)')
peat<-peat[peat$sourceLabels != 'Tropical shrubland (NA)',]
peat$sourceLabels

###########################
## Set up target
peat$target <- substr(peat$X,13,24)
unique(peat$target)
peat$targetLabels <- recode(peat$target, 
                            "ical rainfor" = 'Forest, tropical rainforest (14,661)', 
                            "ical moist d" = 'Forest, tropical moist deciduous forest (1,233)', 
                            "ical mountai" = 'Forest, tropical mountain system (95,074)',
                            "to_Veg" = "Shrub, grassland, or herbaceous (10,392)", 
                            "to_Agricultu" = "Croplands (34,240)", 
                            "to_BuiltUp" = "Built-up (1,658)", 
                            "to_Other" = "Other (3,556)", 
                            "to_Silvopast" = 'Silvopastoral (722)'
                            )

#####################################################################
### Crop structure.
#####################################################################

#######################
# add in crop structure data.
cropType <- read.csv('Results/Fig2_IndoCropStructure.csv')
head(cropType)
cropType$X <- as.character(cropType$X)

## remove non tree
cropType<-cropType[cropType$X != 'None',]

## create source
cropType$sourceLabels <- cropType$source<- "Croplands (34,240)"

## Set up target
cropType$targetLabels <- cropType$target <- str_replace_all(cropType$X, "_", " ")
cropType$targetLabels[cropType$targetLabels == 'All herbaceous crops'] <- 'All herbaceous crops (11,482)'
cropType$targetLabels[cropType$targetLabels == 'All palm crops'] <- 'All palm crops (11,290)'
cropType$targetLabels[cropType$targetLabels == 'All Shrub crops'] <- 'All shrub crops (898)'
cropType$targetLabels[cropType$targetLabels == 'All tree crops'] <- 'All tree crops (10,049)'
cropType$targetLabels[cropType$targetLabels == 'Crop support'] <- 'Crop support (521)'

unique(cropType$target)

#######################
# merge data sets
peat<-rbind(peat,cropType)
head(peat)
tail(peat)

write.csv(peat[,c('sourceLabels','targetLabels','Area_ha', 'SE_Area_ha')],'Results/Fig2_Final/Fig2_Indonesia_Formatted_Table.csv', row.names=F)

#######################
## Look up how to customize colors:
## https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram/

## https://stackoverflow.com/questions/49294056/how-to-set-the-color-of-nodes-in-the-sankey-plot-using-the-networkd3-library?rq=1

################################################################################
## with link groups & no shrub
################################################################################
#####################################################################################
peat<-read.csv('Results/Fig2_Final/Fig2_Indonesia_Formatted_TableLinkGrpExcFinal.csv')
peat$sourceLabels<-as.character(peat$sourceLabels)
peat$targetLabels<-as.character(peat$targetLabels)
peat$LinkGroup<-as.character(peat$LinkGroup)

head(peat)

nodes = data.frame(
  name=c(as.character(peat$sourceLabels), as.character(peat$targetLabels)) %>% unique())
peat$IDsource=match(peat$sourceLabels, nodes$name)-1 
peat$IDtarget=match(peat$targetLabels, nodes$name)-1

# Add a 'group' column to the nodes data frame:
nodes$group=as.factor(c("Crop","TR","TR","TMS","Dec","TMS","Hcrop", 
                        "PCrop", "SCrop", "TCrop", "BU", "CropSup", 
                        "Dec", "TMS","TR","Other","Veg","SP"))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["ToCrop", "peat", "ToBU", "ToOther", "ToSP", "ToVeg", "Forest", "sub", 
"TR", "Dec", "TMS", "Crop", "BU", "Other", "SP", "Veg",
"HCrop","PCrop","SCrop","TCrop","CropSup"]) 
.range(["#FFD300","E2A76F", "grey","lightgrey","#B399D4","FCF4A3","#B3E2CD","#FFD300",
"darkgreen", "#03BB85", "#93CB56", "orange", "black", "grey", "purple", "#FFFFE0",
"yellow","#BE5504","gold","#5E1914","#996515"])'

my_colorTest <- ["ToCrop",  "peat",   "ToBU","ToOther",   "ToSP",  "ToVeg", "Forest", "sub", 
                 #"#FFD300","E2A76F", "grey","lightgrey","#B399D4","FCF4A3","#B3E2CD","#FFD300",
                  
                 "TR",        "Dec",      "TMS",      "Crop",  "BU",    "Other", "SP",    "Veg",
                 #"darkgreen", "#03BB85", "#93CB56", "orange", "black", "grey", "purple", "#FFFFE0",

                 "HCrop",   "PCrop",  "SCrop","TCrop","CropSup"]) 
                 # "yellow","orange","gold",  "red",  "#996515"])
                  ]

# Make the Network. I call my colour scale with the colourScale argument
sankeyNetwork(Links = peat, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "Area_ha", 
              NodeID = "name", colourScale=my_color,
              LinkGroup = "LinkGroup", NodeGroup="group", fontSize = 18, nodeWidth = 20,fontFamily = "Arial")

################################################################################
#####################################################################################
################################################################################
#####################################################################################

# From these flows we need to create a node data frame: 
# it lists every entities involved in the flow
nodes = data.frame(
  name=c(as.character(peat$sourceLabels), as.character(peat$targetLabels)) %>% unique())
peat$IDsource=match(peat$sourceLabels, nodes$name)-1 
peat$IDtarget=match(peat$targetLabels, nodes$name)-1

# Add a 'group' column to the nodes data frame:
nodes$group=as.factor(c("TR","TR","Dec","TMS","TMS","TS","Crop","BU", "Other", "SP", "Veg", "TR", "Dec", "TMS", "TS", 
                        "HCrop","PCrop","SCrop","TCrop","CropSup"))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["TR", "Dec", "TMS", "TS", "Crop", "BU", "Other", "SP", "Veg",
"HCrop","PCrop","SCrop","TCrop","CropSup"]) 
.range(["#93CB56", "#FFFFE0", "darkgreen", "#d2b48c", "orange", "red", "grey", "purple", "#E6F5C9",
"ivory","gold","yellow","orange","#996515"])'


# Make the Network. I call my colour scale with the colourScale argument
sankeyNetwork(Links = peat, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "Area_ha", NodeID = "name", colourScale=my_color,
              NodeGroup="group", fontSize = 15, nodeWidth = 20,fontFamily = "Arial")

