library(networkD3)
library(plotly)
packageVersion('plotly')

folder<-'C:\\Users\\krtenneson\\Desktop\\IP_IntermediateFiles\\GIAsia\\CSVdataMarcelo'
setwd(folder)
filesList <- list.files(pattern="*.csv")
length(filesList)

Colombia <- read.csv('colombiaComplete.csv')
Vietnam <- read.csv('vietnam.csv')

Col<-Colombia[,c(1,3,4,12:24)]
Viet<-Vietnam[,c(1,3,4,12:24)]

head(Col[1:3,])
hist(Col$actively_saved_on_day)
hist(Viet$actively_saved_on_day)

hist(Col$number_of_trees*2)
hist(Viet$number_of_trees*2)

table(Col$land_use)
table(Viet$land_use)

plot(table(Col$agroforestry_types))
plot(table(Viet$agroforestry_types))

treeCode <- c(NA, 0, 2, 4, 6, 8, 10, 
             11-19, 20-29, 30-39, 40-49, 50-59, 
             60-69, 70-79, 80-89, 90-100)

##################################
####################################################################
##################################
weights<-table(Col$land_use, Col$agroforestry_types)

##################################
province<-unique(Col$Department)
i = 1

weights<-table(Col$land_use[Col$Department==province[i]], 
               Col$agroforestry_types[Col$Department==province[i]])
weights
province[i]
i  = i + 1
####################################################################
##################################

nodes = data.frame("name" = 
                     c("Cropland", # Node 0
                       "Forest", # Node 1
                       "Grassland", # Node 2
                       "Other", # Node 3
                       "Settlement", # Node 4
                       "Wetlands", # Node 5
                       
                       "AgriSilvicultural", # Node 6
                       "Boundary Planting", # Node 7
                       "Homegardens", # Node 8
                       "Mangrove Systems", # Node 9
                       "None", # Node 10
                       "Shadow Systems", # Node 11
                       "Silvopastoral", # Node 12
                       "Woodlots")) # Node 13

links = as.data.frame(matrix(c(
  0, 6, weights[2,2], # Each row represents a link. The first number
  0, 7, weights[2,3], # represents the node being conntected from. 
  0, 8, weights[2,4], # the second number represents the node connected to.
  0, 9, weights[2,5],  # The third number is the value of the node
  0, 10, weights[2,6],   
  0, 11, weights[2,7],  
  0, 12, weights[2,8],  
  0, 13, weights[2,9],  
  1, 6, weights[3,2], #
  1, 7, weights[3,3],  
  1, 8, weights[3,4], 
  1, 9, weights[3,5], 
  1, 10, weights[3,6],  
  1, 11, weights[3,7],
  1, 12, weights[3,8], 
  1, 13, weights[3,9], 
  2, 6, weights[4,2], # 
  2, 7, weights[4,3],  
  2, 8, weights[4,4], 
  2, 9, weights[4,5], 
  2, 10, weights[4,6],  
  2, 11, weights[4,7],
  2, 12, weights[4,8], 
  2, 13, weights[4,9], 
  3, 6, weights[5,2], #
  3, 7, weights[5,3],  
  3, 8, weights[5,4], 
  3, 9, weights[5,5], 
  3, 10, weights[5,6],  
  3, 11, weights[5,7],
  3, 12, weights[5,8], 
  3, 13, weights[5,9], 
  4, 6, weights[6,2], #
  4, 7, weights[6,3],  
  4, 8, weights[6,4], 
  4, 9, weights[6,5], 
  4, 10, weights[6,6],  
  4, 11, weights[6,7],
  4, 12, weights[6,8], 
  4, 13, weights[6,9], 
  5, 6, weights[7,2], #
  5, 7, weights[7,3],  
  5, 8, weights[7,4], 
  5, 9, weights[7,5], 
  5, 10, weights[7,6],  
  5, 11, weights[7,7],
  5, 12, weights[7,8],
  5, 13, weights[7,9]),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 16, nodeWidth = 30)

##################################
### ANTIOQUIA #################################################################
####################################################################
##################################
Ant<-Col[Col$Department=='Antioquia', ]
Ant$land_use<-as.factor(as.character(Ant$land_use)) 
Ant$agroforestry_types<-as.factor(as.character(Ant$agroforestry_types))
weights<-table(Ant$land_use, Ant$agroforestry_types)
weights
####################################################################
##################################

nodes = data.frame("name" = 
                     c("Cropland", # Node 0
                       "Forest", # Node 1
                       "Grassland", # Node 2
                       "Other", # Node 3
                       "Settlement", # Node 4
                       "Wetlands", # Node 5
                       
                       "AgriSilvicultural", # Node 6
                       "Boundary Planting", # Node 7
                       "Homegardens", # Node 8
                       "None", # Node 9
                       "Silvopastoral", # Node 10
                       "Woodlots")) # Node 11

links = as.data.frame(matrix(c(
  0, 6, weights[2,2], # Each row represents a link. The first number
  0, 7, weights[2,3], # represents the node being conntected from. 
  0, 8, weights[2,4], # the second number represents the node connected to.
  0, 9, weights[2,5],  # The third number is the value of the node
  0, 10, weights[2,6],   
  0, 11, weights[2,7],  
  1, 6, weights[3,2], #
  1, 7, weights[3,3],  
  1, 8, weights[3,4], 
  1, 9, weights[3,5], 
  1, 10, weights[3,6],  
  1, 11, weights[3,7], 
  2, 6, weights[4,2], # 
  2, 7, weights[4,3],  
  2, 8, weights[4,4], 
  2, 9, weights[4,5], 
  2, 10, weights[4,6],  
  2, 11, weights[4,7], 
  3, 6, weights[5,2], #
  3, 7, weights[5,3],  
  3, 8, weights[5,4], 
  3, 9, weights[5,5], 
  3, 10, weights[5,6],  
  3, 11, weights[5,7], 
  4, 6, weights[6,2], #
  4, 7, weights[6,3],  
  4, 8, weights[6,4], 
  4, 9, weights[6,5], 
  4, 10, weights[6,6],  
  4, 11, weights[6,7], 
  5, 6, weights[7,2], #
  5, 7, weights[7,3],  
  5, 8, weights[7,4], 
  5, 9, weights[7,5], 
  5, 10, weights[7,6],  
  5, 11, weights[7,7]),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 16, fontFamily = 'sans', nodeWidth = 30)


##################################
### CALDAS #################################################################
####################################################################
##################################
Cal<-Col[Col$Department=='Caldas', ]
Cal$land_use<-as.factor(as.character(Cal$land_use)) 
Cal$agroforestry_types<-as.factor(as.character(Cal$agroforestry_types))
weights<-table(Cal$land_use, Cal$agroforestry_types)
weights
####################################################################
##################################

nodes = data.frame("name" = 
                     c("Cropland", # Node 0
                       "Forest", # Node 1
                       "Grassland", # Node 2
                       "Other", # Node 3
                       "Settlement", # Node 4
                       "Wetlands", # Node 5
                       
                       "AgriSilvicultural", # Node 6
                       "Boundary Planting", # Node 7
                       "Homegardens", # Node 8
                       "None", # Node 9
                       "Shadow Systems", # Node 10
                       "Silvopastoral", # Node 11
                       "Woodlots")) # Node 12

links = as.data.frame(matrix(c(
  0, 6, weights[2,2], # Each row represents a link. The first number
  0, 7, weights[2,3], # represents the node being conntected from. 
  0, 8, weights[2,4], # the second number represents the node connected to.
  0, 9, weights[2,5],  # The third number is the value of the node
  0, 10, weights[2,6],   
  0, 11, weights[2,7],  
  0, 12, weights[2,8],  
  1, 6, weights[3,2], #
  1, 7, weights[3,3],  
  1, 8, weights[3,4], 
  1, 9, weights[3,5], 
  1, 10, weights[3,6],  
  1, 11, weights[3,7],
  1, 12, weights[3,8], 
  2, 6, weights[4,2], # 
  2, 7, weights[4,3],  
  2, 8, weights[4,4], 
  2, 9, weights[4,5], 
  2, 10, weights[4,6],  
  2, 11, weights[4,7],
  2, 12, weights[4,8], 
  3, 6, weights[5,2], #
  3, 7, weights[5,3],  
  3, 8, weights[5,4], 
  3, 9, weights[5,5], 
  3, 10, weights[5,6],  
  3, 11, weights[5,7],
  3, 12, weights[5,8], 
  4, 6, weights[6,2], #
  4, 7, weights[6,3],  
  4, 8, weights[6,4], 
  4, 9, weights[6,5], 
  4, 10, weights[6,6],  
  4, 11, weights[6,7],
  4, 12, weights[6,8], 
  5, 6, weights[7,2], #
  5, 7, weights[7,3],  
  5, 8, weights[7,4], 
  5, 9, weights[7,5], 
  5, 10, weights[7,6],  
  5, 11, weights[7,7],
  5, 12, weights[7,8]),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 16, fontFamily = 'sans', nodeWidth = 30)


##################################
### CASANARE #################################################################
####################################################################
##################################
Casa<-Col[Col$Department=='Casanare', ]
Casa$land_use<-as.factor(as.character(Casa$land_use)) 
Casa$agroforestry_types<-as.factor(as.character(Casa$agroforestry_types))
weights<-table(Casa$land_use, Casa$agroforestry_types)
weights

####################################################################
##################################

nodes = data.frame("name" = 
                     c("Cropland", # Node 0
                       "Forest", # Node 1
                       "Grassland", # Node 2
                       "Other", # Node 3
                       "Settlement", # Node 4
                       "Wetlands", # Node 5
                       
                       "AgriSilvicultural", # Node 6
                       "None", # Node 7
                       "Shadow Systems", # Node 8
                       "Silvopastoral")) # Node 9

links = as.data.frame(matrix(c(
  0, 6, weights[2,2], # Each row represents a link. The first number
  0, 7, weights[2,3], # represents the node being conntected from. 
  0, 8, weights[2,4], # the second number represents the node connected to.
  0, 9, weights[2,5],  # The third number is the value of the node
  1, 6, weights[3,2], #
  1, 7, weights[3,3],  
  1, 8, weights[3,4], 
  1, 9, weights[3,5], 
  2, 6, weights[4,2], # 
  2, 7, weights[4,3],  
  2, 8, weights[4,4], 
  2, 9, weights[4,5], 
  3, 6, weights[5,2], #
  3, 7, weights[5,3],  
  3, 8, weights[5,4], 
  3, 9, weights[5,5], 
  4, 6, weights[6,2], #
  4, 7, weights[6,3],  
  4, 8, weights[6,4], 
  4, 9, weights[6,5], 
  5, 6, weights[7,2], #
  5, 7, weights[7,3],  
  5, 8, weights[7,4], 
  5, 9, weights[7,5]),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 16, nodeWidth = 30)

##################################
### HUILA #################################################################
####################################################################
##################################
Huila<-Col[Col$Department=='Huila', ]
Huila$land_use<-as.factor(as.character(Huila$land_use)) 
Huila$agroforestry_types<-as.factor(as.character(Huila$agroforestry_types))
weights<-table(Huila$land_use, Huila$agroforestry_types)
weights
####################################################################
##################################

nodes = data.frame("name" = 
                     c("Cropland", # Node 0
                       "Forest", # Node 1
                       "Grassland", # Node 2
                       "Other", # Node 3
                       "Settlement", # Node 4
                       "Wetlands", # Node 5
                       
                       "AgriSilvicultural", # Node 6
                       "Boundary Planting", # Node 7
                       "Homegardens", # Node 8
                       "Mangrove Systems", # Node 9
                       "None", # Node 10
                       "Shadow Systems", # Node 11
                       "Silvopastoral", # Node 12
                       "Woodlots")) # Node 13

links = as.data.frame(matrix(c(
  0, 6, weights[2,2], # Each row represents a link. The first number
  0, 7, weights[2,3], # represents the node being conntected from. 
  0, 8, weights[2,4], # the second number represents the node connected to.
  0, 9, weights[2,5],  # The third number is the value of the node
  0, 10, weights[2,6],   
  0, 11, weights[2,7],  
  0, 12, weights[2,8],  
  0, 13, weights[2,9],  
  1, 6, weights[3,2], #
  1, 7, weights[3,3],  
  1, 8, weights[3,4], 
  1, 9, weights[3,5], 
  1, 10, weights[3,6],  
  1, 11, weights[3,7],
  1, 12, weights[3,8], 
  1, 13, weights[3,9], 
  2, 6, weights[4,2], # 
  2, 7, weights[4,3],  
  2, 8, weights[4,4], 
  2, 9, weights[4,5], 
  2, 10, weights[4,6],  
  2, 11, weights[4,7],
  2, 12, weights[4,8],
  2, 13, weights[4,9], 
  3, 6, weights[5,2], #
  3, 7, weights[5,3],  
  3, 8, weights[5,4], 
  3, 9, weights[5,5], 
  3, 10, weights[5,6],  
  3, 11, weights[5,7],
  3, 12, weights[5,8], 
  3, 13, weights[5,9], 
  4, 6, weights[6,2], #
  4, 7, weights[6,3],  
  4, 8, weights[6,4], 
  4, 9, weights[6,5], 
  4, 10, weights[6,6],  
  4, 11, weights[6,7],
  4, 12, weights[6,8],
  4, 13, weights[6,9], 
  5, 6, weights[7,2], #
  5, 7, weights[7,3],  
  5, 8, weights[7,4], 
  5, 9, weights[7,5], 
  5, 10, weights[7,6],  
  5, 11, weights[7,7],
  5, 12, weights[7,8],
  5, 13, weights[7,9]),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 16, fontFamily = 'sans', nodeWidth = 30)


##################################
### META #################################################################
####################################################################
##################################
Meta<-Col[Col$Department=='Meta', ]
Meta$land_use<-as.factor(as.character(Meta$land_use)) 
Meta$agroforestry_types<-as.factor(as.character(Meta$agroforestry_types))
weights<-table(Meta$land_use, Meta$agroforestry_types)
weights
####################################################################
##################################

nodes = data.frame("name" = 
                     c("Cropland", # Node 0
                       "Forest", # Node 1
                       "Grassland", # Node 2
                       "Other", # Node 3
                       "Wetlands", # Node 4
                       
                       "AgriSilvicultural", # Node 5
                       "Boundary Planting", # Node 6
                       "None", # Node 7
                       "Shadow Systems", # Node 8
                       "Silvopastoral", # Node 9
                       "Woodlots")) # Node 10

links = as.data.frame(matrix(c(
  0, 5, weights[1,1], # Each row represents a link. The first number
  0, 6, weights[1,2], # represents the node being conntected from. 
  0, 7, weights[1,3], # the second number represents the node connected to.
  0, 8, weights[1,4],  # The third number is the value of the node
  0, 9, weights[1,5],   
  0, 10, weights[1,6],  
  1, 5, weights[2,1], #
  1, 6, weights[2,2],  
  1, 7, weights[2,3], 
  1, 8, weights[2,4], 
  1, 9, weights[2,5],  
  1, 10, weights[2,6],
  2, 5, weights[3,1], # 
  2, 6, weights[3,2],  
  2, 7, weights[3,3], 
  2, 8, weights[3,4], 
  2, 9, weights[3,5],  
  2, 10, weights[3,6],
  3, 5, weights[4,1], #
  3, 6, weights[4,2],  
  3, 7, weights[4,3], 
  3, 8, weights[4,4], 
  3, 9, weights[4,5],  
  3, 10, weights[4,6], 
  4, 5, weights[5,1], #
  4, 6, weights[5,2],  
  4, 7, weights[5,3], 
  4, 8, weights[5,4], 
  4, 9, weights[5,5],  
  4, 10, weights[5,6]),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 16, nodeWidth = 30)

##################################
### TOLIMA #################################################################
####################################################################
##################################
Tolima<-Col[Col$Department=='Tolima', ]
Tolima$land_use<-as.factor(as.character(Tolima$land_use)) 
Tolima$agroforestry_types<-as.factor(as.character(Tolima$agroforestry_types))
weights<-table(Tolima$land_use, Tolima$agroforestry_types)
weights
####################################################################
##################################

nodes = data.frame("name" = 
                     c("Cropland", # Node 0
                       "Forest", # Node 1
                       "Grassland", # Node 2
                       "Other", # Node 3
                       "Settlement", # Node 4
                       "Wetlands", # Node 5
                       
                       "AgriSilvicultural", # Node 6
                       "Boundary Planting", # Node 7
                       "Homegardens", # Node 8
                       "None", # Node 9
                       "Shadow Systems", # Node 10
                       "Silvopastoral", # Node 11
                       "Woodlots")) # Node 12

links = as.data.frame(matrix(c(
  0, 6, weights[2,2], # Each row represents a link. The first number
  0, 7, weights[2,3], # represents the node being conntected from. 
  0, 8, weights[2,4], # the second number represents the node connected to.
  0, 9, weights[2,5],  # The third number is the value of the node
  0, 10, weights[2,6],   
  0, 11, weights[2,7],  
  0, 12, weights[2,8],
  1, 6, weights[3,2], #
  1, 7, weights[3,3],  
  1, 8, weights[3,4], 
  1, 9, weights[3,5], 
  1, 10, weights[3,6],  
  1, 11, weights[3,7],
  1, 12, weights[3,8], 
  2, 6, weights[4,2], # 
  2, 7, weights[4,3],  
  2, 8, weights[4,4], 
  2, 9, weights[4,5], 
  2, 10, weights[4,6],  
  2, 11, weights[4,7],
  2, 12, weights[4,8], 
  3, 6, weights[5,2], #
  3, 7, weights[5,3],  
  3, 8, weights[5,4], 
  3, 9, weights[5,5], 
  3, 10, weights[5,6],  
  3, 11, weights[5,7],
  3, 12, weights[5,8], 
  4, 6, weights[6,2], #
  4, 7, weights[6,3],  
  4, 8, weights[6,4], 
  4, 9, weights[6,5], 
  4, 10, weights[6,6],  
  4, 11, weights[6,7],
  4, 12, weights[6,8], 
  5, 6, weights[7,2], #
  5, 7, weights[7,3],  
  5, 8, weights[7,4], 
  5, 9, weights[7,5], 
  5, 10, weights[7,6],  
  5, 11, weights[7,7],
  5, 12, weights[7,8]),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 16, nodeWidth = 30)

##################################
### VALLE #################################################################
####################################################################
##################################
Valle<-Col[Col$Department=='Valle', ]
Valle$land_use<-as.factor(as.character(Valle$land_use)) 
Valle$agroforestry_types<-as.factor(as.character(Valle$agroforestry_types))
weights<-table(Valle$land_use, Valle$agroforestry_types)
weights
####################################################################
##################################

nodes = data.frame("name" = 
                     c("Cropland", # Node 0
                       "Forest", # Node 1
                       "Grassland", # Node 2
                       "Other", # Node 3
                       "Settlement", # Node 4
                       "Wetlands", # Node 5
                       
                       "AgriSilvicultural", # Node 6
                       "Boundary Planting", # Node 7
                       "Homegardens", # Node 8
                       "Mangrove Systems", # Node 9
                       "None", # Node 10
                       "Shadow Systems", # Node 11
                       "Silvopastoral", # Node 12
                       "Woodlots")) # Node 13

links = as.data.frame(matrix(c(
  0, 6, weights[2,2], # Each row represents a link. The first number
  0, 7, weights[2,3], # represents the node being conntected from. 
  0, 8, weights[2,4], # the second number represents the node connected to.
  0, 9, weights[2,5],  # The third number is the value of the node
  0, 10, weights[2,6],   
  0, 11, weights[2,7],  
  0, 12, weights[2,8],
  0, 13, weights[2,9],
  1, 6, weights[3,2], #
  1, 7, weights[3,3],  
  1, 8, weights[3,4], 
  1, 9, weights[3,5], 
  1, 10, weights[3,6],  
  1, 11, weights[3,7],
  1, 12, weights[3,8],
  1, 13, weights[3,9], 
  2, 6, weights[4,2], # 
  2, 7, weights[4,3],  
  2, 8, weights[4,4], 
  2, 9, weights[4,5], 
  2, 10, weights[4,6],  
  2, 11, weights[4,7],
  2, 12, weights[4,8], 
  2, 13, weights[4,9], 
  3, 6, weights[5,2], #
  3, 7, weights[5,3],  
  3, 8, weights[5,4], 
  3, 9, weights[5,5], 
  3, 10, weights[5,6],  
  3, 11, weights[5,7],
  3, 12, weights[5,8],
  3, 13, weights[5,9], 
  4, 6, weights[6,2], #
  4, 7, weights[6,3],  
  4, 8, weights[6,4], 
  4, 9, weights[6,5], 
  4, 10, weights[6,6],  
  4, 11, weights[6,7],
  4, 12, weights[6,8], 
  4, 13, weights[6,9], 
  5, 6, weights[7,2], #
  5, 7, weights[7,3],  
  5, 8, weights[7,4], 
  5, 9, weights[7,5], 
  5, 10, weights[7,6],  
  5, 11, weights[7,7],
  5, 12, weights[7,8],
  5, 13, weights[7,9]),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 16, nodeWidth = 30)

##################################
#### VICHADA ################################################################
####################################################################
##################################
Vichada<-Col[Col$Department=='Vichada', ]
Vichada$land_use<-as.factor(as.character(Vichada$land_use)) 
Vichada$agroforestry_types<-as.factor(as.character(Vichada$agroforestry_types))
weights<-table(Vichada$land_use, Vichada$agroforestry_types)
weights
####################################################################
##################################

nodes = data.frame("name" = 
                     c("Cropland", # Node 0
                       "Forest", # Node 1
                       "Grassland", # Node 2
                       "Other", # Node 3
                       "Wetlands", # Node 4
                       
                       "AgriSilvicultural", # Node 5
                       "Boundary Planting", # Node 6
                       "Homegardens", # Node 7
                       "Mangrove Systems", # Node 8
                       "None", # Node 9
                       "Silvopastoral", # Node 10
                       "Woodlots")) # Node 11

links = as.data.frame(matrix(c(
  0, 5, weights[2,2], # Each row represents a link. The first number
  0, 6, weights[2,3], # represents the node being conntected from. 
  0, 7, weights[2,4], # the second number represents the node connected to.
  0, 8, weights[2,5],  # The third number is the value of the node
  0, 9, weights[2,6],   
  0, 10, weights[2,7],  
  0, 11, weights[2,8],  
  1, 5, weights[3,2], #
  1, 6, weights[3,3],  
  1, 7, weights[3,4], 
  1, 8, weights[3,5], 
  1, 9, weights[3,6],  
  1, 10, weights[3,7],
  1, 11, weights[3,8], 
  2, 5, weights[4,2], # 
  2, 6, weights[4,3],  
  2, 7, weights[4,4], 
  2, 8, weights[4,5], 
  2, 9, weights[4,6],  
  2, 10, weights[4,7],
  2, 11, weights[4,8], 
  3, 5, weights[5,2], #
  3, 6, weights[5,3],  
  3, 7, weights[5,4], 
  3, 8, weights[5,5], 
  3, 9, weights[5,6],  
  3, 10, weights[5,7],
  3, 11, weights[5,8], 
  4, 5, weights[6,2], #
  4, 6, weights[6,3],  
  4, 7, weights[6,4], 
  4, 8, weights[6,5], 
  4, 9, weights[6,6],  
  4, 10, weights[6,7],
  4, 11, weights[6,8]),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 16, nodeWidth = 30)
