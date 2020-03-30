# setup ------------------------------------------------------------------------
library(networkD3)
library(plotly)
packageVersion('plotly')


Colombia <- read.csv('sankey/colombiaComplete.csv')

Col<-Colombia[,c(1,3,4,12:24)]

# exploratory data analysis ----------------------------------------------------
head(Col[1:3,])
hist(Col$actively_saved_on_day)

hist(Col$number_of_trees*2)

table(Col$land_use)

plot(table(Col$agroforestry_types))

treeCode <- c(NA, 0, 2, 4, 6, 8, 10, 
             11-19, 20-29, 30-39, 40-49, 50-59, 
             60-69, 70-79, 80-89, 90-100)


# constructing sankey components -----------------------------------------------
# probably will write a function to do most of these construction steps.

weight<-table(Col$land_use, Col$agroforestry_types)

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
  0, 6, weight[2,2], # Each row represents a link. The first number
  0, 7, weight[2,3], # represents the node being conntected from. 
  0, 8, weight[2,4], # the second number represents the node connected to.
  0, 9, weight[2,5],  # The third number is the value of the node
  0, 10, weight[2,6],   
  0, 11, weight[2,7],  
  0, 12, weight[2,8],  
  0, 13, weight[2,9],  
  1, 6, weight[3,2], #
  1, 7, weight[3,3],  
  1, 8, weight[3,4], 
  1, 9, weight[3,5], 
  1, 10, weight[3,6],  
  1, 11, weight[3,7],
  1, 12, weight[3,8], 
  1, 13, weight[3,9], 
  2, 6, weight[4,2], # 
  2, 7, weight[4,3],  
  2, 8, weight[4,4], 
  2, 9, weight[4,5], 
  2, 10, weight[4,6],  
  2, 11, weight[4,7],
  2, 12, weight[4,8], 
  2, 13, weight[4,9], 
  3, 6, weight[5,2], #
  3, 7, weight[5,3],  
  3, 8, weight[5,4], 
  3, 9, weight[5,5], 
  3, 10, weight[5,6],  
  3, 11, weight[5,7],
  3, 12, weight[5,8], 
  3, 13, weight[5,9], 
  4, 6, weight[6,2], #
  4, 7, weight[6,3],  
  4, 8, weight[6,4], 
  4, 9, weight[6,5], 
  4, 10, weight[6,6],  
  4, 11, weight[6,7],
  4, 12, weight[6,8], 
  4, 13, weight[6,9], 
  5, 6, weight[7,2], #
  5, 7, weight[7,3],  
  5, 8, weight[7,4], 
  5, 9, weight[7,5], 
  5, 10, weight[7,6],  
  5, 11, weight[7,7],
  5, 12, weight[7,8],
  5, 13, weight[7,9]),
  byrow = TRUE, ncol = 3))

names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 16, nodeWidth = 30)