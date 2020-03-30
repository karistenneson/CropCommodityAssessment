#' ---
#' title: "Incompatible Uses Script"
#' author: "MS Patterson, tertiarymatt@gmail.com; Karis Tenneson"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: [github_document, word_document]
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
source("00_GIA_functions.R")

#+ inputdata
# Data input and cleaning ------------------------------------------------------
## Code for Indonesia and Philippines
# dataPath <- c("data/Indonesia/")
# files <- dir(dataPath)
# 
# rawData <- files %>%
#   map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))
# 
# rawData <- clean_data(rawData, c(15:19))

# Code for Continental SEA
rawData <- read_csv("data/Compiled/Vietnam_points.csv", col_types = "ddddldc_cdddcdccccc")

# clean data
rawData <- clean_data(rawData, c(14:18))


# Filter to Incompatible uses --------------------------------------------------


dataTree1 <- filter(rawData, 
                    (LAND_COVER == 'Rubber' |
                       LAND_COVER == 'Oil_Palm' |
                       LAND_COVER == 'Pulpwood') & 
                      (LAND_USE == 'Silvopastoral' | 
                         LAND_USE == 'Natural_Forest' | 
                         LAND_USE == 'Other'))

dataTree2 <- filter(rawData, 
                    (LAND_COVER == 'Fruit_Nut' | 
                       LAND_COVER == 'Coconut' | 
                       LAND_COVER == 'Banana') & 
                      (LAND_USE == 'Natural_Forest'))

dataTeaCoff <- filter(rawData, 
                      (LAND_COVER == 'Tea' | LAND_COVER == 'Coffee') & 
                        (LAND_USE == 'Silvopastoral' | 
                           LAND_USE == 'Natural_Forest' | 
                           LAND_USE == 'Plantation'))

dataRice <- filter(rawData, 
                   (LAND_COVER == 'Rice') & 
                     (LAND_USE == 'Silvopastoral' | 
                        LAND_USE == 'Natural_Forest' | 
                        LAND_USE == 'Plantation'| 
                        LAND_USE == 'Other'))

dataOther <- filter(rawData, 
                    (LAND_COVER == 'Other_Crop') & 
                      (LAND_USE == 'Silvopastoral' | 
                         LAND_USE == 'Natural_Forest' | 
                         LAND_USE == 'Plantation'))

dataBU <- filter(rawData, 
                 (LAND_COVER == 'Built_up') & 
                   (LAND_USE == 'Terrace' | 
                      LAND_USE == 'Natural_Forest' | 
                      LAND_USE == 'Plantation'))

# Compile points with incompatible cover/use data
incompPoints <- rbind(dataTree1, dataTree2, dataTeaCoff, dataRice, dataOther, dataBU)
incompPoints

# Use points to find plots
incompPlotIDs <- pull(distinct(incompPoints, PLOT_ID))
incompPlots <- filter(rawData, PLOT_ID %in% incompPlotIDs)
incompPlots

# Write out files of incompatible covers/uses 
write.csv(incompPoints, file = 'data/Incompatible/Vietnam_points.csv', row.names = F)
write.csv(incompPlots, file = 'data/Incompatible/Vietnam_plots.csv', row.names = F)


# Rice plots in Agrisilviculture uses
dataRice2 <- filter(rawData, 
                    (LAND_COVER == 'Rice') & 
                      (LAND_USE == 'Agrisiviculture' | 
                         LAND_USE == 'Strip_Agrisilviculture' | 
                         LAND_USE == 'Boundary_Agrisilviculture'| 
                         LAND_USE == 'Mixed_Agrisilviculture'))

write.csv(dataRice2, file = 'data/Incompatible/Vietnam_rice_points.csv', row.names = F)
