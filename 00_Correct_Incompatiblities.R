#' ---
#' title: "Recode Incompatible Uses Script"
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
# Code for Continental SEA
# rawData <- read_csv("data/Compiled/Myanmar_points.csv", col_types = "ddddldcdcdddcdccccc")

## Code for Indonesia and Philippines
dataPath <- c("data/Philippines/")
files <- dir(dataPath)

rawData <- files %>%
  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))

rawData <- clean_data(rawData, c(15:19))

# Recode Incompatible uses --------------------------------------------------

dataRecode <- mutate(rawData, LAND_USE = case_when(
  # Pulpwood, Rubber, and Oil Palm in these become plantation
  (LAND_COVER == 'Rubber' |
     LAND_COVER == 'Oil_Palm' |
     LAND_COVER == 'Pulpwood') & 
    (LAND_USE == 'Silvopastoral' | 
       LAND_USE == 'Natural_Forest' | 
       LAND_USE == 'Other') ~ 'Plantation',
  # Fruit_Nut, Coconut, Banana in natural forest become plantation
  (LAND_COVER == 'Fruit_Nut' | 
     LAND_COVER == 'Coconut' | 
     LAND_COVER == 'Banana') & 
    (LAND_USE == 'Natural_Forest') ~ "Plantation",
  # Tea/Coffee in Silvopastoral or natural forest become Agrisilviculture
  (LAND_COVER == 'Tea' | LAND_COVER == 'Coffee') & 
    (LAND_USE == 'Silvopastoral' | 
       LAND_USE == 'Natural_Forest') ~ "Agrisiviculture",
  # Tea/Coffee in Plantation become Other
  (LAND_COVER == 'Tea' | LAND_COVER == 'Coffee') & 
    LAND_USE == 'Plantation' ~ "Other_LAND_USE",
  # Rice plots != Terrace or Other become Other
  (LAND_COVER == 'Rice') & 
    (LAND_USE != 'Other_LAND_USE' | 
       LAND_USE != 'Terrace') ~ "Other_LAND_USE",
  # Other crop in Silvopastoral become Agrisiviculture
  (LAND_COVER == 'Other_Crop') & 
    (LAND_USE == 'Silvopastoral' | 
       LAND_USE == 'Natural_Forest') ~ "Agrisiviculture",
  # Other crop in Plantation becomes other
  LAND_COVER == 'Other_Crop' & LAND_USE == 'Plantation' ~ "Other_LAND_USE",
  # Built up become other
  LAND_COVER == 'Built_up' & LAND_USE != 'Other_LAND_USE' ~ "Other_LAND_USE",
  TRUE ~ LAND_USE
  )
)

# Write out file of points with 'corrected' covers/uses 
write.csv(dataRecode, file = 'data/Corrected/Philippines.csv', row.names = F)
