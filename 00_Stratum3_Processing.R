#' ---
#' title: "Incorporating Stratum 3 Data"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'
#'
#' Set working directory to where data is being stored.

# Setup ------------------------------------------------------------------------
#+ setwd
#setwd("~/R/GIA/")

#' ### Required packages
#+ Packages
library(rlang)
library(tidyverse)
library(knitr)
source("00_GIA_functions.R")

# Read in Data -----------------------------------------------------------------
#+ inputdata
dataPath <- c("data/Raw_Vietnam/")
files <- dir(dataPath)

rawData <- files %>% 
  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))


stratum3 <- read_csv("Data/Stratum3/VietnamStrata3.csv")

# Building stratum 3 data into useful tables -----------------------------------
#+build
colnames(stratum3)
stratum3 <- stratum3[,-5]
unique(stratum3$Dynamics)

sum(stratum3$Dynamics == "Tree_cover_loss" | 
      stratum3$Dynamics == "Tree_cover_loss_partial", na.rm = T)

# randomly sample stratum3 plots of inclusion in the data, 
# unless there's less than 250, in which case include them all.
plots <- if (nrow(stratum3) > 250) {
  sample_n(stratum3, 250)  
} else {stratum3}

sum(plots$Dynamics == "Tree_cover_loss" | 
      plots$Dynamics == "Tree_cover_loss_partial", na.rm = T)

# make names match with the rest of the data
colnames(plots) <- c("LON", "LAT", "PL_ORIGID", "Dynamics")

# Add other columns into the data, and provide default values
plots <- add_column(plots,  PLOT_ID = 1:nrow(plots), SAMPLE_ID = na_dbl, 
           FLAGGED = FALSE, ANALYSES = 0, USER_ID = 'potapove@user.com',
           COLLECTION_TIME = 0, ANALYSIS_DURATION = 0, IMAGERY_TITLE = na_chr, 
           PL_PLOTID = na_dbl, PL_PACKETID = na_dbl, PL_COUNTRY = 'Vietnam', 
           PL_STRATUM = 3, `LAND COVER` = "Other", 
           `UNDERSTORY PRESENT?` = 'No', `UNDERSTORY COVER` = na_chr, 
           `LAND USE` = 'Other', `LAND USE YEAR 2000` = na_chr)

plots <- mutate(plots, 
                PL_PLOTID = as.numeric(paste0(999, 
                formatC(PL_ORIGID, width = 4, format = "d", flag = "0"))))

# transform Potapov codes into our equivalent codes
plots <- mutate(plots, 
  `LAND USE YEAR 2000` = case_when(
    Dynamics == "Tree_cover_rotation" ~ "Forest Commodity",
    Dynamics == "Tree_cover_rotation_partial" ~ "Forest Commodity",
    Dynamics == "Tree_cover_gain" ~ "Other",
    Dynamics == "Tree_cover_gain_partial" ~ "Other",
    Dynamics == "Tree_cover_loss" ~ "Natural Forest",
    Dynamics == "Tree_cover_loss_partial" ~ "Natural Forest",
    TRUE ~ "Other"
  ), 
  `LAND USE` = case_when(
    Dynamics == "Tree_cover_rotation" ~ "Plantation",
    Dynamics == "Tree_cover_rotation_partial" ~ "Plantation",
    TRUE ~ "Other"
  ), 
  `LAND COVER` = case_when(
    Dynamics == "Tree_cover_rotation" ~ "Other Tree",
    Dynamics == "Tree_cover_rotation_partial" ~ "Other Tree",
    Dynamics == "Tree_cover_gain" ~ "Other Tree",
    Dynamics == "Tree_cover_gain_partial" ~ "Other Tree",
    TRUE ~ "Other"
  )
)

plots[,16:20]
# Convert plot data into point data

points <- map_dfc(plots, rep, 24) %>% 
  arrange(., PLOT_ID)
points$SAMPLE_ID <- 1:24

points[,16:20]

# removing plots already in the data that have been PIed------------------------
#+ cleanup

# find plots in stratum 3 that are already PIed
removeS3 <- points$PL_ORIGID %in% rawData$PL_ORIGID &
      points$`LAND USE YEAR 2000` == "Natural Forest"

# find stratum 3 plots in CEO data
dupes <- rawData$PL_ORIGID %in% points$PL_ORIGID &
  (rawData$PL_STRATUM == 3 | rawData$PL_STRATUM == 3)
  

# Remove points already in CEO data from 'fake' stratum 3 
points <- points[!removeS3,]
#points <- points[,-21]

# Remove points NOT in 'fake' stratum 3 from CEO
removeCEOS3 <- rawData$PL_STRATUM == 3 & dupes == FALSE
cleanData <- rawData[!removeCEOS3,]

#build clean data table
points <- select(points, colnames(cleanData))
fullData <- bind_rows(cleanData, points)

# Clean up the data
fullData <- clean_data(fullData, c(15:19))

# Check Sample Sizes
c(length(unique(fullData[which(fullData$PL_STRATUM == 1),]$PL_PLOTID)),
  length(unique(fullData[which(fullData$PL_STRATUM == 2),]$PL_PLOTID)),
  length(fullData[which(fullData$PL_STRATUM == 3),]$PL_PLOTID)/24)


#Write out compiled points
write_csv(fullData, "Data/Compiled/Vietnam_points.csv", na = "")

#Write out Stratum3 points
write_csv(points, "Data/Stratum3/VietnamStratum3_points.csv")