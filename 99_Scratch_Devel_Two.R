#' ---
#' title: "Scratch File for Random Needs"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'
#'
#' Set working directory to where data is being stored.

#+ setwd
#setwd("~/R/GIA/")

#' ### Required packages
#+ Packages
library(rlang)
library(tidyverse)
library(knitr)
source("00_GIA_functions.R")

#+ inputdata

jan <- read_csv("Data/Vietnam/ceo-vn70403-sample-data-2019-05-08.csv")
colnames(jan)

jan$`LAND USE`[jan$FLAGGED == FALSE & jan$`LAND COVER` == 'Rubber'& 
                 jan$`LAND USE` == 'Other'] <- "Plantation"
write_csv(jan, "Data/Vietnam/ceo-vn70403-sample-data-2019-05-08.csv")

#+ Building stratum 3 data into useful tables
stratum3 <- read_csv("Data/Stratum3/VietnamStrata3.csv")

colnames(stratum3)
stratum3 <- stratum3[,-5]
unique(stratum3$Dynamics)

sum(stratum3$Dynamics == "Tree_cover_loss" | stratum3$Dynamics == "Tree_cover_loss_partial")

plots <- sample_n(stratum3, 250)
sum(plots$Dynamics == "Tree_cover_loss" | plots$Dynamics == "Tree_cover_loss_partial")

colnames(plots) <- c("LON", "LAT", "PL_ORIGID", "Dynamics")
plots <- add_column(plots,  PLOT_ID = 1:250, SAMPLE_ID = na_dbl, FLAGGED = FALSE, 
           ANALYSES = 0, USER_ID = 'potapove@user.com',
           COLLECTION_TIME = 0, ANALYSIS_DURATION = 0, IMAGERY_TITLE = na_chr, 
           PL_PLOTID = na_dbl, PL_PACKETID = na_dbl, PL_COUNTRY = 'Vietnam', 
           PL_STRATUM = 3, `LAND COVER` = "Other", 
           `UNDERSTORY PRESENT?` = 'No', `UNDERSTORY COVER` = "", 
           `LAND USE` = 'Other', `LAND USE YEAR 2000` = na_chr)

plots <- mutate(plots, `LAND USE YEAR 2000` = case_when(
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

plots <- select(plots, colnames(jan), everything())
points <- map_dfc(plots, rep, 24)
points <- arrange(points, PLOT_ID)
points$SAMPLE_ID <- 1:24

#+ removing plots already in the data that have been PIed. 
dataPath <- c("data/Vietnam")
files <- dir(dataPath)

rawData <- files %>% 
  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))

# find plots in stratum 3 that are already PIed
removeS3 <- points$PL_ORIGID %in% rawData$PL_ORIGID &
      points$Dynamics == "Tree_cover_loss" | points$Dynamics == "Tree_cover_loss_partial"

# find stratum 3 plots in CEO data
dupes <-   rawData$PL_ORIGID %in% points$PL_ORIGID &
  (rawData$PL_STRATUM == 3 | rawData$PL_STRATUM == 3)
  

# Remove points already in CEO data from 'fake' stratum 3 
points <- points[!removeS3,]
points <- points[,-21]

# Remove points NOT in 'fake' stratum 3 from CEO
removeCEOS3 <- rawData$PL_STRATUM == 3 & dupes == FALSE
cleanData <- rawData[!removeCEOS3,]

#Write out Stratum3 points
write_csv(points, "Data/Stratum3/VietnamStratum3_points.csv")

#build clean data table
points <- select(points, colnames(cleanData))
fullData <- bind_rows(cleanData, points)

# Clean up the data
fullData <- clean_data(fullData, c(15:19))
write_csv(fullData, "Data/Compiled/Vietnam_points.csv", na = "")
