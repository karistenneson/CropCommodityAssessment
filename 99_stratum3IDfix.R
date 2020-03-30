#' ---
#' title: "IDing Stratum 3 Data"
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
library(tidyverse)
source("00_GIA_functions.R")

# Read in Data -----------------------------------------------------------------
#+ inputdata
stratum3Filt <- read_csv("Data/Stratum3/VietnamStratum3_points.csv")

compData <- read_csv("Data/Corrected/Vietnam.csv", 
                     col_types = "ddddldcdcdddcdccccc")

#Check number of unique plots
length(unique(compData$PL_PLOTID))
length(unique(compData$PLOT_ID))

# Add ORIG ID to PL_PLOTID
stratum3Filt <- mutate(stratum3Filt, PL_PLOTID = as.numeric(paste0(999, 
                formatC(PL_ORIGID, width = 4, format = "d", flag = "0"))))

#Check that lengths match
nrow(stratum3Filt) == sum(is.na(compData$PL_PLOTID))

#prep data to replace in other table
replaceRows <- which(compData$PLOT_ID %in% stratum3Filt$PLOT_ID)

#replace NAs with new IDs 
compData[replaceRows, "PL_PLOTID"] <- stratum3Filt$PL_PLOTID

#Check number of unique plots
length(unique(compData$PL_PLOTID))
length(unique(compData$PLOT_ID))
sum(is.na(compData$PL_PLOTID))

#Write out compiled points
write_csv(compData, "Data/Corrected/Vietnam.csv", na = "")

#Write out Stratum3 points
write_csv(stratum3Filt, "Data/Stratum3/VietnamStratum3_points.csv")
