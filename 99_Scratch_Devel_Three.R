#' ---
#' title: "Scratch File for Development"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'
#'
#' Set working directory to where data is being stored.
#+ setwd
#setwd("~/R/GIA/")

#' ## TO DO  
#' 0. [X] Assemble list of estimator equations to code.  
#' 1. [X] Determine additional data inputs needed.  
#' 2. [X] Write presence function.  
#' 3. [X] Write functions for Question/Analysis Tables.  
#' 4. [X] Write estimator functions.
#' 5. [X] Write estimator functions for "object in class".  
#' 6. [X] Construct wrappers for main workflow.
#' 7. [X] Construct wrapper for "object in class" workflow.
#' 8. [x] Figure out y_occ workflow.
#' 8. [X] Area and error functions.   
#' 9. [X] Create country analysis script template.
#' 10. [x] Create pretty output formatting.  
#' 11. [X] Calculate carbon numbers.
#' 12. [X] Document functions more thoroughly.
#' 13. [X] Work out adding Understory analysis.  
#' 13. [ ] Update Vietnam with new table formats.  
#' 14. [ ] Convert country analysis script to proper Rmd.  
#'    


#' ## Getting Set Up  
#+ Packages
library(tidyverse)
library(knitr)
source("00_GIA_functions.R")

#+ user_supplied_variables
# User needs to supply variables -----------------------------------------------

# Area of strata and sample sizes
strata <- c(1, 2, 3)
stratumAreas <- c("Strata1 Area" = 8324441, "Strata2 Area" = 	12512668, 
                  "Strata3 Area" = 169619791)

# Metadata to save, and Grouping Variables
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")
groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

# Survey Questions
questions <-  c("LAND_COVER", "UNDERSTORY_PRESENT", "UNDERSTORY_COVER",
                "LAND_USE", "LAND_USE_YEAR_2000")


#+ inputdata
# Data input and cleaning ------------------------------------------------------
dataPath <- c("data/Indonesia")
files <- dir(dataPath)

rawData <- files %>% 
  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))

# clean data
rawData <- clean_data(rawData, c(15:19))

# remove unneeded columns 
keep <- c(metaNames, "FLAGGED", questions)
rawData <- rawData[,keep]

#Determine actual sample size
sampSize <- c(length(unique(rawData[which(rawData$PL_STRATUM==1),]$PL_PLOTID)),
              length(unique(rawData[which(rawData$PL_STRATUM==2),]$PL_PLOTID)),
              length(unique(rawData[which(rawData$PL_STRATUM==3),]$PL_PLOTID)))

#' General results object to use for development. 
generalResults <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                                 ns = sampSize, areas = stratumAreas, qstns = questions, 
                                 grplst = groupList)


#' ## Understory analysis development  
#' The understory areas are small, and contain many NAs. This borks things up,
#' and needs to be addressed so that they can be incorporated. 

#+ understory_dev
understoryArea <- t(generalResults$Cover$UNDERSTORY_PRESENT) * sum(stratumAreas)
colnames(understoryArea) <- c("Area_ha", "SE_Area_ha")


coverInUnder <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions, 
                               grplst = groupList, strata = strata, ns = sampSize,
                               areas = stratumAreas, cvrfld = "UNDERSTORY_COVER", 
                               cndtnfld = "UNDERSTORY_PRESENT")

coverIn2000Area <- calcError(coverInUnder, understoryArea, 3, 4, 1, 2)

#understory associations
underCrops <- c("Coffee", "Other_Crops")
treeCrops <- c("Coconut", "Fruit_Nut" ,"Pulpwood", "Rubber", "Oil_Palm", 
           "Other_Tree", "Other_Palm")

uy_occ <- build_yocc(rawData, mtdt = metaNames, qstns = questions, 
                    cvrfld = "UNDERSTORY_COVER", cndtnfld1 = "LAND_COVER", 
                    cndtnfld2 = "UNDERSTORY_PRESENT", covers = underCrops, 
                    conditions1 = treeCrops, conditions2 = "Yes")


uy_occResults <- do_yocc_analysis(uy_occ, mtdt = metaNames, strata = strata, 
                                 areas = stratumAreas, ns = sampSize, 
                                 qstns = questions, grplst = groupList)
