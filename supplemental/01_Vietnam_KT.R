#' ---
#' title: "Vietnam Analysis Script"
#' author: "MS Patterson, tertiarymatt@gmail.com"
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

#+ user supplied variables

# Area of strata and sample sizes
strata <- c(1, 2, 3)
stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799, 
                  "Strata3 Area" = 31059507)
sampSize <- c(312, 576, 250)

# Metadata to save, and Grouping Variables
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")
groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

#+ inputdata
rawData <- read_csv("data/Compiled/Vietnam_points.csv", 
                    col_types = "ddddldcdcdddcdccccc")
rawData$CropsPlus <- rawData$LAND_COVER
rawData$CropsPlus[(rawData$LAND_COVER == "Coffee" |
                      rawData$LAND_COVER == "Tea" |
                      rawData$LAND_COVER == "Rubber" |
                      rawData$LAND_COVER == "Bamboo" |
                      rawData$LAND_COVER == "Pulpwood" |
                      rawData$LAND_COVER == "Fruit_Nut" |
                      rawData$LAND_COVER == "Rice" |
                      rawData$LAND_COVER == "Other_Crop")] <- 'CropCov'
data <- table(rawData$LAND_USE,rawData$CropsPlus)
write.table(data, 'cropCovbyLU', sep = ',')

rawData$ForestLoss <- 'None'
rawData$ForestLoss[(rawData$LAND_COVER == "Coffee" |
  rawData$LAND_COVER == "Tea" |
  rawData$LAND_COVER == "Rubber" |
  rawData$LAND_COVER == "Bamboo" |
  rawData$LAND_COVER == "Pulpwood" |
  rawData$LAND_COVER == "Fruit_Nut" |
  rawData$LAND_COVER == "Rice" |
  rawData$LAND_COVER == "Other_Crop") &
  (rawData$LAND_USE_YEAR_2000 == "Natural_Forest") &
  (rawData$LAND_USE != "Natural_Forest")] <- 'Forest_2_Crop'
  
# Survey Questions
questions <-  c("LAND_COVER", "UNDERSTORY_PRESENT", "UNDERSTORY_COVER",
                "LAND_USE", "LAND_USE_YEAR_2000")

# remove unneeded columns 
keep <- c(metaNames, "FLAGGED", questions)
rawData <- rawData[,keep]


#+ Analysis
generalResults <- do_analysis(table = rawData, mtdt = metaNames, strata = strata,
                       ns = sampSize, areas = stratumAreas, qstns = questions, 
                       grplst = groupList)

# calculate top level areas
coverArea <- t(generalResults$Cover$LAND_COVER) * sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

useArea <- t(generalResults$Cover$LAND_USE) * sum(stratumAreas)
colnames(useArea) <- c("Area_ha", "SE_Area_ha")

use2000Area <- t(generalResults$Cover$LAND_USE_YEAR_2000) * sum(stratumAreas)
colnames(use2000Area) <- c("Area_ha", "SE_Area_ha")

# top level areas
captionQ1 <- c("Table X. The estimated  area in each land cover type for Vietnam 
              in the period after 2015. This data was collected using question 
              3 in the Collect Earth Online survey, based on Digital Globe and 
               Bing imagery.")
kable(coverArea, digits = 0, caption = captionQ1)

captionQ2 <- c("Table X. The estimated  area in each land use type for Vietnam 
              in the period after 2015. This data was collected using question 
              3 in the Collect Earth Online survey, based on Digital Globe and 
               Bing imagery.")
kable(useArea, digits = 0, caption = captionQ2)

captionQ3 <- c("Table X. The estimated  area in each land use type for Vietnam in the 
               year 2000. This data was collected using question 3 in the 
               Collect Earth Online survey, based on Landsat time series 
               imagery.")

kable(use2000Area, digits = 0, caption = captionQ3)

coverIn2000 <- do_y_oc_analysis(rawData, mtdt = metaNames, qstns = questions, 
                  grplst = groupList, strata = strata, ns = sampSize,
                  areas = stratumAreas, cover = "LAND_COVER",
                  condition = "LAND_USE_YEAR_2000")

coverInUse <- do_y_oc_analysis(rawData, mtdt = metaNames, qstns = questions, 
                  grplst = groupList, strata = strata, ns = sampSize,
                  areas = stratumAreas, cover = "LAND_COVER", 
                  condition = "LAND_USE")

useinCover <- do_y_oc_analysis(rawData, mtdt = metaNames, qstns = questions, 
                  grplst = groupList, strata = strata, ns = sampSize,
                  areas = stratumAreas, cover = "LAND_USE",
                  condition = "LAND_COVER")

useinUse2000 <- do_y_oc_analysis(rawData, mtdt = metaNames, qstns = questions, 
                               grplst = groupList, strata = strata, ns = sampSize,
                               areas = stratumAreas, cover = "LAND_USE",
                               condition = "LAND_USE_YEAR_2000")

# calculate class interaction areas 
coverIn2000Area <- calcError(coverIn2000, use2000Area)
colnames(coverIn2000Area) <- c("Area_ha", "SE_Area_ha")

coverInUseArea <- calcError(coverInUse, useArea)
colnames(coverInUseArea) <- c("Area_ha", "SE_Area_ha")

useInCoverArea <- calcError(useinCover, coverArea)
colnames(useInCoverArea) <- c("Area_ha", "SE_Area_ha")

useInUse2000Area <- calcError(useinUse2000, use2000Area)
colnames(useInUse2000Area) <- c("Area_ha", "SE_Area_ha")

kable(coverIn2000Area)
kable(coverInUseArea)
kable(useInCoverArea)
kable(useInUse2000Area)

#+ prettyTables1, results='asis'
# produce tidy output tables
prettyCoverin2000Area <- makePretty(coverIn2000Area)

for (t in 1:length(prettyCoverin2000Area)) {
  caption <- paste0("Table X. Estimated area of commodities in the ", 
                    names(prettyCoverin2000Area)[t], 
                    " land use area in the year 2000.")
  print(kable(prettyCoverin2000Area[[t]], digits = 0, caption = caption,
              col.names = c("Cover", "Area (ha)", "SE (ha)")))
}

#+ prettyTables2, results='asis'
# produce tidy output tables
prettyCoverinUseArea <- makePretty(coverInUseArea)

for (t in 1:length(prettyCoverinUseArea)) {
  caption <- paste0("Table X. Estimated areas of commodities in ", 
                    names(prettyCoverinUseArea)[t], 
                    " land use area after 2015.")
  print(kable(prettyCoverinUseArea[[t]], digits = 0, caption = caption,
              col.names = c("Cover", "Area (ha)", "SE (ha)")))
}

#+ prettyTables3, results='asis'
prettyUseinCoverArea <- makePretty(useInCoverArea)

for (t in 1:length(prettyUseinCoverArea)) {
  caption <- paste0("Table X. Estimated areas of agroforestry types in ", 
                    names(prettyUseinCoverArea)[t],
                    " land cover after 2015.")
  print(kable(prettyUseinCoverArea[[t]], digits = 0, caption = caption,
              col.names = c("Cover", "Area (ha)", "SE (ha)")))
}

# produce tidy output tables
prettyUseInUse2000Area <- makePretty(useInUse2000Area)

for (t in 1:length(prettyUseInUse2000Area)) {
  caption <- paste0("Table X. Estimated area of land uses after 2015 in the ", 
                    names(prettyUseInUse2000Area)[t], 
                    " land use area in the year 2000.")
  print(kable(prettyUseInUse2000Area[[t]], digits = 0, caption = caption,
              col.names = c("Cover", "Area (ha)", "SE (ha)")))
}

# calculate Carbon values for areas
# ALL OF THIS IS DRAFT

carbonNames <- c("Bamboo", "Coffee", "Fruit_Nut", "Other_Crop", "Pulpwood",
                 "Rice", "Rubber", "Tea" )

carbonMono <- c(26.5, 5, 34.07, 4, 23, 5, 31.74, 15.3)
carbonMonoSE <- c()
names(carbonMono) <- carbonNames


carbonAF <- c(26.5, 11, 34.07, 20, 23, 5, 31.74, 22)
carbonAFSE <- c()
names(carbonAF) <- carbonNames

# Calculate total carbon associated with agroforestry systems
comA <- coverInUseArea[seq(1, 120, 8), ][c(1, 3, 4, 7, 11, 12, 13, 14),]
comBA <- coverInUseArea[seq(2, 120, 8), ][c(1, 3, 4, 7, 11, 12, 13, 14),]
comMA <- coverInUseArea[seq(3, 120, 8), ][c(1, 3, 4, 7, 11, 12, 13, 14),]
comSP <- coverInUseArea[seq(7, 120, 8), ][c(1, 3, 4, 7, 11, 12, 13, 14),]
comAA <- comA[,1] + comBA[,1] + comMA[,1] + comSP[,1]

comAASE <- bind_cols(A = comA[,2]^2, BA = comBA[,2]^2, 
                     MA = comMA[,2]^2, SP = comSP[,2]^2) %>% 
  rowwise() %>% 
  transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>% 
  sqrt()

allAF <- bind_cols(commodity = carbonNames, Area_ha = comAA, 
                   SE_ha = comAASE)

draftAF <- transmute(allAF,  commodity = commodity, Mg_C = Area_ha * carbonAF, 
                 SE = SE_ha * carbonAF)

captionAF <- c("Table X. Aboveground biomass carbon values associated with 
             the area of commodities in agroforestry land uses in the period 
             after 2015.")
kable(draftAF, caption = captionAF, digits = 0,
      col.names = c("Commodity", "MgC", "SE"))

# Calculate carbon in associated with plantation areas
comO <- coverInUseArea[seq(5, 120, 8), ][c(1, 3, 4, 7, 11, 12, 13, 14),]
comP <- coverInUseArea[seq(6, 120, 8), ][c(1, 3, 4, 7, 11, 12, 13, 14),]
comT <- coverInUseArea[seq(8, 120, 8), ][c(1, 3, 4, 7, 11, 12, 13, 14),]
comoPT <- comO[,1] + comP[,1] + comT[,1]
comoPTSE <- bind_cols(O = comO[,2], P = comP[,2]^2, T = comT[,2]^2) %>% 
  rowwise() %>% 
  transmute(SE_ha = sum(P, T, na.rm = T)) %>% 
  sqrt()

allMono <- bind_cols(commodity = carbonNames, Area_ha = comoPT, 
                   SE_ha = comoPTSE)

draftMono <- transmute(allMono, commodity = commodity, 
                       Mg_C = Area_ha * carbonMono, SE = SE_ha * carbonMono)


captionMono <- c("Table X. Aboveground biomass carbon values associated with 
             the area of commodities in monoculture plantation and terrace land 
             uses in the period after 2015.")
kable(draftMono, caption = captionMono, digits = 0, 
      col.names = c("Commodity", "MgC", "SE"))
