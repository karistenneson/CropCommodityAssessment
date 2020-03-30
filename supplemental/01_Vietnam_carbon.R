#' ---
#' title: "Viet Nam Analysis Script"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: [github_document, word_document]
#' ---
#'
#'
#' Set working directory to where data is being stored.
#+ setwd
#setwd('C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\GIA')
#setwd("~/R/GIA/")

#' ### Required packages
#+ Packages
library(tidyverse)
library(knitr)
library(rmarkdown)
source("00_GIA_functions.R")

#+ user_supplied_variables
# User needs to supply variables -----------------------------------------------

# Area of strata and sample sizes
strata <- c(1, 2, 3)
stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799, 
                  "Strata3 Area" = 31059507)
sampSize <- c(312, 576, 250)

# Metadata to save, and Grouping Variables
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")
groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

# Survey Questions
questions <-  c("LAND_COVER", "UNDERSTORY_PRESENT", "UNDERSTORY_COVER",
                "LAND_USE", "LAND_USE_YEAR_2000")


#+ inputdata
# Data input and cleaning ------------------------------------------------------
rawData <- read_csv("data/Compiled/Vietnam_points.csv", 
                    col_types = "ddddldcdcdddcdccccc")
colnames(rawData)
# clean data
rawData <- clean_data(rawData, c(15:19))

# remove unneeded columns 
keep <- c(metaNames, "FLAGGED", questions)
rawData <- rawData[,keep]

#+ Analysis
# Analysis ---------------------------------------------------------------------
generalResults <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                                 ns = sampSize, areas = stratumAreas, qstns = questions, 
                                 grplst = groupList)

# calculate top level areas
coverArea <- t(generalResults$Cover$LAND_COVER) * sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

useArea <- t(generalResults$Cover$LAND_USE) * sum(stratumAreas)
colnames(useArea) <- c("Area_ha", "SE_Area_ha")

use2000Area <- t(generalResults$Cover$LAND_USE_YEAR_2000) * sum(stratumAreas)
colnames(use2000Area) <- c("Area_ha", "SE_Area_ha")

# top level area tables
captionQ1 <- c("Table X. The estimated  area in each land cover type for Viet Nam 
              in the period after 2015. This data was collected using question 
              1 in the Collect Earth Online survey, based on Digital Globe and 
               Bing imagery.")
kable(coverArea, digits = 0, caption = captionQ1)

captionQ2 <- c("Table X. The estimated  area in each land use type for Viet Nam 
              in the period after 2015. This data was collected using question 
              2 in the Collect Earth Online survey, based on Digital Globe and 
               Bing imagery.")
kable(useArea, digits = 0, caption = captionQ2)

captionQ3 <- c("Table X. The estimated  area in each land use type for Viet Nam
                in the year 2000. This data was collected using question 3 in the 
               Collect Earth Online survey, based on Landsat time series 
               imagery.")

kable(use2000Area, digits = 0, caption = captionQ3)

#+ y_occ
# Former Forest only -----------------------------------------------------------
#' Triple conditional for area of commodities that were in year 2000 
#' natural forest areas

questions2 <- c("LAND_COVER", "LAND_USE", "LAND_USE_YEAR_2000")

crops <- c("Coconut", "Coffee", "Fruit_Nut" ,"Pulpwood", "Rice", "Rubber", 
           "Oil_Palm", "Tea", "Other_Crop")

y_occ <- build_yocc(rawData, mtdt = metaNames, qstns = questions2, 
                     cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                     cndtnfld2 = "LAND_USE_YEAR_2000", covers = crops, 
                     conditions1 = NULL, conditions2 = "Natural_Forest")


y_occResults <- do_yocc_analysis(y_occ, mtdt = metaNames, strata = strata, 
                                 areas = stratumAreas, ns = sampSize, 
                                 qstns = questions2, grplst = groupList)


# calculate areas
cResults <- bind_rows(y_occResults$Cover)

cResults %>% 
  select(cover, condition1, PercentCover) %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas)) %>% 
  spread(cover, PercentCover) %>% 
  kable(., digits = 0, col.names = c("Land Use", sort(crops)), 
        caption = c("Table X. Total area of each commodity crop, by land use, in areas 
              that were forested in the year 2000."))

cResults %>% 
  select(cover, condition1, SE) %>% 
  mutate(SE = SE * sum(stratumAreas)) %>% 
  spread(cover,SE) %>% 
  kable(., digits = 0, col.names = c("Land Use", sort(crops)), 
        caption = c("Table X. Standard error of the area of each commodity crop, by 
        land use, in areas that were forested in the year 2000."))


#+ y_oc
# Object in cover analyses -----------------------------------------------------
coverIn2000 <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                                grplst = groupList, strata = strata, ns = sampSize,
                                areas = stratumAreas, cvrfld = "LAND_COVER",
                                cndtnfld = "LAND_USE_YEAR_2000")

coverInUse <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                               grplst = groupList, strata = strata, ns = sampSize,
                               areas = stratumAreas, cvrfld = "LAND_COVER", 
                               cndtnfld = "LAND_USE")

useinCover <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                               grplst = groupList, strata = strata, ns = sampSize,
                               areas = stratumAreas, cvrfld = "LAND_USE",
                               cndtnfld = "LAND_COVER")

useinUse2000 <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                                 grplst = groupList, strata = strata, ns = sampSize,
                                 areas = stratumAreas, cvrfld = "LAND_USE",
                                 cndtnfld = "LAND_USE_YEAR_2000")

# calculate class interaction areas 
coverIn2000Area <- calcError(coverIn2000, use2000Area, 3, 4, 1, 2)

coverInUseArea <- calcError(coverInUse, useArea, 3, 4, 1, 2)

useInCoverArea <- calcError(useinCover, coverArea, 3, 4, 1, 2)

useInUse2000Area <- calcError(useinUse2000, use2000Area, 3, 4, 1, 2)

#+ prettyTables 
## produce tidy output tables for "object in cover" ----------------------------

# area of covers in 2000 uses
coverIn2000Area %>% 
  select(Cover, Condition, Value) %>% 
  spread(Condition, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each commodity crop, 
                                   in each of the land uses for that were 
                                   labelled in the year 2000."))

coverIn2000Area %>% 
  select(Cover, Condition, Error) %>% 
  spread(Condition, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of the area of each 
                                   commodity crop, in each of the land uses for 
                                   that were labelled in the year 2000."))

coverIn2000Area %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of the
                                       area of each commodity crop, in each of 
                                       the land uses that were labelled in 
                                       the year 2000."))

# area of covers in recent uses
coverInUseArea %>% 
  select(Cover, Condition, Value) %>% 
  spread(Condition, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each commodity crop, 
                                   in each of the land uses for that were 
                                   labelled in the period after 2015."))

coverInUseArea %>% 
  select(Cover, Condition, Error) %>% 
  spread(Condition, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of the area of each 
                                   commodity crop, in each of the land uses for 
                                   that were labelled in the period after 2015."))

coverInUseArea %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of the
                                       area of each commodity crop, in each of 
                                       the land uses that were labelled in the 
                                       period after 2015."))

# area of recent uses in covers
useInCoverArea %>% 
  select(Cover, Condition, Value) %>% 
  spread(Cover, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each land use from
                                   the period after 2015 occuring in each 
                                   land cover type."))

useInCoverArea %>% 
  select(Cover, Condition, Error) %>% 
  spread(Cover, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of each land use from
                                   the period after 2015 occuring in each 
                                   land cover type."))

useInCoverArea %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Cover, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of each 
                                       land use from the period after 2015 
                                       occuring in each land cover type."))

# area of recent uses in 2000 uses
useInUse2000Area %>% 
  select(Cover, Condition, Value) %>% 
  spread(Condition, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each land use from
                                   the period after 2015 occuring in each 
                                   land use type in 2000."))

useInUse2000Area %>% 
  select(Cover, Condition, Error) %>% 
  spread(Condition, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of each land use from
                                   the period after 2015 occuring in each 
                                   land use type in 2000."))

useInUse2000Area %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of each 
                                       land use from the period after 2015 
                                       occuring in each land use type in 2000."))

#' ## Understory analysis  
#' Not necessary in Viet Nam, as no crops were associated with an understory 
#' layer in the sample.

#+ Carbon
# calculate Carbon values for areas --------------------------------------------

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
comAA <- comA[,3] + comBA[,3] + comMA[,3] + comSP[,3]

comAASE <- bind_cols(A = pull(comA[,4])^2, BA = pull(comBA[,4])^2, 
                     MA = pull(comMA[,4])^2, SP = pull(comSP[,4])^2) %>% 
  rowwise() %>% 
  transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>% 
  sqrt()

allAF <- bind_cols(commodity = carbonNames, Area_ha = pull(comAA), 
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
comoPT <- comO[,3] + comP[,3] + comT[,3]
comoPTSE <- bind_cols(O = pull(comO[,4])^2, P = pull(comP[,4])^2, 
                      Tr = pull(comT[,4])^2) %>% 
  rowwise() %>% 
  transmute(SE_ha = sum(O, P, Tr, na.rm = T)) %>% 
  sqrt()

allMono <- bind_cols(commodity = carbonNames, Area_ha = pull(comoPT), 
                     SE_ha = comoPTSE)

draftMono <- transmute(allMono, commodity = commodity, 
                       Mg_C = Area_ha * carbonMono, SE = SE_ha * carbonMono)


captionMono <- c("Table X. Aboveground biomass carbon values associated with 
                 the area of commodities in monoculture plantation and terrace land 
                 uses in the period after 2015.")
kable(draftMono, caption = captionMono, digits = 0, 
      col.names = c("Commodity", "MgC", "SE"))

#+ raw_tables ------------------------------------------------------------------
kable(coverIn2000Area)
kable(coverInUseArea)
kable(useInCoverArea)
kable(useInUse2000Area)