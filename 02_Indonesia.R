#' ---
#' title: "Indonesia Analysis Script"
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

#+ user_supplied_variables
# User needs to supply variables -----------------------------------------------

# Area of strata and sample sizes
country <- c("Indonesia")
strata <- c(1, 2, 3)
stratumAreas <- c("Strata1 Area" = 8324441, "Strata2 Area" = 	12512668, 
                  "Strata3 Area" = 169619791)

# Metadata to save, and Grouping Variables
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")
groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

# Survey Questions
questions <-  c("LAND_COVER", "UNDERSTORY_PRESENT", "UNDERSTORY_COVER",
                "LAND_USE", "LAND_USE_YEAR_2000", "TCC")

covOrder <- c("Aquaculture", "Bamboo","Coconut","Coffee","Fruit_Nut", 
              "Oil_Palm", "Pulpwood", "Rubber", "Rice", "Tea", "Other_Crop",
              "Other_Palm", "Other_Tree", "Other_Shrub", "Herbaceous", 
              "Non_vegetated", "Built_up", "Water", "Other_LAND_COVER")
useOrder <- c("Agrisiviculture", "Boundary_Agrisilviculture", 
              "Mixed_Agrisilviculture", "Silvopastoral", "Plantation", 
              "Terrace", "Natural_Forest", "Other_LAND_USE")

#+ inputdata
# Data input and cleaning ------------------------------------------------------
dataPath <- c("data/Indonesia")
files <- dir(dataPath)

rawData <- files %>% 
  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))


# clean data
rawData <- clean_data(rawData, c(15:19))

# Add a tree canopy cover variable
rawData <- rawData %>% 
  mutate(TCC = case_when( 
    LAND_USE_YEAR_2000 == "Forest_Commodity" ~ "Tree",
    LAND_USE_YEAR_2000 == "Natural_Forest" ~ "Tree",
    LAND_USE_YEAR_2000 == "Other_LAND_USE_YEAR_2000" ~ "Not_Tree"
  )
)

# remove unneeded columns 
keep <- c(metaNames, "FLAGGED", questions)
rawData <- rawData[,keep]


#Determine actual sample size
sampSize <- c(length(unique(rawData[which(rawData$PL_STRATUM == 1),]$PL_PLOTID)),
              length(unique(rawData[which(rawData$PL_STRATUM == 2),]$PL_PLOTID)),
              length(unique(rawData[which(rawData$PL_STRATUM == 3),]$PL_PLOTID))
              )

#+ Analysis
# Analysis ---------------------------------------------------------------------
generalResults <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                                 ns = sampSize, areas = stratumAreas, qstns = questions, 
                                 grplst = groupList)

# calculate top level areas
coverArea <- t(generalResults$Cover$LAND_COVER) * sum(stratumAreas)
colnames(coverArea) <- c("Area (ha)", "SE Area (ha)")

useArea <- t(generalResults$Cover$LAND_USE) * sum(stratumAreas)
colnames(useArea) <- c("Area (ha)", "SE Area (ha)")

use2000Area <- t(generalResults$Cover$LAND_USE_YEAR_2000) * sum(stratumAreas)
colnames(use2000Area) <- c("Area (ha)", "SE Area (ha)")

tcc2000Area <- t(generalResults$Cover$TCC) * sum(stratumAreas)
colnames(tcc2000Area) <- c("Area (ha)", "SE Area (ha)")

# top level area tables
coverAreaPretty <- as_tibble(coverArea, rownames = "cover")
colnames(coverAreaPretty) <- c("cover", "Area (ha)", "SE Area (ha)")
coverAreaPretty <- arrange(coverAreaPretty, match(cover,  covOrder))
captionQ1 <- paste("Table R1. The estimated  area in each land cover type for",
                    country, "in the period after 2015. This data was collected",
                    "using question 1 in the Collect Earth Online survey,", 
                    "based on Digital Globe and Bing imagery.")
kable(coverAreaPretty, digits = 0, caption = captionQ1)


useAreaPretty <- as_tibble(useArea,rownames = "use")
colnames(useAreaPretty) <- c("use", "Area (ha)", "SE Area (ha)")
useAreaPretty <- arrange(useAreaPretty, match(use, useOrder))
captionQ2 <- paste("Table R2. The estimated  area in each land use type for",
               country, "in the period after 2015. This data was collected", 
               "using question 2 in the Collect Earth Online survey,",
               "based on Digital Globe and Bing imagery.")
kable(useAreaPretty, digits = 0, caption = captionQ2)

captionQ3 <- paste("Table R3. The estimated  area in each land use type for", 
                   country, "in the year 2000. This data was collected", 
                   "using question 3 in the Collect Earth Online survey,", 
                   "based on Landsat time series imagery.")
kable(use2000Area, digits = 0, caption = captionQ3)

captionQ4 <- paste("Table R4. The estimated  area of tree canopy cover in",
                   country, "for the year 2000. This data was collected using",
                   "question 3 in the Collect Earth Online survey,",
                   "based on Landsat time series imagery.")
kable(tcc2000Area, digits = 0, caption = captionQ4)

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

# area of covers in recent uses
coverInUseArea %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  arrange(match(Cover, covOrder)) %>%
  select(Cover, useOrder) %>% 
  kable(., align = "lrrr", 
    caption = c("Table R5. Estimate of area and standard error of the area 
                    of each land cover (in hectares), that occurred in each of 
                    the land uses that were labeled for the period after 2015. 
                    All the agrisilvicultural land uses have been summed for 
                    readability.  ‘Other Crop’ contains any agricultural or 
                    crop land covers that could not be identified during the 
                    photo interpretation process. ‘Other Land Cover’ contains 
                    all other land covers that did not fit into the other 
                    categories in strata 1 & 2, and all land covers in plots 
                    in stratum 3 that did not experience forest loss or gain."))


# area of covers in 2000 uses
coverIn2000Area %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  arrange(match(Cover, covOrder)) %>%
  kable(., align = "lrrr", 
    caption = c("Table R6. Estimate of area and standard error of the area of each
              land cover (in hectares), that occurred in each of the land uses 
              that were labelled in the year 2000. ‘Other Crops’ contains any 
              agricultural or crop land covers that could not be identified 
              during the photo interpretation process. ‘Other Land Cover’ 
              contains all other land covers that did not fit into the other 
              categories in strata 1 & 2, and all land covers in plots in 
              stratum 3 that did not experience forest loss or gain."))

# area of recent uses in covers
useInCoverArea %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Cover, Pretty) %>% 
  arrange(match(Condition, useOrder)) %>%
  select(Condition, useOrder) %>% 
  kable(., align = "lrrr", 
    caption = c("Table R7. Estimate of area and standard error of the area of each
              land cover (in hectares), that occurred in each of the land uses 
              that were labelled in the year 2015. ‘Other Crops’ contains any 
              agricultural or crop land covers that could not be identified 
              during the photo interpretation process. ‘Other Land Cover’ 
              contains all other land covers that did not fit into the other 
              categories in strata 1 & 2, and all land covers in plots in 
              stratum 3 that did not experience forest loss or gain."))


# area of recent uses in 2000 uses
useInUse2000Area %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>%
  spread(Condition, Pretty) %>% 
  arrange(match(Cover, useOrder)) %>% 
  kable(., align = "lrrr", 
    caption = c("Table R8. Estimate area and standard error (in hectares) of
                each land use from the period after 2015 occuring in each land 
                use type in 2000. ‘Other Land Use’ contains all other land 
                covers that did not fit into the other categories"))

#+ y_occ1
# Former Forest only -----------------------------------------------------------
#' Triple conditional for area of commodities that were forested in the year
#' 2000. 

questions2 <- c("LAND_COVER", "LAND_USE", "LAND_USE_YEAR_2000")

crops <- c("Aquaculture", "Coconut", "Coffee", "Fruit_Nut" ,"Pulpwood", "Rice", 
           "Rubber", "Oil_Palm", "Tea", "Other_Crop", "Other_Tree", "Other_Palm",
           "Other_Shrub", "Herbaceous", "Other_LAND_COVER")

y_occ <- build_yocc(rawData, mtdt = metaNames, qstns = questions2, 
                    cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                    cndtnfld2 = "LAND_USE_YEAR_2000", covers = crops, 
                    conditions1 = NULL, conditions2 = "Natural_Forest")


y_occResults <- do_yocc_analysis(y_occ, mtdt = metaNames, strata = strata, 
                                 areas = stratumAreas, ns = sampSize, 
                                 qstns = questions2, grplst = groupList)


# calculate areas, make table
cResults <- bind_rows(y_occResults$Cover)

cTableP1 <- cResults %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas), 
         SE = SE * sum(stratumAreas)) %>% 
  mutate(Pretty = paste(round(PercentCover), round(SE), sep = " \u00B1 ")) %>% 
  select(cover, condition1, Pretty) %>%
  spread(condition1, Pretty) %>% 
  arrange(match(cover, covOrder)) %>% 
  select(cover, useOrder)

kable(cTableP1, digits = 0, col.names = c("Crops", useOrder), align = "lrrr",
      caption = c("Table R9. Estimate of area and standard error of each 
      commodity land cover (in hectares), by land use, in areas that were 
      forested in the year 2000 and experienced forest cover loss between 
      2001  and 2015. ‘Other Crops’ contains any agricultural or crop land
      covers that could not be identified during the photo interpretation 
      process.  ‘Other Land Use’ contains all other land covers that did not fit
      into the other categories."))

#+ y_occ2
# Crops in TCC in 2000 ---------------------------------------------------------
#' Triple conditional for area of commodities that were had tree cover in the
#' year 2000.

questions3 <- c("LAND_COVER", "LAND_USE", "TCC")

crops <- c("Aquaculture", "Coconut", "Coffee", "Fruit_Nut" ,"Pulpwood", "Rice", 
           "Rubber", "Oil_Palm", "Tea", "Other_Crop", "Other_Tree", "Other_Palm",
           "Other_Shrub", "Herbaceous")

y_occ2 <- build_yocc(rawData, mtdt = metaNames, qstns = questions3, 
                     cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                     cndtnfld2 = "TCC", covers = crops, 
                     conditions1 = NULL, conditions2 = "Tree")


y_occResults2 <- do_yocc_analysis(y_occ2, mtdt = metaNames, strata = strata, 
                                  areas = stratumAreas, ns = sampSize, 
                                  qstns = questions3, grplst = groupList)


# calculate areas
cResults2 <- bind_rows(y_occResults2$Cover)

cTableP2 <- cResults2 %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas), 
         SE = SE * sum(stratumAreas)) %>% 
  mutate(Pretty = paste(round(PercentCover), round(SE), sep = " \u00B1 ")) %>% 
  select(cover, condition1, Pretty) %>% 
  spread(condition1, Pretty) %>% 
  arrange(match(cover, covOrder)) %>% 
  select(cover, useOrder)


kable(cTableP2, digits = 0, col.names = c("Crops", useOrder), align = "lrrr",
      caption = c("Table R10. Estimate of area and standard error of each 
      commodity land cover (in hectares), by land use, in areas that had tree 
      cover in the year 2000 and experienced tree canopy cover loss between 
      2001  and 2015. ‘Other Crops’ contains any agricultural or crop land
      covers that could not be identified during the photo interpretation 
      process.  ‘Other Land Use’ contains all other land covers that did not fit
      into the other categories."))


#+ y_occ3
# Support TCC in 2000 ----------------------------------------------------------
#' Triple conditional for "support area" of commodities that were had tree cover
#' in the year 2000.
questions4 <- c("LAND_COVER", "LAND_USE", "TCC")

support <- c("Built_up", "Non_vegetated", "Other_LAND_COVER", "Water")

y_occ3 <- build_yocc(rawData, mtdt = metaNames, qstns = questions4, 
                     cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                     cndtnfld2 = "TCC", covers = support, 
                     conditions1 = NULL, conditions2 = "Tree")


y_occResults3 <- do_yocc_analysis(y_occ3, mtdt = metaNames, strata = strata, 
                                  areas = stratumAreas, ns = sampSize, 
                                  qstns = questions4, grplst = groupList)

# calculate areas
cResults3 <- bind_rows(y_occResults3$Cover)

cTableP3 <- cResults3 %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas), 
         SE = SE * sum(stratumAreas)) %>% 
  mutate(Pretty = paste(round(PercentCover), round(SE), sep = " \u00B1 ")) %>% 
  select(cover, condition1, Pretty) %>% 
  spread(condition1, Pretty) %>% 
  arrange(match(cover, covOrder)) %>% 
  select(cover, useOrder)

kable(cTableP3, digits = 0, col.names = c("Crops", useOrder), align = "lrrr",
      caption = c("Table R11. Estimate of area and standard error of each 
      commodity \'support\' land cover (in hectares), by land use, in areas that
      had tree cover in the year 2000 and experienced tree canopy loss between 
      2001  and 2015. ‘Other Crops’ contains any agricultural or crop land
      covers that could not be identified during the photo interpretation 
      process.  ‘Other Land Use’ contains all other land covers that did not fit
      into the other categories."))


#+ Carbon
# Carbon Factors ---------------------------------------------------------------

# Note, Fruit/Nut value is temporary, should really be a range, not average
# Remember to adjust for commodities present in each country. 
carbonMono <- c("Coconut" = 32, 
                "Coffee" = 18.9, 
                "Fruit_Nut" = 42.1, 
                "Oil_Palm" = 38.97,
                "Pulpwood" = 38.2, 
                "Rubber" = 38.2,
                "Rice" = 1, 
                "Other_Crop" = 5,
                "Other_Palm" = 32,
                "Other_Tree" = 39,
                "Other_Shrub" = 22.5,
                "Herbaceous" = 5)


carbonAF <- c("Coconut" = 32, 
              "Coffee" = 32.8, 
              "Fruit_Nut" = 42.1,
              "Oil_Palm" = 38.97, 
              "Pulpwood" = 38.2,
              "Rubber" = 38.2,
              "Rice" = 1,
              "Other_Crop" = 5,
              "Other_Palm" = 32,
              "Other_Tree" = 39,
              "Other_Shrub" = 32.8,
              "Herbaceous" = 5)

kable(cbind(carbonMono, carbonAF), digits = 1, 
      col.names = c("Monoculture", "Agroforestry"),
      caption = c("Table R12. Carbon factors used to calculate the aboveground 
      biomass for each commodity. Values for commodities were compiled for 
      monoculture and agroforestry systems from peer-reviewed and grey 
      literature. Time-averaged values are often used to estimate the carbon 
      storage of rotational commodity crops because they allow for the 
      “averaging” of a mixture of freshly replanted and mature commodity areas 
      across the landscape. Sources for the carbon values presented here are 
      included in Appendix 1."))

# calculate Carbon values for previously tree-covered areas --------------------

# make results into an area
tccAreas <- arrange(cResults2, match(cover, covOrder)) %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas),
         SE = SE * sum(stratumAreas))

## Agroforestry Carbon
tccA <- tccAreas[seq(1,112,8),][-c(1,9),] #keep all elements
  
tccBA <- tccAreas[seq(2,112,8),][-c(1,9),] #keep all elements

tccMA <- tccAreas[seq(3,112,8),][-c(1,9),] #keep all elements

tccSP <- tccAreas[seq(7,112,8),][-c(1,9),]
#remove herbaceous, other palm, other shrub, other tree
tccSP[,c(4,5)] <- tccSP[,c(4,5)] * c(1,1,1,1,1,1,1,1,0,0,0,0) 

tccAA <- tccA[,4] + tccBA[,4] + tccMA[,4] + tccSP[,4]

tccAASE <- bind_cols(A = pull(tccA[,5])^2, BA = pull(tccBA[,5])^2, 
                    MA = pull(tccMA[,5])^2, SP = pull(tccSP[,5])^2) %>% 
  rowwise() %>% 
  transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>% 
  sqrt()

tccAF <- bind_cols(commodity = pull(tccAreas[seq(1,112,8),1][-c(1,9),]), 
                   Area_ha = pull(tccAA), 
                   SE_ha = tccAASE)

tccAF <- transmute(tccAF,  commodity = commodity, Mg_C = Area_ha * carbonAF, 
                   SE = SE_ha * carbonAF)
 
captiontccAF <- c("Table R13. Aboveground biomass carbon values (1,000s Mg C) 
 associated with the area of commodities in agroforestry land uses that occur in
 formerly tree covered areas.")

kable(tccAF, caption = captiontccAF, digits = 0,
       col.names = c("Commodity", "MgC", "SE"))

## Calculate carbon in associated with non-AF areas
tccO <- tccAreas[seq(5,112,8),][-c(1,9),]
#remove herbaceous, other palm, other shrub, other tree
tccO[,c(4,5)] <- tccO[,c(4,5)] * c(1,1,1,1,1,1,1,1,0,0,0,0)

tccP <- tccAreas[seq(6,112,8),][-c(1,9),] #keep all elements

tccT <- tccAreas[seq(8,112,8),][-c(1,9),] #keep all elements

tccOPT <- tccO[,4] + tccP[,4] + tccT[,4]

tccOPTSE <- bind_cols(O = pull(tccO[,5])^2, P = pull(tccP[,5])^2, 
                     Tr = pull(tccT[,5])^2) %>% 
  rowwise() %>% 
  transmute(SE_ha = sum(O, P, Tr, na.rm = T)) %>% 
  sqrt()

tccMono <- bind_cols(commodity = pull(tccAreas[seq(1,112,8),1][-c(1,9),]), 
                     Area_ha = pull(tccOPT), 
                     SE_ha = tccOPTSE)

tccMono <- transmute(tccMono, commodity = commodity, 
                   Mg_C = Area_ha * carbonMono, SE = SE_ha * carbonMono)


captiontccMono <- c("Table R14. Aboveground biomass carbon values (1,000s Mg C) 
  associated with the area of commodities in monoculture plantation and terrace 
  land that occur in formerly tree-covered areas.")

kable(tccMono, caption = captiontccMono, digits = 0, 
     col.names = c("Commodity", "MgC", "SE"))

# Calculate Carbon for Formerly Forested Areas ---------------------------------

# # Calculate total carbon associated with agroforestry systems
# comA <- coverInUseArea[seq(1, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comBA <- coverInUseArea[seq(2, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comMA <- coverInUseArea[seq(3, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comSP <- coverInUseArea[seq(7, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comAA <- comA[,3] + comBA[,3] + comMA[,3] + comSP[,3]
# 
# comAASE <- bind_cols(A = pull(comA[,4])^2, BA = pull(comBA[,4])^2, 
#                      MA = pull(comMA[,4])^2, SP = pull(comSP[,4])^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>% 
#   sqrt()
# 
# allAF <- bind_cols(commodity = carbonNames, Area_ha = pull(comAA), 
#                    SE_ha = comAASE)
# 
# draftAF <- transmute(allAF,  commodity = commodity, Mg_C = Area_ha * carbonAF, 
#                      SE = SE_ha * carbonAF)
# 
# captionAF <- c("Table X. Aboveground biomass carbon values associated with 
#                the area of commodities in agroforestry land uses in the period 
#                after 2015.")
# kable(draftAF, caption = captionAF, digits = 0,
#       col.names = c("Commodity", "MgC", "SE"))
# 
# 
# # Calculate carbon in associated with plantation areas
# comO <- coverInUseArea[seq(5, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comP <- coverInUseArea[seq(6, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comT <- coverInUseArea[seq(8, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comOPT <- comO[,3] + comP[,3] + comT[,3]
# comOPTSE <- bind_cols(O = pull(comO[,4])^2, P = pull(comP[,4])^2, 
#                       Tr = pull(comT[,4])^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(O, P, Tr, na.rm = T)) %>% 
#   sqrt()
# 
# allMono <- bind_cols(commodity = carbonNames, Area_ha = pull(comOPT), 
#                      SE_ha = comOPTSE)
# 
# draftMono <- transmute(allMono, commodity = commodity, 
#                        Mg_C = Area_ha * carbonMono, SE = SE_ha * carbonMono)
# 
# 
# captionMono <- c("Table X. Aboveground biomass carbon values associated with 
#                  the area of commodities in monoculture plantation and terrace land 
#                  uses in the period after 2015.")
# kable(draftMono, caption = captionMono, digits = 0, 
#       col.names = c("Commodity", "MgC", "SE"))

# calculate Carbon values for previously forested areas ------------------------
# make results into an area
# ffAreas <- arrange(cResults, cover) %>% 
#   mutate(PercentCover = PercentCover * sum(stratumAreas),
#          SE = SE * sum(stratumAreas))
# 
# # agroforestry carbon
# ffA <- ffAreas[seq(1,72,8),][-9,]
# ffBA <- ffAreas[seq(2,72,8),][-9,]
# ffMA <- ffAreas[seq(3,72,8),][-9,]
# ffSP <- ffAreas[seq(7,72,8),][-9,]
# ffAA <- ffA[,4] + ffBA[,4] + ffMA[,4] + ffSP[,4]
# 
# ffAASE <- bind_cols(A = pull(ffA[,5])^2, BA = pull(ffBA[,5])^2, 
#                      MA = pull(ffMA[,5])^2, SP = pull(ffSP[,5])^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>% 
#   sqrt()
# 
# ffAF <- bind_cols(commodity = carbonNames, Area_ha = pull(ffAA), 
#                    SE_ha = ffAASE)
# 
# ffAF <- transmute(ffAF,  commodity = commodity, Mg_C = Area_ha * carbonAF, 
#                      SE = SE_ha * carbonAF)
# 
# captionffAF <- c("Table X. Aboveground biomass carbon values associated with 
#                the area of commodities in agroforestry land uses that occur in
#                  formerly forested areas.")
# kable(ffAF, caption = captionffAF, digits = 0,
#       col.names = c("Commodity", "MgC", "SE"))
# 
# # Calculate carbon in associated with non-AF areas
# ffO <- ffAreas[seq(5,72,8),][-9,]
# ffP <- ffAreas[seq(6,72,8),][-9,]
# ffT <- ffAreas[seq(8,72,8),][-9,]
# ffOPT <- ffO[,4] + ffP[,4] + ffT[,4]
# ffOPTSE <- bind_cols(O = pull(ffO[,5])^2, P = pull(ffP[,5])^2, 
#                       Tr = pull(ffT[,5])^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(O, P, Tr, na.rm = T)) %>% 
#   sqrt()
# 
# ffMono <- bind_cols(commodity = carbonNames, Area_ha = pull(ffOPT), 
#                      SE_ha = ffOPTSE)
# 
# ffMono <- transmute(ffMono, commodity = commodity, 
#                        Mg_C = Area_ha * carbonMono, SE = SE_ha * carbonMono)
# 
# 
# captionffMono <- c("Table X. Aboveground biomass carbon values associated with 
#                  the area of commodities in monoculture plantation and terrace land 
#                  that occur in formerly forested areas.")
# kable(ffMono, caption = captionffMono, digits = 0, 
#       col.names = c("Commodity", "MgC", "SE"))


#+ Understory
# Understory -------------------------------------------------------------------
#' ## Understory analysis  
#' The understory areas are small, but contain a small areas that need to be 
#' accounted for in the analysis. 

#+ understory
questions3 <- c("UNDERSTORY_PRESENT", "UNDERSTORY_COVER")
understoryArea <- t(generalResults$Cover$UNDERSTORY_PRESENT) * sum(stratumAreas)
colnames(understoryArea) <- c("Area_ha", "SE_Area_ha")


coverInUnder <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions3, 
                                grplst = groupList, strata = strata, ns = sampSize,
                                areas = stratumAreas, cvrfld = "UNDERSTORY_COVER", 
                                cndtnfld = "UNDERSTORY_PRESENT")

coverInUnderArea <- calcError(coverInUnder, understoryArea, 3, 4, 1, 2)

coverInUnderArea %>% 
  select(Cover, Condition, Value) %>% 
  spread(Condition, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each land cover from
                                   the period after 2015 occuring in the 
                                   understory."))
coverInUnderArea %>% 
  select(Cover, Condition, Error) %>% 
  spread(Condition, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of each land cover from
                                   the period after 2015 occuring in the understory."))

coverInUnderArea %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of each 
                                       land cover from the period after 2015 
                                       occuring in the understory."))

#understory associations
underCrops <- c("Coffee", "Other_Crop")
treeCrops <- c("Coconut", "Fruit_Nut" ,"Pulpwood", "Rubber", "Oil_Palm", 
               "Other_Tree", "Other_Palm", "Other_Crop")

uy_occ <- build_yocc(rawData, mtdt = metaNames, qstns = questions, 
                     cvrfld = "UNDERSTORY_COVER", cndtnfld1 = "LAND_COVER", 
                     cndtnfld2 = "UNDERSTORY_PRESENT", covers = underCrops, 
                     conditions1 = treeCrops, conditions2 = "Yes")


uy_occResults <- do_yocc_analysis(uy_occ, mtdt = metaNames, strata = strata, 
                                  areas = stratumAreas, ns = sampSize, 
                                  qstns = questions, grplst = groupList)

uResults <- bind_rows(uy_occResults$Cover)

uResults %>% 
  select(cover, condition1, PercentCover) %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas)) %>% 
  spread(cover, PercentCover) %>% 
  kable(., digits = 0, col.names = c("Land Use", sort(underCrops)), 
        caption = c("Table X. Total area of each understory commodity crop, 
        in each of the associated overstory crops."))

uResults %>% 
  select(cover, condition1, SE) %>% 
  mutate(SE = SE * sum(stratumAreas)) %>% 
  spread(cover,SE) %>% 
  kable(., digits = 0, col.names = c("Land Use", sort(underCrops)), 
        caption = c("Table X. Total area of each understory commodity crop, 
        in each of the associated overstory crops."))

#+ Extra_tables ------------------------------------------------------------------
## Raw Tables 
# kable(coverIn2000Area)
# kable(coverInUseArea)
# kable(useInCoverArea)
# kable(useInUse2000Area)