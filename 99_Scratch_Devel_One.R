#' ---
#' title: "Old Scratch File for Development"
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
library(tidyverse)
library(knitr)
source("00_GIA_functions.R")

#+ inputdata
dataPath <- c("data/Vietnam")
files <- dir(dataPath)

test <- files %>% 
  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))

# remove unneeded columns 
dataNames <- colnames(test)
keep <- c(1,2, 5, 7, 10, 11, 13:19)
test <- test[,keep]

# remove Potapov plots
test <- subset(test, PL_STRATUM != 3)

# Clean up the data
test <- clean_data(test, c(9:13))

#+ TestingFunctions
# Test presence, using Tea
result <- presence(test, lblfld = "LAND_COVER", cmmdty = 'Tea')$Tea

# determine frequency of Tea
sum(result)

# Test question table builder
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")

landCover <- build_question(test, mtdt = metaNames, qstn = "LAND_COVER")

# Test analysis table builder
commodityNames <- c("Rubber", "Tea", "Coffee", "Pulpwood", "Coconut", "Oil_Palm")
groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

toAnalyze <- build_yc(landCover, lblfld = 'LAND_COVER', 
                           cmmdtylst = commodityNames)

# Test plot level summarization
pSummary <- plot_means(toAnalyze, grplst = groupList, cmmdtylst = commodityNames)

# Stratum summarization
sSummary <- stratum_means(toAnalyze, grplst = groupList, cmmdtylst = commodityNames)

sError <- stratum_SE(toAnalyze, grplst = groupList, cmmdtylst = commodityNames)

stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799, 
                  "Strata3 Area" = 1604577)
#stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799) 

sampSize <- c(312, 576, 425)
#sampSize <- c(312, 576)

# Overall proportion of cover and variance
p_hat_sub_c <- overall_prop(sSummary, areas = stratumAreas)
se_hat_sub_c <- overall_SE(pSummary, c(1,2), areas = stratumAreas, ns = sampSize)

p_hat_sub_c * 100
se_hat_sub_c * 100


#+ TestingWrapper
# Doing full analysis

# Extra tidbits needed for totals
ntotal <- nrow(test)/24
nstrata1 <- length(which(test$PL_STRATUM == 1))/24
nstrata2 <- length(which(test$PL_STRATUM == 2))/24
nstrata3 <- length(which(test$PL_STRATUM == 3))/24

nplots <- c("Total Plots" = ntotal, "Strata1 Plots" = nstrata1, 
            "Strata2 Plots" = nstrata2, "Strata3 Plots" = nstrata3)

kable(nplots) 

countryArea <- 32727700

metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")

groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799, 
                  "Strata3 Area" = 1604577)

strata <- sort(unique(test$PL_STRATUM))

sampSize <- c(312, 576, 425)

questions <- colnames(test[,9:13])

results <- do_yc_analysis(table = test, mtdt = metaNames, strata = strata,
           ns = sampSize, areas = stratumAreas, qstns = questions, 
           grplst = groupList)

#' ## Developing "Cover in Condition" Workflow
#+ CoverinCondition

# Test analysis table builder
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")

commodityNames <- c("Coffee", "Fruit_Nut", "Pulpwood", "Rice", "Rubber", "Tea")

groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

questions <- colnames(test[,9:13])

stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799) 

strata <- sort(unique(test$PL_STRATUM))

sampSize <- c(312, 576)

#Build tables for doing o_in_c analysis for use types in commodities
landcover <- build_question(test, mtdt = metaNames, qstn = c("LAND_COVER"))
landuse <- build_question(test, mtdt = metaNames, qstn = c("LAND_USE"))
conditions <- sort(unique(landcover$LAND_COVER))
covers <- sort(unique(landuse$LAND_USE))

y_cTable <- build_yc(landcover, lblfld = 'LAND_COVER', cmmdtylst = conditions)


y_ocTable <- build_yoc(table = test, mtdt = metaNames, qstns = questions, 
                      grplst = groupList, cvrfld = "LAND_USE", 
                      cndtnfld = "LAND_COVER", conditions = conditions, 
                      covers = covers)

p_hat_o_in_c <- cond_prop(y_ocTable, y_cTable, areas = stratumAreas, grplst = groupList, 
          conditions = conditions)

se_p_hat_o_in_c <- cond_SE(yoctable = y_ocTable, yctable = y_cTable, strata = strata, 
        grplst = groupList, conditions = conditions, covers = covers, 
        areas = stratumAreas, ns = sampSize)

#cbind(p_hat_o_in_c, se_p_hat_o_in_c) * 100
use_in_rubber <- cbind(p_hat_o_in_c[seq(13, 120, 15)], se_p_hat_o_in_c[seq(13, 120, 15)])

#Build tables for doing o_in_c analysis for commodities in use types
covers2 <- sort(unique(landcover$LAND_COVER))
conditions2 <- sort(unique(landuse$LAND_USE))

y_cTable2 <- build_yc(landuse, lblfld = 'LAND_USE', cmmdtylst = conditions2)


y_ocTable2 <- build_yoc(table = test, mtdt = metaNames, qstns = questions, 
                        grplst = groupList, cvrfld = "LAND_COVER", 
                        cndtnfld = "LAND_USE", conditions = conditions2, 
                        covers = covers2)

p_hat_o_in_c2 <- cond_prop(y_ocTable2, y_cTable2, areas = stratumAreas, grplst = groupList, 
                          conditions = conditions2)

se_p_hat_o_in_c2 <- cond_SE(yoctable = y_ocTable2, yctable = y_cTable2, strata = strata, 
                           grplst = groupList, conditions = conditions2, covers = covers2, 
                           areas = stratumAreas, ns = sampSize)

#cbind(p_hat_o_in_c2, se_p_hat_o_in_c2) * 100
com_in_agslv <- cbind(p_hat_o_in_c2[seq(1, 120, 8)], se_p_hat_o_in_c2[seq(1, 120, 8)])


#Build tables for doing o_in_c analysis for commodities in year 2000 covers
landuse2000 <- build_question(test, mtdt = metaNames, qstn = c("LAND_USE_YEAR_2000"))

covers3 <- sort(unique(landcover$LAND_COVER))
conditions3 <- sort(unique(landuse2000$LAND_USE_YEAR_2000))

y_cTable3 <- build_yc(landuse2000, lblfld = 'LAND_USE_YEAR_2000', cmmdtylst = conditions3)


y_ocTable3 <- build_yoc(table = test, mtdt = metaNames, qstns = questions, 
                         grplst = groupList, cvrfld = "LAND_COVER", 
                         cndtnfld = "LAND_USE_YEAR_2000", conditions = conditions3, 
                         covers = covers3)

p_hat_o_in_c3 <- cond_prop(y_ocTable3, y_cTable3, areas = stratumAreas, grplst = groupList, 
                           conditions = conditions3)

se_p_hat_o_in_c3 <- cond_SE(yoctable = y_ocTable3, yctable = y_cTable3, strata = strata, 
                            grplst = groupList, conditions = conditions3, covers = covers3, 
                            areas = stratumAreas, ns = sampSize)

com_in_2000 <- cbind(p_hat_o_in_c3, se_p_hat_o_in_c3)
com_in_forest <- cbind(p_hat_o_in_c3[seq(1, 45, 3)], se_p_hat_o_in_c3[seq(1, 45, 3)])

do_yoc_analysis(test, mtdt = metaNames, qstns = questions, 
                             grplst = groupList, strata = strata, ns = sampSize,
                             areas = stratumAreas, cover = "LAND_COVER",
                             condition = "LAND_USE_YEAR_2000")

#' ## Figure out areas and what not

#+ areaanderror

#coverHA <- round(t(results$Cover$LAND_COVER) * countryArea, 0)
coverHA <- round(t(results$Cover$LAND_COVER) * sum(stratumAreas), 0)
colnames(coverHA) <- c("Area_ha", "SE_Area_ha")

#useHA <- round(t(results$Cover$LAND_USE) * countryArea, 0)
useHA <- round(t(results$Cover$LAND_USE) * sum(stratumAreas), 0)
colnames(useHA) <- c("Area_ha", "SE_Area_ha")

#use2000HA <- round(t(results$Cover$LAND_USE_YEAR_2000) * countryArea, 0)
use2000HA <- round(t(results$Cover$LAND_USE_YEAR_2000) * sum(stratumAreas), 0)
colnames(use2000HA) <- c("Area_ha", "SE_Area_ha")

coverHA
useHA
use2000HA

# one commodity example
rubberAreas <- use_in_rubber[,1] * coverHA["Rubber", "Area_ha"]
rubberAreas_SE <- rubberAreas * sqrt((use_in_rubber[,2]/use_in_rubber[,1])^2 + 
                     (coverHA["Rubber", "SE_Area_ha"]/coverHA["Rubber", "Area_ha"])^2)

use_in_rubber_ha <- round(cbind(rubberAreas, rubberAreas_SE), 2) 
colnames(use_in_rubber_ha) <- c("Area_ha", "SE_Area_ha")
use_in_rubber_ha

com_in_agslv
com_in_forest 

p_hat_o_in_c3 * use2000HA[,1]
p_hat_o_in_c3 * use2000HA[,1] * sqrt((se_p_hat_o_in_c3/p_hat_o_in_c3)^2 + (use2000HA[,2]/use2000HA[,1])^2)

com_in_2000_Areas <- calcError(com_in_2000, use2000HA)

contents <- str_split(rownames(com_in_2000_Areas), "-", 3, simplify = T)
data <- tibble(condition = contents[,3], cover = contents[,1], 
                  area_ha = com_in_2000_Areas[,1], se_area = com_in_2000_Areas[,2])
conditions <- unique(data$condition)

prettyTable <- list()
for (c in seq_along(conditions)) {
  prettyTable[[c]] <- filter(data, condition == conditions[c])
  prettyTable[[c]] <- prettyTable[[c]][,-1]
  names(prettyTable)[c] <- conditions[c]
}

prettyTable

# Test triple conditional 

questions <- c("LAND_COVER", "LAND_USE", "LAND_USE_YEAR_2000")
crops <- c("Coffee", "Fruit_Nut" ,"Pulpwood", "Rubber", "Tea", "Other_Crop")

y_occ_test <- build_yocc(test, mtdt = metaNames, qstns = questions, 
            cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
            cndtnfld2 = "LAND_USE_YEAR_2000", covers = crops, conditions1 = NULL,
            conditions2 = "Natural_Forest")

sum(test$LAND_COVER == "Pulpwood" & test$LAND_USE == "Plantation" & 
      test$LAND_USE_YEAR_2000 == "Natural_Forest")

yoccaTest <- do_yocc_analysis(y_occ_test, mtdt = metaNames, strata = strata, 
                              areas = stratumAreas, ns = sampSize, qstns = questions, 
                              grplst = groupList)

cResults <- bind_rows(yoccaTest$Cover)

cResults %>% 
  select(cover, condition1, PercentCover) %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas)) %>% 
  spread(cover, PercentCover)

