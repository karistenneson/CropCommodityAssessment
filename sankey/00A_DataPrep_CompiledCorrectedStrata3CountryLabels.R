#' ---
#' title: "02_Indonesia_TCC_CarbonSankey.R"
#' author: "K Tenneson, karistenneson@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' ---
#'
#'
#' Set working directory to where data is being stored.
#+ setwd

setwd("C:\\Users\\karis\\Documents\\GreenInvestAsia\\githubFiles\\Final\\GIA")

#' ### Required packages
#+ Packages
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)

#+ inputdata
# Data input and cleaning ------------------------------------------------------

## sampled the FAO ecofloristic data in ArcMap and exported merged csv.
#rawDataNoPeatlands <- read.csv('data\\IndonesiaIntermediateFiles\\CleanedEcoFloristic.csv')
# rawDataOrigC <- read.csv('data\\EcoFloristicJoin\\cambodia_code_loss.csv')
# rawDataOrigI <- read.csv('data\\EcoFloristicJoin\\indonesia_code_loss.csv')
# rawDataOrigL <- read.csv('data\\EcoFloristicJoin\\laos_code_loss.csv')
# rawDataOrigM <- read.csv('data\\EcoFloristicJoin\\myanmar_code_loss.csv')
# rawDataOrigP <- read.csv('data\\EcoFloristicJoin\\philippines_code_loss.csv')
# rawDataOrigT <- read.csv('data\\EcoFloristicJoin\\thailand_code_loss.csv')
# rawDataOrigV <- read.csv('data\\EcoFloristicJoin\\vietnam_code_loss.csv')

rawDataOrigCorr <- rbind(read.csv('data\\Corrected\\Cambodia.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\Corrected\\Indonesia.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\Corrected\\Laos.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\Corrected\\Myanmar.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\Corrected\\Philippines.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\Corrected\\Thailand.csv', stringsAsFactors=FALSE), 
                     read.csv('data\\Corrected\\Vietnam.csv', stringsAsFactors=FALSE)
                     )

head(rawDataOrigCorr)
# subset columns.
rawData <- rawDataOrigCorr[,c("PLOT_ID", "PL_ORIGID", "SAMPLE_ID", "LAT", "LON", 
                          "PL_COUNTRY", "PL_STRATUM", "USER_ID", "IMAGERY_TITLE", 
                          "LAND_COVER", "UNDERSTORY_COVER", "LAND_USE","LAND_USE_YEAR_2000",
                          "PL_PLOTID")]
head(rawData)

write.csv(rawData, 'data//EcoFloristicJoin//Compiled03112020.csv')

