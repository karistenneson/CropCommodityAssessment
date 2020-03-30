#' ---
#' title: "QA/QC for Training Data"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' Set working directory to where data is being stored.
#+ setwd
setwd("~/R/GIA/")

#' ### Required packages
#+ Packages
library(tidyverse)
library(irr)
library(knitr)

#' ### Import Data
#+ Import
hieu <- read_csv("data/ceo-hieu-nguyen-practice-v2-plot-data-2019-04-18.csv")
hung <- read_csv("data/ceo-hung-le-practice-v2-plot-data-2019-04-18.csv")
jan <- read_csv("data/ceo-jan-johnson-practice-v2-plot-data-2019-04-18.csv")
mark <- read_csv("data/ceo-mark-hammond-practice-v2-plot-data-2019-04-18.csv")
ryan <- read_csv("data/ceo-ryan-thomas-practice-v2-plot-data-2019-04-18.csv")
vu <- read_csv("data/ceo-vu-quy-practice-v2-plot-data-2019-04-18.csv")

crossData <- rbind(hieu, hung, vu, jan, mark, ryan)
crossData <- na.omit(crossData)

#' ### Process data into form that can be used for irr  
#' Provide overview of the process. 

#+ Process
# Find and extract the class names. 
names(crossData)

# Set columns with classnames
classCol <- c(18:58)

#clean up
classes <- colnames(crossData[classCol]) %>% 
	gsub(":", "_", .) %>%
	gsub(" ", "_", .) %>% 
	gsub("/", "_", .)

classes

colnames(crossData)[classCol] <- classes
names(crossData)

# Process data into form that can be used for irr
# to do this, need to prepare a list of data frames with values for each class
# this is then fed to the irr functions. 
cross_tables <- rep(list(NA),length(classes))
names(cross_tables) <- classes

for (m in 1:length(cross_tables)) {
  cross_tables[[m]] <- select(crossData, "USER_ID", "PL_PLOTID", classes[m]) %>%
    spread(., "USER_ID", classes[m]) %>%
    .[,-1] %>%
  	na.omit(.) %>% 
    as.matrix(.)
}

#' ### Calculate Metrics of Agreement  
#' **Iota** is used to calculate overall agreement between raters, and represents
#' an overall look at how close they agree. A more granular approach is necessary
#' to improve agreement, but iota provides a useful summary.  
#' 
#' #### Citations  
#' 1. Conger, A.J. (1980). Integration and generalisation of Kappas for multiple
#'  raters. Psychological Bulletin, 88, 322-328.  
#' 1. Janson, H., & Olsson, U. (2001). A measure of agreement for interval or
#'  nominal multivariate observations. Educational and Psychological Measurement, 61, 277-289. 

#+ Iota, a multivariate metric of agreement
crossval_iota <- iota(cross_tables, scaledata = "q")
crossval_iota

#' For checking agreement of individual classes, we can use several approaches.  
#' The **intraclass correlation coefficient** and 
#' **mean bivariate Pearson's** are two.
#' The ICC is used to measure consistency between two raters, and uses an F-test
#' to test for significance.  
#'  
#' #### Citations  
#'  1. Bartko, J.J. (1966). The intraclass correlation coefficient as a measure of 
#'  reliability. Psychological Reports, 19, 3-11.  
#'  1. McGraw, K.O., & Wong, S.P. (1996), Forming inferences about some intraclass 
#'  correlation coefficients. Psychological Methods, 1, 30-46.  
#'  1. Shrout, P.E., & Fleiss, J.L. (1979), Intraclass correlation: uses in 
#'  assessing rater reliability. Psychological Bulletin, 86, 420-428.  

#+ Per-class agreement

# Intraclass Correlation Coefficient
cross_icc <- list()
for (m in 1:length(cross_tables)) {
  cross_icc[[m]] <- icc(cross_tables[[m]], model = "oneway", type = "agreement")
}
names(cross_icc) <- classes

# make a "table" from data values in list
icc_values <- data_frame(length(classes), 7)
for (m in 1:length(cross_icc)) {
  icc_values[m,1] <- names(cross_icc[m])
  icc_values[m,2] <- round(cross_icc[[m]]$value, 4)
  icc_values[m,3] <- round(cross_icc[[m]]$lbound, 4)
  icc_values[m,4] <- round(cross_icc[[m]]$ubound, 4)
  icc_values[m,5] <- round(cross_icc[[m]]$p.value, 4)
  icc_values[m,6] <- cross_icc[[m]]$subjects
  icc_values[m,7] <- cross_icc[[m]]$raters
}

colnames(icc_values) <- c("Class", "ICC", "Lower", "Upper", "ICC Pvalue", "n", "raters")

# # Mean Bivariate Pearson's
# cross_cor <- list()
# for (m in 1:length(cross_tables)) {
#   cross_cor[[m]] <- meancor(cross_tables[[m]])
# }
# names(cross_cor) <- classes
# 
# # make a "table" from data values in list
# cor_values <- data_frame(length(classes), 3)
# for (m in 1:length(cross_icc)) {
# 	cor_values[m,1] <- names(cross_cor[m])
# 	cor_values[m,2] <- round(cross_cor[[m]]$value, 4)
# 	cor_values[m,3] <- round(cross_cor[[m]]$p.value, 4)
# }
# colnames(cor_values) <- c("Class", "Cor", "Cor Pvalue")

# Assemble tables into one object for display. 
#kable(bind_cols(cor_values, icc_values[,2:7]))
kable(icc_values)