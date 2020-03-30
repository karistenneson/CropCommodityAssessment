#' ---
#' title: "GIA Samples"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

library(raster)
setwd("~/R/GIA")

philippines <- raster("Forest_Loss_Philippines.tif")
NAvalue(philippines) <- 0

sampPhil <- sampleStratified(philippines, 1000, xy=TRUE, sp=TRUE)
