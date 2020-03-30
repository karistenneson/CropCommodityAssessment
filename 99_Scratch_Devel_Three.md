Scratch File for Development
================
MS Patterson, <tertiarymatt@gmail.com>
June 17, 2019

Set working directory to where data is being stored.

``` r
#setwd("~/R/GIA/")
```

## TO DO

0.  [x] Assemble list of estimator equations to code.  
1.  [x] Determine additional data inputs needed.  
2.  [x] Write presence function.  
3.  [x] Write functions for Question/Analysis Tables.  
4.  [x] Write estimator functions.
5.  [x] Write estimator functions for “object in class”.  
6.  [x] Construct wrappers for main workflow.
7.  [x] Construct wrapper for “object in class” workflow.
8.  [x] Figure out y\_occ workflow.
9.  [x] Area and error functions.  
10. [x] Create country analysis script template.
11. [x] Create pretty output formatting.  
12. [x] Calculate carbon numbers.
13. [x] Document functions more thoroughly.
14. [x] Work out adding Understory analysis.  
15. [ ] Update Vietnam with new table formats.  
16. [ ] Convert country analysis script to proper Rmd.

## Getting Set Up

``` r
library(tidyverse)
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

    ## Registered S3 method overwritten by 'rvest':
    ##   method            from
    ##   read_xml.response xml2

    ## -- Attaching packages ------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.1       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
source("00_GIA_functions.R")
```

``` r
# User needs to supply variables -----------------------------------------------

# Area of strata and sample sizes
strata <- c(1, 2, 3)
stratumAreas <- c("Strata1 Area" = 8324441, "Strata2 Area" =    12512668, 
                  "Strata3 Area" = 169619791)

# Metadata to save, and Grouping Variables
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")
groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

# Survey Questions
questions <-  c("LAND_COVER", "UNDERSTORY_PRESENT", "UNDERSTORY_COVER",
                "LAND_USE", "LAND_USE_YEAR_2000")
```

``` r
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
```

General results object to use for development.

``` r
generalResults <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                                 ns = sampSize, areas = stratumAreas, qstns = questions, 
                                 grplst = groupList)
```

    ## Warning in sqrt(colSums(strataVar)): NaNs produced

## Understory analysis development

The understory areas are small, and contain many NAs. This borks things
up, and needs to be addressed so that they can be incorporated.

``` r
understoryArea <- t(generalResults$Cover$UNDERSTORY_PRESENT) * sum(stratumAreas)
colnames(understoryArea) <- c("Area_ha", "SE_Area_ha")


coverInUnder <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions, 
                               grplst = groupList, strata = strata, ns = sampSize,
                               areas = stratumAreas, cvrfld = "UNDERSTORY_COVER", 
                               cndtnfld = "UNDERSTORY_PRESENT")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

    ## Warning in sqrt(colSums(strataVar)): NaNs produced

``` r
coverIn2000Area <- calcError(coverInUnder, understoryArea, 3, 4, 1, 2)

#understory associations
underCrops <- c("Coffee", "Other_Crops")
treeCrops <- c("Coconut", "Fruit_Nut" ,"Pulpwood", "Rubber", "Oil_Palm", 
           "Other_Tree", "Other_Palm")

uy_occ <- build_yocc(rawData, mtdt = metaNames, qstns = questions, 
                    cvrfld = "UNDERSTORY_COVER", cndtnfld1 = "LAND_COVER", 
                    cndtnfld2 = "UNDERSTORY_PRESENT", covers = underCrops, 
                    conditions1 = treeCrops, conditions2 = "Yes")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")
    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
uy_occResults <- do_yocc_analysis(uy_occ, mtdt = metaNames, strata = strata, 
                                 areas = stratumAreas, ns = sampSize, 
                                 qstns = questions, grplst = groupList)
```
