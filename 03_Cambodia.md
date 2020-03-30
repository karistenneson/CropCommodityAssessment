Cambodia Analysis Script
================
MS Patterson, <tertiarymatt@gmail.com>
July 26, 2019

Set working directory to where data is being stored.

``` r
#setwd("~/R/GIA/")
```

### Required packages

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

    ## -- Attaching packages ------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.1       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(rmarkdown)
source("00_GIA_functions.R")
```

``` r
# User needs to supply variables -----------------------------------------------

# Area of strata and sample sizes
country <- "Cambodia"

strata <- c(1, 2, 3)

stratumAreas <- c("Strata1 Area" = 875284, "Strata2 Area" = 922311, 
                  "Strata3 Area" = 16329763)

# Metadata to save, and Grouping Variables
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")

groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

# Survey Questions
questions <-  c("LAND_COVER", "UNDERSTORY_PRESENT", "UNDERSTORY_COVER",
                "LAND_USE", "LAND_USE_YEAR_2000", "TCC")

covOrder <- c("Aquaculture", "Banana" ,"Bamboo","Coconut","Coffee","Fruit_Nut", 
              "Oil_Palm", "Pulpwood", "Rubber", "Rice", "Tea", "Other_Crop",
              "Other_Palm", "Other_Tree", "Other_Shrub", "Herbaceous", 
              "Non_vegetated", "Built_up", "Water", "Other_LAND_COVER")

#no terrace in Cambodia, so removed here.
useOrder <- c("Agrisiviculture", "Boundary_Agrisilviculture", 
              "Mixed_Agrisilviculture", "Strip_Agrisilviculture", 
              "Silvopastoral", "Plantation", "Natural_Forest", "Other_LAND_USE")
```

``` r
# Data input and cleaning ------------------------------------------------------
rawData <- read_csv("data/Corrected/Cambodia.csv", col_types = "ddddldcdcdddcdccccc")

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
              length(rawData[which(rawData$PL_STRATUM == 3),]$PL_PLOTID)/24)

paste("There are", sampSize[1], "samples in stratum 1,", sampSize[2], 
  "samples in stratum 2, and", sampSize[3], "samples in stratum 3")
```

    ## [1] "There are 322 samples in stratum 1, 456 samples in stratum 2, and 223 samples in stratum 3"

``` r
# Analysis ---------------------------------------------------------------------
generalResults <- do_yc_analysis(table = rawData, mtdt = metaNames, strata = strata,
                                 ns = sampSize, areas = stratumAreas, qstns = questions, 
                                 grplst = groupList)
```

    ## Warning in sqrt(colSums(strataVar)): NaNs produced

``` r
# calculate top level areas
coverArea <- t(generalResults$Cover$LAND_COVER) * sum(stratumAreas)
colnames(coverArea) <- c("Area_ha", "SE_Area_ha")

useArea <- t(generalResults$Cover$LAND_USE) * sum(stratumAreas)
colnames(useArea) <- c("Area_ha", "SE_Area_ha")

use2000Area <- t(generalResults$Cover$LAND_USE_YEAR_2000) * sum(stratumAreas)
colnames(use2000Area) <- c("Area_ha", "SE_Area_ha")

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
```

| cover              | Area (ha) | SE Area (ha) |
| :----------------- | --------: | -----------: |
| Banana             |       576 |          590 |
| Bamboo             |    434761 |        46261 |
| Coconut            |       576 |          590 |
| Coffee             |     15864 |         6085 |
| Fruit\_Nut         |     41747 |         8926 |
| Oil\_Palm          |      7996 |         4250 |
| Pulpwood           |     21216 |         6513 |
| Rubber             |    790806 |        75770 |
| Rice               |    617447 |        73396 |
| Tea                |     42353 |         9221 |
| Other\_Crop        |   2851451 |       135882 |
| Other\_Palm        |      1617 |         1117 |
| Other\_Tree        |  10370452 |       368688 |
| Other\_Shrub       |    259630 |        19894 |
| Herbaceous         |   1345867 |        86274 |
| Non\_vegetated     |   1297813 |       101706 |
| Built\_up          |     67256 |         6844 |
| Water              |     12530 |         4793 |
| Other\_LAND\_COVER |    822683 |       428534 |

Table R1. The estimated area in each land cover type for Cambodia in the
period after 2015. This data was collected using question 1 in the
Collect Earth Online survey, based on Digital Globe and Bing imagery.

``` r
useAreaPretty <- as_tibble(useArea,rownames = "use")
colnames(useAreaPretty) <- c("use", "Area (ha)", "SE Area (ha)")
useAreaPretty <- arrange(useAreaPretty, match(use, useOrder))
captionQ2 <- paste("Table R2. The estimated  area in each land use type for",
                   country, "in the period after 2015. This data was collected", 
                   "using question 2 in the Collect Earth Online survey,",
                   "based on Digital Globe and Bing imagery.")
kable(useAreaPretty, digits = 0, caption = captionQ2)
```

| use                        | Area (ha) | SE Area (ha) |
| :------------------------- | --------: | -----------: |
| Agrisiviculture            |   2220758 |       128816 |
| Boundary\_Agrisilviculture |    631129 |        73604 |
| Mixed\_Agrisilviculture    |   1290366 |       100467 |
| Strip\_Agrisilviculture    |      1975 |         2023 |
| Silvopastoral              |     26995 |         7047 |
| Plantation                 |    904352 |       238400 |
| Natural\_Forest            |   9874413 |       277198 |
| Other\_LAND\_USE           |   4052654 |       384706 |

Table R2. The estimated area in each land use type for Cambodia in the
period after 2015. This data was collected using question 2 in the
Collect Earth Online survey, based on Digital Globe and Bing imagery.

``` r
captionQ3 <- paste("Table R3. The estimated  area in each land use type for", 
                   country, "in the year 2000. This data was collected", 
                   "using question 3 in the Collect Earth Online survey,", 
                   "based on Landsat time series imagery.")
kable(use2000Area, digits = 0, caption = captionQ3)
```

|                              | Area\_ha | SE\_Area\_ha |
| ---------------------------- | -------: | -----------: |
| Forest\_Commodity            |  1915419 |       257046 |
| Natural\_Forest              | 10952542 |       280071 |
| Other\_LAND\_USE\_YEAR\_2000 |  6134680 |       366719 |

Table R3. The estimated area in each land use type for Cambodia in the
year 2000. This data was collected using question 3 in the Collect Earth
Online survey, based on Landsat time series imagery.

``` r
captionQ4 <- paste("Table R4. The estimated  area of tree canopy cover in",
                   country, "for the year 2000. This data was collected using",
                   "question 3 in the Collect Earth Online survey,",
                   "based on Landsat time series imagery.")
kable(tcc2000Area, digits = 0, caption = captionQ4)
```

|           | Area (ha) | SE Area (ha) |
| --------- | --------: | -----------: |
| Not\_Tree |   6134680 |       366719 |
| Tree      |  12867962 |       366599 |

Table R4. The estimated area of tree canopy cover in Cambodia for the
year 2000. This data was collected using question 3 in the Collect Earth
Online survey, based on Landsat time series imagery.

``` r
# Object in cover analyses -----------------------------------------------------
coverIn2000 <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                               grplst = groupList, strata = strata, ns = sampSize,
                               areas = stratumAreas, cvrfld = "LAND_COVER",
                               cndtnfld = "LAND_USE_YEAR_2000")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
coverInUse <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                              grplst = groupList, strata = strata, ns = sampSize,
                              areas = stratumAreas, cvrfld = "LAND_COVER", 
                              cndtnfld = "LAND_USE")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
useinCover <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                              grplst = groupList, strata = strata, ns = sampSize,
                              areas = stratumAreas, cvrfld = "LAND_USE",
                              cndtnfld = "LAND_COVER")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
useinUse2000 <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions2, 
                                grplst = groupList, strata = strata, ns = sampSize,
                                areas = stratumAreas, cvrfld = "LAND_USE",
                                cndtnfld = "LAND_USE_YEAR_2000")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
# calculate class interaction areas 
coverIn2000Area <- calcError(coverIn2000, use2000Area, 3, 4, 1, 2)

coverInUseArea <- calcError(coverInUse, useArea, 3, 4, 1, 2)

useInCoverArea <- calcError(useinCover, coverArea, 3, 4, 1, 2)

useInUse2000Area <- calcError(useinUse2000, use2000Area, 3, 4, 1, 2)
```

``` r
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
```

| Cover              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |      Plantation |  Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | --------------: | ---------------: | :--------------- |
| Banana             |         0 ± NaN |                    0 ± NaN |               576 ± 594 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 0 ± NaN          |
| Bamboo             |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |     2628 ± 2891 |   432050 ± 49312 | 82 ± 85          |
| Coconut            |         0 ± NaN |                    0 ± NaN |               576 ± 593 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 0 ± NaN          |
| Coffee             |     8138 ± 4313 |                    0 ± NaN |             5751 ± 3923 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 1975 ± 2040      |
| Fruit\_Nut         |     4742 ± 2880 |                    0 ± NaN |            34701 ± 8966 | 0 ± NaN                 |       0 ± NaN |     2304 ± 1876 |          0 ± NaN | 0 ± NaN          |
| Oil\_Palm          |         0 ± NaN |                    0 ± NaN |             5477 ± 3431 | 0 ± NaN                 |       0 ± NaN |     2519 ± 2772 |          0 ± NaN | 0 ± NaN          |
| Pulpwood           |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |   21107 ± 10199 |          0 ± NaN | 110 ± 114        |
| Rubber             |         0 ± NaN |                    0 ± NaN |             2798 ± 1977 | 0 ± NaN                 |       0 ± NaN | 785599 ± 302508 |          0 ± NaN | 2409 ± 2403      |
| Rice               |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 617447 ± 110715  |
| Tea                |         0 ± NaN |                1728 ± 1793 |               741 ± 619 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 39884 ± 10512    |
| Other\_Crop        | 936488 ± 110676 |             360235 ± 74954 |          541118 ± 87313 | 1893 ± 3643             |       0 ± NaN |         0 ± NaN |          0 ± NaN | 1011717 ± 159726 |
| Other\_Palm        |     1426 ± 1115 |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          82 ± 84 | 110 ± 114        |
| Other\_Tree        |    48917 ± 8330 |             253921 ± 52682 |          488810 ± 72854 | 82 ± 152                |     438 ± 401 |  64846 ± 211492 | 9254304 ± 460434 | 259134 ± 168509  |
| Other\_Shrub       |     8032 ± 3261 |                9765 ± 4608 |             4797 ± 2269 | 0 ± NaN                 |  19958 ± 9788 |     2411 ± 1411 |   119270 ± 14072 | 95396 ± 18308    |
| Herbaceous         |  622207 ± 87141 |                2852 ± 2058 |          196143 ± 22525 | 0 ± NaN                 |   4190 ± 3076 |    13839 ± 6633 |     40025 ± 5254 | 466612 ± 77189   |
| Non\_vegetated     |  590808 ± 85535 |                2628 ± 2757 |             5400 ± 2528 | 0 ± NaN                 |   2409 ± 2713 |     9099 ± 5161 |     27694 ± 3399 | 659774 ± 115710  |
| Built\_up          |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 67256 ± 11333    |
| Water              |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |       987 ± 1012 | 11542 ± 4939     |
| Other\_LAND\_COVER |         0 ± NaN |                    0 ± NaN |             3479 ± 2235 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 819205 ± 372161  |

Table R5. Estimate of area and standard error of the area of each land
cover (in hectares), that occurred in each of the land uses that were
labeled for the period after 2015. All the agrisilvicultural land uses
have been summed for readability. ‘Other Crop’ contains any agricultural
or crop land covers that could not be identified during the photo
interpretation process. ‘Other Land Cover’ contains all other land
covers that did not fit into the other categories in strata 1 & 2, and
all land covers in plots in stratum 3 that did not experience forest
loss or gain.

``` r
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
```

| Cover              | Forest\_Commodity |  Natural\_Forest | Other\_LAND\_USE\_YEAR\_2000 |
| :----------------- | ----------------: | ---------------: | ---------------------------: |
| Banana             |           0 ± NaN |        576 ± 590 |                      0 ± NaN |
| Bamboo             |       2628 ± 2760 |   431886 ± 48930 |                    247 ± 254 |
| Coconut            |           0 ± NaN |          0 ± NaN |                    576 ± 592 |
| Coffee             |           0 ± NaN |     13889 ± 5764 |                  1975 ± 2030 |
| Fruit\_Nut         |         247 ± 257 |     40431 ± 9013 |                   1069 ± 836 |
| Oil\_Palm          |       2519 ± 2652 |      5477 ± 3376 |                      0 ± NaN |
| Pulpwood           |       7453 ± 4219 |     13763 ± 5212 |                      0 ± NaN |
| Rubber             |   629142 ± 150871 |   142202 ± 18269 |                 19462 ± 6274 |
| Rice               |           0 ± NaN |      7845 ± 3796 |               609602 ± 88819 |
| Tea                |       1975 ± 2057 |     11207 ± 4949 |                 29171 ± 7953 |
| Other\_Crop        |       6304 ± 3976 |  1060225 ± 67402 |             1784921 ± 194747 |
| Other\_Palm        |           0 ± NaN |          82 ± 84 |                  1535 ± 1122 |
| Other\_Tree        |   678554 ± 331082 | 8374384 ± 493377 |             1317514 ± 212533 |
| Other\_Shrub       |       1752 ± 1363 |   117633 ± 14732 |               140244 ± 18414 |
| Herbaceous         |   583994 ± 119355 |   581032 ± 46761 |               180841 ± 20813 |
| Non\_vegetated     |         850 ± 577 |     64733 ± 8674 |             1232231 ± 146462 |
| Built\_up          |           0 ± NaN |     66159 ± 7253 |                   1097 ± 589 |
| Water              |           0 ± NaN |      6633 ± 3600 |                  5897 ± 3226 |
| Other\_LAND\_COVER |           0 ± NaN |     14385 ± 4416 |              808298 ± 454353 |

Table R6. Estimate of area and standard error of the area of each land
cover (in hectares), that occurred in each of the land uses that were
labelled in the year 2000. ‘Other Crops’ contains any agricultural or
crop land covers that could not be identified during the photo
interpretation process. ‘Other Land Cover’ contains all other land
covers that did not fit into the other categories in strata 1 & 2, and
all land covers in plots in stratum 3 that did not experience forest
loss or gain.

``` r
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
```

| Condition          | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |      Plantation |  Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | --------------: | ---------------: | :--------------- |
| Bamboo             |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |     2628 ± 2747 |   432050 ± 80180 | 82 ± 85          |
| Banana             |         0 ± NaN |                    0 ± NaN |              576 ± 1030 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 0 ± NaN          |
| Built\_up          |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 67256 ± 12499    |
| Coconut            |         0 ± NaN |                    0 ± NaN |              576 ± 1055 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 0 ± NaN          |
| Coffee             |     8138 ± 6142 |                    0 ± NaN |             5751 ± 4994 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 1975 ± 2290      |
| Fruit\_Nut         |     4742 ± 3194 |                    0 ± NaN |            34701 ± 8040 | 0 ± NaN                 |       0 ± NaN |     2304 ± 1821 |          0 ± NaN | 0 ± NaN          |
| Herbaceous         |  622207 ± 90323 |                2852 ± 2019 |          196143 ± 28336 | 0 ± NaN                 |   4190 ± 2709 |    13839 ± 4350 |     40025 ± 6195 | 466612 ± 74719   |
| Non\_vegetated     |  590808 ± 96154 |                2628 ± 2734 |             5400 ± 2523 | 0 ± NaN                 |   2409 ± 2506 |     9099 ± 4003 |     27694 ± 4579 | 659774 ± 124052  |
| Oil\_Palm          |         0 ± NaN |                    0 ± NaN |             5477 ± 5358 | 0 ± NaN                 |       0 ± NaN |     2519 ± 3278 |          0 ± NaN | 0 ± NaN          |
| Other\_Crop        |  936488 ± 99498 |             360235 ± 49245 |          541118 ± 71573 | 1893 ± 1943             |       0 ± NaN |         0 ± NaN |          0 ± NaN | 1011717 ± 128448 |
| Other\_LAND\_COVER |         0 ± NaN |                    0 ± NaN |             3479 ± 3378 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 819205 ± 426731  |
| Other\_Palm        |     1426 ± 1780 |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |         82 ± 116 | 110 ± 157        |
| Other\_Shrub       |     8032 ± 3307 |                9765 ± 4436 |             4797 ± 2265 | 0 ± NaN                 |  19958 ± 6119 |     2411 ± 1142 |   119270 ± 18474 | 95396 ± 16635    |
| Other\_Tree        |    48917 ± 7651 |             253921 ± 33089 |          488810 ± 52577 | 82 ± 84                 |     438 ± 359 |  64846 ± 225664 | 9254304 ± 534749 | 259134 ± 164839  |
| Pulpwood           |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |   21107 ± 12528 |          0 ± NaN | 110 ± 123        |
| Rice               |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 617447 ± 127126  |
| Rubber             |         0 ± NaN |                    0 ± NaN |             2798 ± 1987 | 0 ± NaN                 |       0 ± NaN | 785599 ± 137348 |          0 ± NaN | 2409 ± 2403      |
| Tea                |         0 ± NaN |                1728 ± 1849 |               741 ± 637 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |          0 ± NaN | 39884 ± 15258    |
| Water              |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |       987 ± 1186 | 11542 ± 7812     |

Table R7. Estimate of area and standard error of the area of each land
cover (in hectares), that occurred in each of the land uses that were
labelled in the year 2015. ‘Other Crops’ contains any agricultural or
crop land covers that could not be identified during the photo
interpretation process. ‘Other Land Cover’ contains all other land
covers that did not fit into the other categories in strata 1 & 2, and
all land covers in plots in stratum 3 that did not experience forest
loss or gain.

``` r
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
```

| Cover                      | Forest\_Commodity |  Natural\_Forest | Other\_LAND\_USE\_YEAR\_2000 |
| :------------------------- | ----------------: | ---------------: | ---------------------------: |
| Agrisiviculture            |   610397 ± 124716 |   364543 ± 27880 |             1245819 ± 148688 |
| Boundary\_Agrisilviculture |           0 ± NaN |   622576 ± 77079 |                  8553 ± 4487 |
| Mixed\_Agrisilviculture    |       2962 ± 2325 |   490225 ± 53862 |              797178 ± 100984 |
| Strip\_Agrisilviculture    |           0 ± NaN |          0 ± NaN |                  1975 ± 2029 |
| Silvopastoral              |           0 ± NaN |     25462 ± 6939 |                  1533 ± 1590 |
| Plantation                 |   686842 ± 276520 |   186271 ± 20903 |                 31238 ± 8801 |
| Natural\_Forest            |   607435 ± 137495 | 8026965 ± 235623 |             1240014 ± 148509 |
| Other\_LAND\_USE           |       7783 ± 4404 |  1236502 ± 86961 |             2808369 ± 527271 |

Table R8. Estimate area and standard error (in hectares) of each land
use from the period after 2015 occuring in each land use type in 2000.
‘Other Land Use’ contains all other land covers that did not fit into
the other categories

``` r
# Former Forest only -----------------------------------------------------------
```

Triple conditional for area of commodities that were forested in the
year 2000.

``` r
questions2 <- c("LAND_COVER", "LAND_USE", "LAND_USE_YEAR_2000")

crops <- c("Banana", "Coconut", "Coffee", "Fruit_Nut" ,"Pulpwood", "Rice", 
           "Rubber", "Oil_Palm", "Tea", "Other_Crop", "Other_Tree", "Other_Palm",
           "Other_Shrub", "Herbaceous", "Other_LAND_COVER")

y_occ <- build_yocc(rawData, mtdt = metaNames, qstns = questions2, 
                    cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                    cndtnfld2 = "LAND_USE_YEAR_2000", covers = crops, 
                    conditions1 = NULL, conditions2 = "Natural_Forest")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")
    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
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
```

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |     Plantation |  Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | -------------: | ---------------: | :--------------- |
| Banana             |           0 ± 0 |                      0 ± 0 |               576 ± 590 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coconut            |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coffee             |     8138 ± 4260 |                      0 ± 0 |             5751 ± 3871 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 0 ± 0            |
| Fruit\_Nut         |     4495 ± 2842 |                      0 ± 0 |            33632 ± 8299 | 0 ± 0                   |         0 ± 0 |    2304 ± 1667 |            0 ± 0 | 0 ± 0            |
| Oil\_Palm          |           0 ± 0 |                      0 ± 0 |             5477 ± 3371 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 0 ± 0            |
| Pulpwood           |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |   13763 ± 5186 |            0 ± 0 | 0 ± 0            |
| Rubber             |           0 ± 0 |                      0 ± 0 |               329 ± 337 | 0 ± 0                   |         0 ± 0 | 141763 ± 17479 |            0 ± 0 | 110 ± 113        |
| Rice               |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 7845 ± 3785      |
| Tea                |           0 ± 0 |                      0 ± 0 |               576 ± 590 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 10631 ± 4897     |
| Other\_Crop        |  315425 ± 23195 |             355931 ± 42766 |            18753 ± 5646 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 370117 ± 40777   |
| Other\_Palm        |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |          82 ± 84 | 0 ± 0            |
| Other\_Tree        |    20372 ± 6382 |             252743 ± 30515 |          309913 ± 36751 | 0 ± 0                   |     329 ± 340 |   19805 ± 6697 | 7540203 ± 239005 | 231020 ± 27601   |
| Other\_Shrub       |     2304 ± 1329 |                9463 ± 4302 |             2852 ± 1824 | 0 ± 0                   |  19520 ± 5704 |      630 ± 415 |     12139 ± 3805 | 70725 ± 11762    |
| Herbaceous         |     4141 ± 2412 |                1810 ± 1854 |          106451 ± 12530 | 0 ± 0                   |   3204 ± 2485 |    5593 ± 2553 |     39257 ± 4995 | 420575 ± 44227   |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |             3479 ± 2199 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 10907 ± 3813     |

Table R9. Estimate of area and standard error of each commodity land
cover (in hectares), by land use, in areas that were forested in the
year 2000 and experienced forest cover loss between 2001 and 2015.
‘Other Crops’ contains any agricultural or crop land covers that could
not be identified during the photo interpretation process. ‘Other Land
Use’ contains all other land covers that did not fit into the other
categories.

``` r
# Support FF in 2000 ----------------------------------------------------------
```

Triple conditional for “support area” of commodities that were had
forest cover in the year 2000.

``` r
support <- c("Built_up", "Non_vegetated", "Other_LAND_COVER", "Water")

y_occ2 <- build_yocc(rawData, mtdt = metaNames, qstns = questions, 
                     cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                     cndtnfld2 = "TCC", covers = support, 
                     conditions1 = NULL, conditions2 = "Tree")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")
    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
y_occResults2 <- do_yocc_analysis(y_occ2, mtdt = metaNames, strata = strata, 
                                  areas = stratumAreas, ns = sampSize, 
                                  qstns = questions2, grplst = groupList)

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
      caption = c("Table R11. Estimate of area and standard error of each 
      commodity \'support\' land cover (in hectares), by land use, in areas that
      had tree cover in the year 2000 and experienced tree canopy loss between 
      2001  and 2015. ‘Other Crops’ contains any agricultural or crop land
      covers that could not be identified during the photo interpretation 
      process.  ‘Other Land Use’ contains all other land covers that did not fit
      into the other categories."))
```

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |  Plantation | Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | ----------: | --------------: | :--------------- |
| Non\_vegetated     |     9667 ± 4192 |                2628 ± 2718 |             2437 ± 1651 | 0 ± 0                   |   2409 ± 2492 | 2933 ± 1700 |     2494 ± 1494 | 43014 ± 5976     |
| Built\_up          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |       0 ± 0 |           0 ± 0 | 66159 ± 6823     |
| Water              |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |       0 ± 0 |      987 ± 1011 | 5645 ± 3448      |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |             3479 ± 2199 | 0 ± 0                   |         0 ± 0 |       0 ± 0 |           0 ± 0 | 10907 ± 3813     |

Table R11. Estimate of area and standard error of each commodity
‘support’ land cover (in hectares), by land use, in areas that had
tree cover in the year 2000 and experienced tree canopy loss between
2001 and 2015. ‘Other Crops’ contains any agricultural or crop land
covers that could not be identified during the photo interpretation
process. ‘Other Land Use’ contains all other land covers that did not
fit into the other categories.

``` r
# Crops in TCC in 2000 ---------------------------------------------------------
```

Triple conditional for area of commodities that were had tree cover in
the year 2000.

``` r
questions3 <- c("LAND_COVER", "LAND_USE", "TCC")

crops <- c("Banana", "Coconut", "Coffee", "Fruit_Nut" ,"Pulpwood", "Rice", 
           "Rubber", "Oil_Palm", "Tea", "Other_Crop", "Other_Tree", "Other_Palm",
           "Other_Shrub", "Herbaceous")

y_occ3 <- build_yocc(rawData, mtdt = metaNames, qstns = questions3, 
                     cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                     cndtnfld2 = "TCC", covers = crops, 
                     conditions1 = NULL, conditions2 = "Tree")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")
    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
y_occResults3 <- do_yocc_analysis(y_occ3, mtdt = metaNames, strata = strata, 
                                  areas = stratumAreas, ns = sampSize, 
                                  qstns = questions3, grplst = groupList)


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
      caption = c("Table R10. Estimate of area and standard error of each 
      commodity land cover (in hectares), by land use, in areas that had tree 
      cover in the year 2000 and experienced tree canopy cover loss between 
      2001  and 2015. ‘Other Crops’ contains any agricultural or crop land
      covers that could not be identified during the photo interpretation 
      process.  ‘Other Land Use’ contains all other land covers that did not fit
      into the other categories."))
```

| Crops        | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |     Plantation |  Natural\_Forest | Other\_LAND\_USE |
| :----------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | -------------: | ---------------: | :--------------- |
| Banana       |           0 ± 0 |                      0 ± 0 |               576 ± 590 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coconut      |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coffee       |     8138 ± 4260 |                      0 ± 0 |             5751 ± 3871 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 0 ± 0            |
| Fruit\_Nut   |     4742 ± 2853 |                      0 ± 0 |            33632 ± 8299 | 0 ± 0                   |         0 ± 0 |    2304 ± 1667 |            0 ± 0 | 0 ± 0            |
| Oil\_Palm    |           0 ± 0 |                      0 ± 0 |             5477 ± 3371 | 0 ± 0                   |         0 ± 0 |    2519 ± 2605 |            0 ± 0 | 0 ± 0            |
| Pulpwood     |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |   21107 ± 6487 |            0 ± 0 | 110 ± 113        |
| Rubber       |           0 ± 0 |                      0 ± 0 |               987 ± 753 | 0 ± 0                   |         0 ± 0 | 767947 ± 75551 |            0 ± 0 | 2409 ± 2381      |
| Rice         |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 7845 ± 3785      |
| Tea          |           0 ± 0 |                      0 ± 0 |               576 ± 590 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 12606 ± 5294     |
| Other\_Crop  |  317944 ± 23300 |             355931 ± 42766 |            20563 ± 5933 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |            0 ± 0 | 372092 ± 40824   |
| Other\_Palm  |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |          82 ± 84 | 0 ± 0            |
| Other\_Tree  |    46670 ± 7141 |             252743 ± 30515 |          310077 ± 36751 | 0 ± 0                   |     329 ± 340 | 64462 ± 226922 | 8147637 ± 248846 | 231020 ± 27601   |
| Other\_Shrub |     2304 ± 1329 |                9463 ± 4302 |             2852 ± 1824 | 0 ± 0                   |  19520 ± 5704 |     1288 ± 795 |     12139 ± 3805 | 71820 ± 11804    |
| Herbaceous   |  585474 ± 70240 |                1810 ± 1854 |          106780 ± 12534 | 0 ± 0                   |   3204 ± 2485 |    7925 ± 3082 |     39257 ± 4995 | 420575 ± 44227   |

Table R10. Estimate of area and standard error of each commodity land
cover (in hectares), by land use, in areas that had tree cover in the
year 2000 and experienced tree canopy cover loss between 2001 and 2015.
‘Other Crops’ contains any agricultural or crop land covers that could
not be identified during the photo interpretation process. ‘Other Land
Use’ contains all other land covers that did not fit into the other
categories.

``` r
# Support TCC in 2000 ----------------------------------------------------------
```

Triple conditional for “support area” of commodities that were had tree
cover in the year 2000.

``` r
support <- c("Built_up", "Non_vegetated", "Other_LAND_COVER", "Water")

y_occ4 <- build_yocc(rawData, mtdt = metaNames, qstns = questions3, 
                     cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                     cndtnfld2 = "TCC", covers = support, 
                     conditions1 = NULL, conditions2 = "Tree")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")
    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
y_occResults4 <- do_yocc_analysis(y_occ4, mtdt = metaNames, strata = strata, 
                                  areas = stratumAreas, ns = sampSize, 
                                  qstns = questions3, grplst = groupList)

# calculate areas
cResults4 <- bind_rows(y_occResults4$Cover)

cTableP4 <- cResults4 %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas), 
         SE = SE * sum(stratumAreas)) %>% 
  mutate(Pretty = paste(round(PercentCover), round(SE), sep = " \u00B1 ")) %>% 
  select(cover, condition1, Pretty) %>% 
  spread(condition1, Pretty) %>% 
  arrange(match(cover, covOrder)) %>% 
  select(cover, useOrder)

kable(cTableP4, digits = 0, col.names = c("Crops", useOrder), align = "lrrr",
      caption = c("Table R11. Estimate of area and standard error of each 
      commodity \'support\' land cover (in hectares), by land use, in areas that
      had tree cover in the year 2000 and experienced tree canopy loss between 
      2001  and 2015. ‘Other Crops’ contains any agricultural or crop land
      covers that could not be identified during the photo interpretation 
      process.  ‘Other Land Use’ contains all other land covers that did not fit
      into the other categories."))
```

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |  Plantation | Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | ----------: | --------------: | :--------------- |
| Non\_vegetated     |     9667 ± 4192 |                2628 ± 2718 |             2437 ± 1651 | 0 ± 0                   |   2409 ± 2492 | 2933 ± 1700 |     2494 ± 1494 | 43014 ± 5976     |
| Built\_up          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |       0 ± 0 |           0 ± 0 | 66159 ± 6823     |
| Water              |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |       0 ± 0 |      987 ± 1011 | 5645 ± 3448      |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |             3479 ± 2199 | 0 ± 0                   |         0 ± 0 |       0 ± 0 |           0 ± 0 | 10907 ± 3813     |

Table R11. Estimate of area and standard error of each commodity
‘support’ land cover (in hectares), by land use, in areas that had
tree cover in the year 2000 and experienced tree canopy loss between
2001 and 2015. ‘Other Crops’ contains any agricultural or crop land
covers that could not be identified during the photo interpretation
process. ‘Other Land Use’ contains all other land covers that did not
fit into the other categories.

## Understory analysis

Investigate presence of understory \# Understory ——————————————————————-

``` r
sum(rawData$UNDERSTORY_PRESENT == "Yes")
```

    ## [1] 8

``` r
unique(rawData$UNDERSTORY_COVER)
```

    ## [1] NA           "Other_Crop"

Eight points in other crop exist as understory. Will exclude, as it’s
already included in the agroforestry landuse by default.

## Carbon Analysis

``` r
# calculate Carbon values for areas --------------------------------------------
carbonMono <- c("Banana" = 5.7, #from Philippines
                "Coffee" = 5.4, # From Vietnam
                "Fruit_Nut" = 75, # cashew
                "Oil_Palm" = 41.63, # Regional average + Indonesia
                "Pulpwood" = 23, # from Vietnam
                "Rubber" = 31.83, # mean of time average from Vietnam & Thailand
                "Rice" = 1.05, # From Thailand
                "Tea" = 15.53, # from Vietnam
                "Other_Crop" = 6.82, # mean from several sources
                "Other_Tree" = 43.28, # mean of trees
                "Other_Shrub" = 10.46, # mean of shrubs
                "Herbaceous" = 6.82) # same as "other crops"

carbonAF <- c("Banana" = 5.7, #from Philippines
              "Coffee" = 11, # From Vietnam
              "Fruit_Nut" = 75, # Cashew
              "Oil_Palm" = 41.63, # Regional average + Indonesia
              "Pulpwood" = 23, # from Vietnam
              "Rubber" = 31.83, # mean of time averages from Vietnam & Thailand
              "Rice" = 1.05, # From Thailand
              "Tea" = 22, # from Vietnam
              "Other_Crop" = 20, # from Vietnam
              "Other_Tree" = 43.28, #mean of trees
              "Other_Shrub" = 16.5, #mean of shrubs
              "Herbaceous" = 20) # same as "other crops"

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
```

|              | Monoculture | Agroforestry |
| ------------ | ----------: | -----------: |
| Banana       |         5.7 |          5.7 |
| Coffee       |         5.4 |         11.0 |
| Fruit\_Nut   |        75.0 |         75.0 |
| Oil\_Palm    |        41.6 |         41.6 |
| Pulpwood     |        23.0 |         23.0 |
| Rubber       |        31.8 |         31.8 |
| Rice         |         1.1 |          1.1 |
| Tea          |        15.5 |         22.0 |
| Other\_Crop  |         6.8 |         20.0 |
| Other\_Tree  |        43.3 |         43.3 |
| Other\_Shrub |        10.5 |         16.5 |
| Herbaceous   |         6.8 |         20.0 |

Table R12. Carbon factors used to calculate the aboveground biomass for
each commodity. Values for commodities were compiled for monoculture and
agroforestry systems from peer-reviewed and grey literature.
Time-averaged values are often used to estimate the carbon storage of
rotational commodity crops because they allow for the “averaging” of a
mixture of freshly replanted and mature commodity areas across the
landscape. Sources for the carbon values presented here are included in
Appendix 1.

``` r
# calculate Carbon values for previously forested areas ------------------------
# make results into an area
ffAreas <- arrange(cResults, cover) %>%
  mutate(PercentCover = PercentCover * sum(stratumAreas),
         SE = SE * sum(stratumAreas))
colnames(ffAreas) <-c("cover", "condition1", "condition2", "area", "SE") 

### agroforestry carbon
ffA <- filter(ffAreas, condition1 == "Agrisiviculture") #keep all elements
ffBA <- filter(ffAreas, condition1 == "Boundary_Agrisilviculture") #keep all elements
ffMA <- filter(ffAreas, condition1 == "Mixed_Agrisilviculture") #keep all elements
ffSA <- filter(ffAreas, condition1 == "Strip_Agrisilviculture") #keep all elements

ffSP <- filter(ffAreas, condition1 == "Silvopastoral")
  # zero out herbaceous, other palm, other shrub, other tree
ffSP[,c(4,5)] <- ffSP[,c(4,5)] * c(1,1,1,1,0,1,1,1,0,0,0,1,1,1,1) 

# calculate group total area
ffAA <- ffA[,4] + ffBA[,4] + ffMA[,4] + ffSA[,4] + ffSP[,4]

# calculate group area SE
ffAASE <- bind_cols(A = pull(ffA[,5])^2, 
                    BA = pull(ffBA[,5])^2,
                    MA = pull(ffMA[,5])^2, 
                    SA = pull(ffSA[,5])^2, 
                    SP = pull(ffSP[,5])^2) %>%
  rowwise() %>%
  transmute(SE_ha = sum(A, BA, MA, SA, SP, na.rm = T)) %>%
  sqrt()

# assemble names, areas, SE
ffAF <- bind_cols(commodity = ffA$cover, Area_ha = pull(ffAA), SE_ha = ffAASE)

# calculate carbon stock
ffAF <- ffAF[ffAF$commodity %in% names(carbonAF),] %>%
  arrange(., match(commodity, names(carbonAF))) %>% 
  transmute(., commodity = commodity, 
               Mg_C = Area_ha * carbonAF,
               SE = SE_ha * carbonAF)

# Make a table
captionffAF <- c("Table X. Aboveground biomass carbon values associated with
               the area of commodities in agroforestry land uses that occur in
                 formerly forested areas.")

kable(ffAF, caption = captionffAF, digits = 0,
      col.names = c("Commodity", "MgC", "SE"))
```

| Commodity    |      MgC |      SE |
| :----------- | -------: | ------: |
| Banana       |     3283 |    3363 |
| Coffee       |   152779 |   63323 |
| Fruit\_Nut   |  2859544 |  657874 |
| Oil\_Palm    |   227991 |  140326 |
| Pulpwood     |        0 |       0 |
| Rubber       |    10477 |   10730 |
| Rice         |        0 |       0 |
| Tea          |    12673 |   12978 |
| Other\_Crop  | 13802167 |  979553 |
| Other\_Tree  | 25233459 | 2085768 |
| Other\_Shrub |   241218 |   80157 |
| Herbaceous   |  2248043 |  257887 |

Table X. Aboveground biomass carbon values associated with the area of
commodities in agroforestry land uses that occur in formerly forested
areas.

``` r
### Calculate carbon in associated with non-AF areas
ffO <- filter(ffAreas, condition1== "Other_LAND_USE")
  # zero out herbaceous, other palm, other shrub, other tree
ffO[,c(4,5)] <- ffO[,c(4,5)] * c(1,1,1,1,0,1,1,1,0,0,0,1,1,1,1)

ffP <- filter(ffAreas, condition1 == "Plantation") #keep all elements
#ffT <- filter(ffAreas, condition1 == "Terrace") #keep all elements

# Calculate group total area
ffOPT <- ffO[,4] + ffP[,4] #+ ffT[,4]

# Calculate group area SE
ffOPTSE <- bind_cols(O = pull(ffO[,5])^2, 
                     P = pull(ffP[,5])^2
                     #Tr = pull(ffT[,5])^2
                     ) %>%
  rowwise() %>%
  transmute(SE_ha = sum(O, P, na.rm = T)) %>%
  sqrt()

# Assemble names, areas, SE
ffMono <- bind_cols(commodity = ffO$cover, Area_ha = pull(ffOPT), SE_ha = ffOPTSE)

# Calculate carbon stocks
ffMono <-  ffMono[ffMono$commodity %in% names(carbonMono),] %>%
  arrange(., match(commodity, names(carbonMono))) %>% 
  transmute(., commodity = commodity,
               Mg_C = Area_ha * carbonMono, 
               SE = SE_ha * carbonMono)

# Make a table
captionffMono <- c("Table X. Aboveground biomass carbon values associated with
                 the area of commodities in monoculture plantation and terrace land
                 that occur in formerly forested areas.")

kable(ffMono, caption = captionffMono, digits = 0,
      col.names = c("Commodity", "MgC", "SE"))
```

| Commodity    |     MgC |     SE |
| :----------- | ------: | -----: |
| Banana       |       0 |      0 |
| Coffee       |       0 |      0 |
| Fruit\_Nut   |  172810 | 125005 |
| Oil\_Palm    |       0 |      0 |
| Pulpwood     |  316552 | 119287 |
| Rubber       | 4515806 | 556356 |
| Rice         |    8237 |   3974 |
| Tea          |  165104 |  76046 |
| Other\_Crop  | 2524196 | 278101 |
| Other\_Tree  |  857157 | 289853 |
| Other\_Shrub |    6595 |   4337 |
| Herbaceous   |   38147 |  17411 |

Table X. Aboveground biomass carbon values associated with the area of
commodities in monoculture plantation and terrace land that occur in
formerly forested areas.

``` r
# calculate Carbon values for previously tree-covered areas --------------------

#UPDATE BELOW HERE IF TCC NEEDED

# make results into an area
# tccAreas <- arrange(cResults2, match(cover, covOrder)) %>% 
#   mutate(PercentCover = PercentCover * sum(stratumAreas),
#          SE = SE * sum(stratumAreas))
# 
# ## Agroforestry Carbon
# tccA <- tccAreas[seq(1,112,8),][-c(1,9),] #keep all elements
# 
# tccBA <- tccAreas[seq(2,112,8),][-c(1,9),] #keep all elements
# 
# tccMA <- tccAreas[seq(3,112,8),][-c(1,9),] #keep all elements
# 
# tccSP <- tccAreas[seq(7,112,8),][-c(1,9),]
# #remove herbaceous, other palm, other shrub, other tree
# tccSP[,c(4,5)] <- tccSP[,c(4,5)] * c(1,1,1,1,1,1,1,1,0,0,0,0) 
# 
# tccAA <- tccA[,4] + tccBA[,4] + tccMA[,4] + tccSP[,4]
# 
# tccAASE <- bind_cols(A = pull(tccA[,5])^2, BA = pull(tccBA[,5])^2, 
#                      MA = pull(tccMA[,5])^2, SP = pull(tccSP[,5])^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>% 
#   sqrt()
# 
# tccAF <- bind_cols(commodity = pull(tccAreas[seq(1,112,8),1][-c(1,9),]), 
#                    Area_ha = pull(tccAA), 
#                    SE_ha = tccAASE)
# 
# tccAF <- transmute(tccAF,  commodity = commodity, Mg_C = Area_ha * carbonAF, 
#                    SE = SE_ha * carbonAF)
# 
# captiontccAF <- c("Table R13. Aboveground biomass carbon values (1,000s Mg C) 
#  associated with the area of commodities in agroforestry land uses that occur in
#  formerly tree covered areas.")
# 
# kable(tccAF, caption = captiontccAF, digits = 0,
#       col.names = c("Commodity", "MgC", "SE"))
# 
# ## Calculate carbon in associated with non-AF areas
# tccO <- tccAreas[seq(5,112,8),][-c(1,9),]
# #remove herbaceous, other palm, other shrub, other tree
# tccO[,c(4,5)] <- tccO[,c(4,5)] * c(1,1,1,1,1,1,1,1,0,0,0,0)
# 
# tccP <- tccAreas[seq(6,112,8),][-c(1,9),] #keep all elements
# 
# tccT <- tccAreas[seq(8,112,8),][-c(1,9),] #keep all elements
# 
# tccOPT <- tccO[,4] + tccP[,4] + tccT[,4]
# 
# tccOPTSE <- bind_cols(O = pull(tccO[,5])^2, P = pull(tccP[,5])^2, 
#                       Tr = pull(tccT[,5])^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(O, P, Tr, na.rm = T)) %>% 
#   sqrt()
# 
# tccMono <- bind_cols(commodity = pull(tccAreas[seq(1,112,8),1][-c(1,9),]), 
#                      Area_ha = pull(tccOPT), 
#                      SE_ha = tccOPTSE)
# 
# tccMono <- transmute(tccMono, commodity = commodity, 
#                      Mg_C = Area_ha * carbonMono, SE = SE_ha * carbonMono)
# 
# 
# captiontccMono <- c("Table R14. Aboveground biomass carbon values (1,000s Mg C) 
#   associated with the area of commodities in monoculture plantation and terrace 
#   land that occur in formerly tree-covered areas.")
# 
# kable(tccMono, caption = captiontccMono, digits = 0, 
#       col.names = c("Commodity", "MgC", "SE"))
```

``` r
## Raw Tables 
# kable(coverIn2000Area)
# kable(coverInUseArea)
# kable(useInCoverArea)
# kable(useInUse2000Area)
```
