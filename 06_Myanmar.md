Myanmar Analysis Script
================
MS Patterson, <tertiarymatt@gmail.com>
July 25, 2019

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

    ## -- Attaching packages ------------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.1.1       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts --------------------------------------------------------------------------------- tidyverse_conflicts() --
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
country <- "Myanmar"

strata <- c(1, 2, 3)

stratumAreas <- c("Strata1 Area" = 599379, "Strata2 Area" = 1863779, 
                  "Strata3 Area" = 64255959)

# Metadata to save, and Grouping Variables
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")

groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

# Survey Questions
questions <-  c("LAND_COVER", "LAND_USE", "LAND_USE_YEAR_2000", "TCC")

covOrder <- c("Aquaculture", "Banana" ,"Bamboo","Coconut","Coffee","Fruit_Nut", 
              "Oil_Palm", "Pulpwood", "Rubber", "Rice", "Tea", "Other_Crop",
              "Other_Palm", "Other_Tree", "Other_Shrub", "Herbaceous", 
              "Non_vegetated", "Built_up", "Water", "Other_LAND_COVER")

#no terrace in Cambodia, so removed here.
useOrder <- c("Agrisiviculture", "Boundary_Agrisilviculture", 
              "Mixed_Agrisilviculture", "Silvopastoral", "Plantation", "Terrace", 
              "Natural_Forest", "Other_LAND_USE")
```

``` r
# Data input and cleaning ------------------------------------------------------
rawData <- read_csv("data/Corrected/Myanmar.csv", col_types = "ddddldcdcdddcdccccc")

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

    ## [1] "There are 177 samples in stratum 1, 564 samples in stratum 2, and 250 samples in stratum 3"

``` r
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
| Bamboo             |     27765 |         8122 |
| Coffee             |       828 |          847 |
| Fruit\_Nut         |      5722 |         4196 |
| Oil\_Palm          |     17104 |         7227 |
| Pulpwood           |   1540160 |       522233 |
| Rubber             |     93013 |        16636 |
| Rice               |     10444 |         5250 |
| Tea                |     25302 |        21683 |
| Other\_Crop        |    423570 |        32474 |
| Other\_Palm        |      2678 |         2493 |
| Other\_Tree        |  10162024 |      1437449 |
| Other\_Shrub       |    305396 |        48914 |
| Herbaceous         |    436258 |       120673 |
| Non\_vegetated     |    114527 |        16241 |
| Built\_up          |     50943 |        32568 |
| Water              |     22440 |         7231 |
| Other\_LAND\_COVER |  53480945 |      1522421 |

Table R1. The estimated area in each land cover type for Myanmar in the
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
| Agrisiviculture            |    145382 |        20664 |
| Boundary\_Agrisilviculture |     12323 |         5749 |
| Mixed\_Agrisilviculture    |     82359 |        16189 |
| Silvopastoral              |     36724 |        10761 |
| Plantation                 |   8562666 |      1325712 |
| Terrace                    |     36415 |        22461 |
| Natural\_Forest            |   1881920 |       562175 |
| Other\_LAND\_USE           |  55961329 |      1414834 |

Table R2. The estimated area in each land use type for Myanmar in the
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
| Forest\_Commodity            |  8398424 |      1342520 |
| Natural\_Forest              |  3045692 |       562294 |
| Other\_LAND\_USE\_YEAR\_2000 | 55275001 |      1426770 |

Table R3. The estimated area in each land use type for Myanmar in the
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
| Not\_Tree |  55275001 |      1426770 |
| Tree      |  11444116 |      1426707 |

Table R4. The estimated area of tree canopy cover in Myanmar for the
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

| Cover              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |        Plantation |       Terrace |  Natural\_Forest | Other\_LAND\_USE   |
| :----------------- | --------------: | -------------------------: | ----------------------: | :------------ | ----------------: | ------------: | ---------------: | :----------------- |
| Bamboo             |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |       9243 ± 5378 |       0 ± NaN |    18108 ± 10007 | 414 ± 424          |
| Coffee             |         0 ± NaN |                    0 ± NaN |               828 ± 878 | 0 ± NaN       |           0 ± NaN |       0 ± NaN |          0 ± NaN | 0 ± NaN            |
| Fruit\_Nut         |         0 ± NaN |                    0 ± NaN |             5722 ± 4490 | 0 ± NaN       |           0 ± NaN |       0 ± NaN |          0 ± NaN | 0 ± NaN            |
| Oil\_Palm          |     3311 ± 3455 |                    0 ± NaN |             3213 ± 3300 | 0 ± NaN       |      10579 ± 5967 |       0 ± NaN |          0 ± NaN | 0 ± NaN            |
| Pulpwood           |         0 ± NaN |                    0 ± NaN |             2208 ± 2359 | 0 ± NaN       |  1534739 ± 546677 |   3213 ± 4337 |          0 ± NaN | 0 ± NaN            |
| Rubber             |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |     93013 ± 26299 |       0 ± NaN |          0 ± NaN | 0 ± NaN            |
| Rice               |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |           0 ± NaN |       0 ± NaN |          0 ± NaN | 10444 ± 5263       |
| Tea                |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |           0 ± NaN | 25302 ± 17437 |          0 ± NaN | 0 ± NaN            |
| Other\_Crop        |  117230 ± 32288 |                8647 ± 8342 |           33357 ± 14506 | 0 ± NaN       |           0 ± NaN |   7632 ± 8312 |          0 ± NaN | 256704 ± 27741     |
| Other\_Palm        |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |         268 ± 282 |       0 ± NaN |      2410 ± 2681 | 0 ± NaN            |
| Other\_Tree        |    16575 ± 6837 |                3405 ± 2957 |             7316 ± 4039 | 0 ± NaN       | 6824314 ± 1167650 |       0 ± NaN | 1755758 ± 958487 | 1554657 ± 627852   |
| Other\_Shrub       |       803 ± 537 |                  134 ± 173 |             1518 ± 1632 | 25449 ± 14523 |      16539 ± 7133 |     268 ± 367 |    70733 ± 59058 | 189953 ± 22254     |
| Herbaceous         |     5588 ± 4268 |                    0 ± NaN |            26590 ± 8778 | 11276 ± 6310  |     58617 ± 17757 |       0 ± NaN |    32460 ± 15942 | 301728 ± 120136    |
| Non\_vegetated     |     1874 ± 1965 |                  138 ± 170 |             1607 ± 1170 | 0 ± NaN       |      12140 ± 5993 |       0 ± NaN |      2451 ± 1854 | 96316 ± 15639      |
| Built\_up          |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |           0 ± NaN |       0 ± NaN |          0 ± NaN | 50943 ± 32619      |
| Water              |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |           0 ± NaN |       0 ± NaN |          0 ± NaN | 22440 ± 7276       |
| Other\_LAND\_COVER |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |       3213 ± 3379 |       0 ± NaN |          0 ± NaN | 53477731 ± 1496117 |

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
| Bamboo             |       6443 ± 4030 |     20920 ± 9064 |                    402 ± 413 |
| Coffee             |           0 ± NaN |        828 ± 874 |                      0 ± NaN |
| Fruit\_Nut         |           0 ± NaN |      5722 ± 4456 |                      0 ± NaN |
| Oil\_Palm          |           0 ± NaN |     16435 ± 8369 |                    669 ± 689 |
| Pulpwood           |  1411418 ± 677200 |   108427 ± 33696 |                 20316 ± 7896 |
| Rubber             |     27546 ± 11311 |    56496 ± 19609 |                  8971 ± 5356 |
| Rice               |           0 ± NaN |     10444 ± 5918 |                      0 ± NaN |
| Tea                |           0 ± NaN |      3213 ± 3410 |                22088 ± 21524 |
| Other\_Crop        |      11856 ± 6680 |   344599 ± 94465 |                67115 ± 14286 |
| Other\_Palm        |         268 ± 282 |      2410 ± 2556 |                      0 ± NaN |
| Other\_Tree        | 6711968 ± 1975328 | 1879211 ± 929410 |             1570846 ± 623292 |
| Other\_Shrub       |      23238 ± 9146 |   259691 ± 70960 |                 22467 ± 6925 |
| Herbaceous         |   171791 ± 120160 |   197673 ± 55860 |                66794 ± 13366 |
| Non\_vegetated     |       1234 ± 1062 |    91059 ± 27907 |                 22234 ± 7722 |
| Built\_up          |     32664 ± 32102 |     17606 ± 7018 |                    674 ± 458 |
| Water              |           0 ± NaN |     14185 ± 6903 |                  8254 ± 4347 |
| Other\_LAND\_COVER |           0 ± NaN |     16773 ± 7754 |           53464171 ± 2579062 |

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
              that were labelled in the year 2000. ‘Other Crops’ contains any 
              agricultural or crop land covers that could not be identified 
              during the photo interpretation process. ‘Other Land Cover’ 
              contains all other land covers that did not fit into the other 
              categories in strata 1 & 2, and all land covers in plots in 
              stratum 3 that did not experience forest loss or gain."))
```

| Condition          | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |        Plantation |       Terrace |  Natural\_Forest | Other\_LAND\_USE   |
| :----------------- | --------------: | -------------------------: | ----------------------: | :------------ | ----------------: | ------------: | ---------------: | :----------------- |
| Bamboo             |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |       9243 ± 6386 |       0 ± NaN |     18108 ± 9898 | 414 ± 457          |
| Built\_up          |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |           0 ± NaN |       0 ± NaN |          0 ± NaN | 50943 ± 130284     |
| Coffee             |         0 ± NaN |                    0 ± NaN |              828 ± 1606 | 0 ± NaN       |           0 ± NaN |       0 ± NaN |          0 ± NaN | 0 ± NaN            |
| Fruit\_Nut         |         0 ± NaN |                    0 ± NaN |             5722 ± 7649 | 0 ± NaN       |           0 ± NaN |       0 ± NaN |          0 ± NaN | 0 ± NaN            |
| Herbaceous         |     5588 ± 4661 |                    0 ± NaN |           26590 ± 13359 | 11276 ± 6062  |     58617 ± 26275 |       0 ± NaN |    32460 ± 15077 | 301728 ± 168072    |
| Non\_vegetated     |     1874 ± 1965 |                  138 ± 144 |             1607 ± 1215 | 0 ± NaN       |      12140 ± 5940 |       0 ± NaN |      2451 ± 1636 | 96316 ± 24628      |
| Oil\_Palm          |     3311 ± 3924 |                    0 ± NaN |             3213 ± 3850 | 0 ± NaN       |      10579 ± 9015 |       0 ± NaN |          0 ± NaN | 0 ± NaN            |
| Other\_Crop        |  117230 ± 22110 |                8647 ± 5219 |           33357 ± 10709 | 0 ± NaN       |           0 ± NaN |   7632 ± 4757 |          0 ± NaN | 256704 ± 38200     |
| Other\_LAND\_COVER |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |       3213 ± 3307 |       0 ± NaN |          0 ± NaN | 53477731 ± 2636804 |
| Other\_Palm        |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |         268 ± 475 |       0 ± NaN |      2410 ± 4310 | 0 ± NaN            |
| Other\_Shrub       |       803 ± 545 |                  134 ± 141 |             1518 ± 1590 | 25449 ± 9765  |      16539 ± 7329 |     268 ± 282 |    70733 ± 46302 | 189953 ± 47998     |
| Other\_Tree        |    16575 ± 6529 |                3405 ± 1545 |             7316 ± 3668 | 0 ± NaN       | 6824314 ± 1857479 |       0 ± NaN | 1755758 ± 647642 | 1554657 ± 696519   |
| Pulpwood           |         0 ± NaN |                    0 ± NaN |             2208 ± 2494 | 0 ± NaN       |  1534739 ± 520414 |   3213 ± 3646 |          0 ± NaN | 0 ± NaN            |
| Rice               |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |           0 ± NaN |       0 ± NaN |          0 ± NaN | 10444 ± 9523       |
| Rubber             |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |     93013 ± 30670 |       0 ± NaN |          0 ± NaN | 0 ± NaN            |
| Tea                |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |           0 ± NaN | 25302 ± 37558 |          0 ± NaN | 0 ± NaN            |
| Water              |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |           0 ± NaN |       0 ± NaN |          0 ± NaN | 22440 ± 12826      |

Table R7. Estimate of area and standard error of the area of each land
cover (in hectares), that occurred in each of the land uses that were
labelled in the year 2000. ‘Other Crops’ contains any agricultural or
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
| Agrisiviculture            |           0 ± NaN |   131091 ± 39552 |                 14291 ± 6800 |
| Boundary\_Agrisilviculture |           0 ± NaN |      6164 ± 3953 |                  6159 ± 4485 |
| Mixed\_Agrisilviculture    |           0 ± NaN |    47922 ± 17647 |                34437 ± 10712 |
| Silvopastoral              |           0 ± NaN |    36456 ± 14375 |                    268 ± 276 |
| Plantation                 | 8185306 ± 2293499 |   345128 ± 95351 |                32232 ± 10020 |
| Terrace                    |       9640 ± 6116 |      4552 ± 3771 |                22222 ± 21525 |
| Natural\_Forest            |     26573 ± 11029 | 1835348 ± 407212 |                 19999 ± 7058 |
| Other\_LAND\_USE           |   176905 ± 155569 |  639030 ± 170296 |           55145394 ± 2579597 |

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

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |     Plantation |     Terrace |  Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :------------ | -------------: | ----------: | ---------------: | :--------------- |
| Banana             |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |          0 ± 0 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coconut            |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |          0 ± 0 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coffee             |           0 ± 0 |                      0 ± 0 |               828 ± 847 | 0 ± 0         |          0 ± 0 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Fruit\_Nut         |           0 ± 0 |                      0 ± 0 |             5722 ± 4196 | 0 ± 0         |          0 ± 0 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Oil\_Palm          |     3311 ± 3386 |                      0 ± 0 |             3213 ± 3305 | 0 ± 0         |    9910 ± 5454 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Pulpwood           |           0 ± 0 |                      0 ± 0 |             2208 ± 2258 | 0 ± 0         | 106219 ± 18080 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Rubber             |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |  56496 ± 12892 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Rice               |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |          0 ± 0 |       0 ± 0 |            0 ± 0 | 10444 ± 5250     |
| Tea                |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |          0 ± 0 | 3213 ± 3305 |            0 ± 0 | 0 ± 0            |
| Other\_Crop        |  104010 ± 16900 |                2622 ± 2681 |            22512 ± 8459 | 0 ± 0         |          0 ± 0 | 1339 ± 1377 |            0 ± 0 | 214116 ± 24164   |
| Other\_Palm        |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |          0 ± 0 |       0 ± 0 |      2410 ± 2478 | 0 ± 0            |
| Other\_Tree        |    15638 ± 5545 |                3405 ± 1387 |             3023 ± 2433 | 0 ± 0         | 134192 ± 19139 |       0 ± 0 | 1717662 ± 543996 | 5291 ± 1745      |
| Other\_Shrub       |       669 ± 496 |                      0 ± 0 |             1518 ± 1552 | 25181 ± 7842  |     1359 ± 862 |       0 ± 0 |    69394 ± 43343 | 161570 ± 20002   |
| Herbaceous         |     5588 ± 4117 |                      0 ± 0 |             8095 ± 4755 | 11276 ± 4157  |   18652 ± 6645 |       0 ± 0 |     27104 ± 7646 | 126959 ± 17604   |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |    3213 ± 3305 |       0 ± 0 |            0 ± 0 | 13560 ± 5509     |

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

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |   Plantation | Terrace | Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :------------ | -----------: | ------: | --------------: | :--------------- |
| Non\_vegetated     |     1874 ± 1928 |                  138 ± 141 |               803 ± 826 | 0 ± 0         | 12140 ± 5371 |   0 ± 0 |     2451 ± 1537 | 74885 ± 13387    |
| Built\_up          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |        0 ± 0 |   0 ± 0 |           0 ± 0 | 50270 ± 32565    |
| Water              |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |        0 ± 0 |   0 ± 0 |           0 ± 0 | 14185 ± 5821     |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |  3213 ± 3305 |   0 ± 0 |           0 ± 0 | 13560 ± 5509     |

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

| Crops        | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |        Plantation |     Terrace |  Natural\_Forest | Other\_LAND\_USE |
| :----------- | --------------: | -------------------------: | ----------------------: | :------------ | ----------------: | ----------: | ---------------: | :--------------- |
| Banana       |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |             0 ± 0 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coconut      |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |             0 ± 0 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coffee       |           0 ± 0 |                      0 ± 0 |               828 ± 847 | 0 ± 0         |             0 ± 0 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Fruit\_Nut   |           0 ± 0 |                      0 ± 0 |             5722 ± 4196 | 0 ± 0         |             0 ± 0 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Oil\_Palm    |     3311 ± 3386 |                      0 ± 0 |             3213 ± 3305 | 0 ± 0         |       9910 ± 5454 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Pulpwood     |           0 ± 0 |                      0 ± 0 |             2208 ± 2258 | 0 ± 0         |  1514423 ± 522186 | 3213 ± 3305 |            0 ± 0 | 0 ± 0            |
| Rubber       |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |     84042 ± 15831 |       0 ± 0 |            0 ± 0 | 0 ± 0            |
| Rice         |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |             0 ± 0 |       0 ± 0 |            0 ± 0 | 10444 ± 5250     |
| Tea          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |             0 ± 0 | 3213 ± 3305 |            0 ± 0 | 0 ± 0            |
| Other\_Crop  |  104010 ± 16900 |                2622 ± 2681 |            22512 ± 8459 | 0 ± 0         |             0 ± 0 | 7498 ± 4682 |            0 ± 0 | 219813 ± 24444   |
| Other\_Palm  |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |         268 ± 275 |       0 ± 0 |      2410 ± 2478 | 0 ± 0            |
| Other\_Tree  |    15638 ± 5545 |                3405 ± 1387 |             3023 ± 2433 | 0 ± 0         | 6822440 ± 1243190 |       0 ± 0 | 1739909 ± 544024 | 6764 ± 1989      |
| Other\_Shrub |       669 ± 496 |                      0 ± 0 |             1518 ± 1552 | 25181 ± 7842  |      16539 ± 6158 |   268 ± 275 |    70197 ± 43351 | 168557 ± 20366   |
| Herbaceous   |     5588 ± 4117 |                      0 ± 0 |             8095 ± 4755 | 11276 ± 4157  |     58215 ± 12264 |       0 ± 0 |     29247 ± 7911 | 257044 ± 119247  |

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

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |   Plantation | Terrace | Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :------------ | -----------: | ------: | --------------: | :--------------- |
| Non\_vegetated     |     1874 ± 1928 |                  138 ± 141 |               803 ± 826 | 0 ± 0         | 12140 ± 5371 |   0 ± 0 |     2451 ± 1537 | 74885 ± 13387    |
| Built\_up          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |        0 ± 0 |   0 ± 0 |           0 ± 0 | 50270 ± 32565    |
| Water              |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |        0 ± 0 |   0 ± 0 |           0 ± 0 | 14185 ± 5821     |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |  3213 ± 3305 |   0 ± 0 |           0 ± 0 | 13560 ± 5509     |

Table R11. Estimate of area and standard error of each commodity
‘support’ land cover (in hectares), by land use, in areas that had
tree cover in the year 2000 and experienced tree canopy loss between
2001 and 2015. ‘Other Crops’ contains any agricultural or crop land
covers that could not be identified during the photo interpretation
process. ‘Other Land Use’ contains all other land covers that did not
fit into the other categories.

## Understory analysis

Investigate presence of understory

``` r
# Understory -------------------------------------------------------------------
sum(rawData$UNDERSTORY_PRESENT == "Yes")
```

    ## Warning: Unknown or uninitialised column: 'UNDERSTORY_PRESENT'.

    ## [1] 0

Zero points in any crop exist as understory.

## Carbon Analysis

``` r
# calculate Carbon values for areas --------------------------------------------
carbonMono <- c("Coffee" = 5.4, # From Vietnam
                "Fruit_Nut" = 51.63, # From Philippines + Cashew
                "Oil_Palm" = 38.97, # from Indonesia
                "Pulpwood" = 23, # from Vietnam
                "Rubber" = 31.83, # mean of time average from Vietnam & Thailand
                "Rice" = 1.05, # From Thailand
                "Tea" = 15.53, # from Vietnam
                "Other_Crop" = 6.82, # mean from several sources
                "Other_Tree" = 43.28, # mean of trees
               #"Other_Palm" = 28.66, # average of palms
                "Other_Shrub" = 10.46, # mean of shrubs
                "Herbaceous" = 6.82) # same as "other crops"

carbonAF <- c("Coffee" = 11, # From Vietnam
              "Fruit_Nut" = 51.63, # From Philippines + Cashew
              "Oil_Palm" = 38.97, # from Indonesia
              "Pulpwood" = 23, # from Vietnam
              "Rubber" = 31.83, # mean of time averages from Vietnam & Thailand
              "Rice" = 1.05, # From Thailand
              "Tea" = 22, # from Vietnam
              "Other_Crop" = 20, # from Vietnam
              "Other_Tree" = 43.28, #mean of trees
             #"Other_Palm" = 28.66, # average of palms
              "Other_Shrub" = 16.5, #mean of shrubs
              "Herbaceous" = 20) # same as "other crops"

kable(cbind(carbonMono, carbonAF), digits = 2, 
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
| Coffee       |        5.40 |        11.00 |
| Fruit\_Nut   |       51.63 |        51.63 |
| Oil\_Palm    |       38.97 |        38.97 |
| Pulpwood     |       23.00 |        23.00 |
| Rubber       |       31.83 |        31.83 |
| Rice         |        1.05 |         1.05 |
| Tea          |       15.53 |        22.00 |
| Other\_Crop  |        6.82 |        20.00 |
| Other\_Tree  |       43.28 |        43.28 |
| Other\_Shrub |       10.46 |        16.50 |
| Herbaceous   |        6.82 |        20.00 |

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

ffSP <- filter(ffAreas, condition1 == "Silvopastoral")
  # zero out herbaceous, other palm, other shrub, other tree
ffSP[,c(4,5)] <- ffSP[,c(4,5)] * c(1,1,1,1,0,1,1,1,0,0,0,1,1,1,1) 

# calculate group total area
ffAA <- ffA[,4] + ffBA[,4] + ffMA[,4] + ffSP[,4]

# calculate group area SE
ffAASE <- bind_cols(A = pull(ffA[,5])^2, 
                    BA = pull(ffBA[,5])^2,
                    MA = pull(ffMA[,5])^2, 
                    SP = pull(ffSP[,5])^2) %>%
  rowwise() %>%
  transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>%
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

| Commodity    |     MgC |     SE |
| :----------- | ------: | -----: |
| Coffee       |    9107 |   9312 |
| Fruit\_Nut   |  295403 | 216660 |
| Oil\_Palm    |  254275 | 184388 |
| Pulpwood     |   50776 |  51924 |
| Rubber       |       0 |      0 |
| Rice         |       0 |      0 |
| Tea          |       0 |      0 |
| Other\_Crop  | 2582878 | 381761 |
| Other\_Tree  |  954996 | 268882 |
| Other\_Shrub |   36089 |  26885 |
| Herbaceous   |  273650 | 125786 |

Table X. Aboveground biomass carbon values associated with the area of
commodities in agroforestry land uses that occur in formerly forested
areas.

``` r
### Calculate carbon in associated with non-AF areas
ffO <- filter(ffAreas, condition1== "Other_LAND_USE")
  # zero out herbaceous, other palm, other shrub, other tree
ffO[,c(4,5)] <- ffO[,c(4,5)] * c(1,1,1,1,0,1,1,1,0,0,0,1,1,1,1)

ffP <- filter(ffAreas, condition1 == "Plantation") #keep all elements
ffT <- filter(ffAreas, condition1 == "Terrace") #keep all elements

# Calculate group total area
ffOPT <- ffO[,4] + ffP[,4] + ffT[,4]

# Calculate group area SE
ffOPTSE <- bind_cols(O = pull(ffO[,5])^2, 
                     P = pull(ffP[,5])^2,
                     Tr = pull(ffT[,5])^2
                     ) %>%
  rowwise() %>%
  transmute(SE_ha = sum(O, P, Tr, na.rm = T)) %>%
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
| Coffee       |       0 |      0 |
| Fruit\_Nut   |       0 |      0 |
| Oil\_Palm    |  386190 | 212525 |
| Pulpwood     | 2443038 | 415837 |
| Rubber       | 1798275 | 410355 |
| Rice         |   10966 |   5512 |
| Tea          |   49904 |  51320 |
| Other\_Crop  | 1469404 | 165067 |
| Other\_Tree  | 5807847 | 828353 |
| Other\_Shrub |   14219 |   9021 |
| Herbaceous   |  127206 |  45322 |

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
