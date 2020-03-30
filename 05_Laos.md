Laos Analysis Script
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
country <- "Laos"

strata <- c(1, 2, 3)

stratumAreas <- c("Strata1 Area" = 242460, "Strata2 Area" = 1631055, 
                  "Strata3 Area" = 21115898)        

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
              "Silvopastoral", "Plantation", "Terrace", "Natural_Forest", 
              "Other_LAND_USE")
```

``` r
# Data input and cleaning ------------------------------------------------------
rawData <- read_csv("data/Corrected/Laos.csv", col_types = "ddddldcdcdddcdccccc")

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

    ## [1] "There are 201 samples in stratum 1, 529 samples in stratum 2, and 247 samples in stratum 3"

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
| Bamboo             |   1357475 |        54253 |
| Coffee             |   2117429 |        85566 |
| Fruit\_Nut         |    971066 |        39305 |
| Oil\_Palm          |      1112 |         1156 |
| Pulpwood           |    323608 |        26654 |
| Rubber             |    143294 |        18521 |
| Rice               |      1377 |         1184 |
| Tea                |    530748 |        21559 |
| Other\_Crop        |   6371144 |       143095 |
| Other\_Palm        |       125 |          128 |
| Other\_Tree        |   6042741 |       589027 |
| Other\_Shrub       |   2284599 |        56666 |
| Herbaceous         |    349877 |        22511 |
| Non\_vegetated     |    856258 |        26282 |
| Built\_up          |    803539 |        26071 |
| Water              |    891827 |        35972 |
| Other\_LAND\_COVER |    185654 |       614310 |

Table R1. The estimated area in each land cover type for Laos in the
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
| Agrisiviculture            |     34923 |         8845 |
| Boundary\_Agrisilviculture |   4228561 |       120706 |
| Mixed\_Agrisilviculture    |   1987911 |        79207 |
| Strip\_Agrisilviculture    |      2253 |         2312 |
| Silvopastoral              |       626 |          642 |
| Plantation                 |    619567 |       549837 |
| Terrace                    |     13050 |         5696 |
| Natural\_Forest            |   6974594 |       140098 |
| Other\_LAND\_USE           |   9370389 |       568012 |

Table R2. The estimated area in each land use type for Laos in the
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
| Forest\_Commodity            |   424777 |       549648 |
| Natural\_Forest              |  9899805 |       164214 |
| Other\_LAND\_USE\_YEAR\_2000 | 12907290 |       561304 |

Table R3. The estimated area in each land use type for Laos in the year
2000. This data was collected using question 3 in the Collect Earth
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
| Not\_Tree |  12907290 |       561304 |
| Tree      |  10324583 |       561204 |

Table R4. The estimated area of tree canopy cover in Laos for the year
2000. This data was collected using question 3 in the Collect Earth
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

| Cover              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |      Plantation |     Terrace | Natural\_Forest  | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | --------------: | ----------: | :--------------- | ---------------: |
| Bamboo             |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |     3129 ± 4994 |     0 ± NaN | 1354221 ± 66445  |        125 ± 129 |
| Coffee             |       876 ± 953 |                    0 ± NaN |             2835 ± 1914 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |     0 ± NaN | 0 ± NaN          | 2113717 ± 200435 |
| Fruit\_Nut         |         0 ± NaN |                    0 ± NaN |          968062 ± 67361 | 0 ± NaN                 |       0 ± NaN |     3004 ± 4870 |     0 ± NaN | 0 ± NaN          |          0 ± NaN |
| Oil\_Palm          |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |     1112 ± 1812 |     0 ± NaN | 0 ± NaN          |          0 ± NaN |
| Pulpwood           |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN | 323483 ± 406861 |     0 ± NaN | 0 ± NaN          |        125 ± 129 |
| Rubber             |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN | 134624 ± 169916 | 7919 ± 6511 | 0 ± NaN          |        751 ± 658 |
| Rice               |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |     0 ± NaN | 0 ± NaN          |      1377 ± 1190 |
| Tea                |         0 ± NaN |                    0 ± NaN |          527897 ± 36628 | 0 ± NaN                 |       0 ± NaN |         0 ± NaN | 2753 ± 2687 | 0 ± NaN          |         97 ± 101 |
| Other\_Crop        |   26662 ± 13118 |           3961859 ± 196755 |          112407 ± 10247 | 2003 ± 3812             |       0 ± NaN |         0 ± NaN | 1627 ± 2081 | 0 ± NaN          | 2266586 ± 213353 |
| Other\_Palm        |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |     0 ± NaN | 125 ± 129        |          0 ± NaN |
| Other\_Tree        |     6759 ± 5134 |             177343 ± 10664 |          367106 ± 27524 | 250 ± 445               |       0 ± NaN |  78084 ± 557590 |   626 ± 603 | 4953121 ± 176593 |  459453 ± 280554 |
| Other\_Shrub       |       626 ± 535 |               88358 ± 5317 |             2253 ± 2192 | 0 ± NaN                 |       0 ± NaN |   11767 ± 15297 |     0 ± NaN | 120428 ± 8727    | 2061167 ± 187267 |
| Herbaceous         |         0 ± NaN |                1001 ± 1029 |             7350 ± 3609 | 0 ± NaN                 |    626 ± 1160 |   47674 ± 60605 |   125 ± 151 | 18136 ± 5860     |   274965 ± 31182 |
| Non\_vegetated     |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |   14313 ± 18724 |     0 ± NaN | 528438 ± 26140   |   313508 ± 30545 |
| Built\_up          |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |     0 ± NaN | 0 ± NaN          |   803539 ± 73654 |
| Water              |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |         0 ± NaN |     0 ± NaN | 125 ± 129        |   891702 ± 84481 |
| Other\_LAND\_COVER |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |     2378 ± 3859 |     0 ± NaN | 0 ± NaN          |  183276 ± 604888 |

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
| Bamboo             |       1377 ± 2633 |  1352853 ± 64888 |                  3245 ± 3100 |
| Coffee             |           0 ± NaN |      5066 ± 3535 |             2112363 ± 157136 |
| Fruit\_Nut         |       3004 ± 6294 |          0 ± NaN |               968062 ± 72015 |
| Oil\_Palm          |       1112 ± 2341 |          0 ± NaN |                      0 ± NaN |
| Pulpwood           |   226283 ± 414842 |    87249 ± 14971 |                 10077 ± 4528 |
| Rubber             |     22806 ± 42425 |   105675 ± 16475 |                 14812 ± 5945 |
| Rice               |           0 ± NaN |      1377 ± 1184 |                      0 ± NaN |
| Tea                |       2753 ± 5779 |         97 ± 101 |               527897 ± 38428 |
| Other\_Crop        |      7600 ± 14512 |  2269095 ± 84479 |             4094448 ± 276674 |
| Other\_Palm        |           0 ± NaN |        125 ± 128 |                      0 ± NaN |
| Other\_Tree        |    74853 ± 567715 | 4965052 ± 185914 |             1002836 ± 279560 |
| Other\_Shrub       |     13442 ± 25004 |   367915 ± 24739 |             1903241 ± 126489 |
| Herbaceous         |     52552 ± 96544 |   164874 ± 19175 |               132450 ± 13592 |
| Non\_vegetated     |     11485 ± 21666 |   565903 ± 27215 |               278870 ± 21108 |
| Built\_up          |       1252 ± 2412 |      8610 ± 2765 |               793678 ± 55906 |
| Water              |       3880 ± 7794 |      1647 ± 1017 |               886300 ± 66342 |
| Other\_LAND\_COVER |       2378 ± 4995 |      4267 ± 2299 |              179010 ± 614876 |

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

| Condition          | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |     Plantation |     Terrace | Natural\_Forest  | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | -------------: | ----------: | :--------------- | ---------------: |
| Bamboo             |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |    3129 ± 3091 |     0 ± NaN | 1354221 ± 93883  |        125 ± 129 |
| Built\_up          |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |        0 ± NaN |     0 ± NaN | 0 ± NaN          |   803539 ± 45335 |
| Coffee             |       876 ± 901 |                    0 ± NaN |             2835 ± 1915 | 0 ± NaN                 |       0 ± NaN |        0 ± NaN |     0 ± NaN | 0 ± NaN          | 2113717 ± 148592 |
| Fruit\_Nut         |         0 ± NaN |                    0 ± NaN |          968062 ± 67877 | 0 ± NaN                 |       0 ± NaN |    3004 ± 3088 |     0 ± NaN | 0 ± NaN          |          0 ± NaN |
| Herbaceous         |         0 ± NaN |                1001 ± 1032 |             7350 ± 3608 | 0 ± NaN                 |     626 ± 645 |  47674 ± 10547 |   125 ± 129 | 18136 ± 6066     |   274965 ± 37166 |
| Non\_vegetated     |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |   14313 ± 5338 |     0 ± NaN | 528438 ± 31354   |   313508 ± 29479 |
| Oil\_Palm          |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |    1112 ± 2122 |     0 ± NaN | 0 ± NaN          |          0 ± NaN |
| Other\_Crop        |    26662 ± 7525 |           3961859 ± 169445 |           112407 ± 8706 | 2003 ± 2057             |       0 ± NaN |        0 ± NaN | 1627 ± 1671 | 0 ± NaN          | 2266586 ± 128582 |
| Other\_LAND\_COVER |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |   2378 ± 11394 |     0 ± NaN | 0 ± NaN          |  183276 ± 606498 |
| Other\_Palm        |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |        0 ± NaN |     0 ± NaN | 125 ± 223        |          0 ± NaN |
| Other\_Shrub       |       626 ± 463 |               88358 ± 4742 |             2253 ± 2189 | 0 ± NaN                 |       0 ± NaN |   11767 ± 4099 |     0 ± NaN | 120428 ± 8893    | 2061167 ± 127166 |
| Other\_Tree        |     6759 ± 4340 |             177343 ± 25481 |          367106 ± 52831 | 250 ± 259               |       0 ± NaN | 78084 ± 549050 |   626 ± 471 | 4953121 ± 691091 |  459453 ± 299969 |
| Pulpwood           |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN | 323483 ± 49723 |     0 ± NaN | 0 ± NaN          |        125 ± 129 |
| Rice               |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |        0 ± NaN |     0 ± NaN | 0 ± NaN          |      1377 ± 2051 |
| Rubber             |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN | 134624 ± 33023 | 7919 ± 4536 | 0 ± NaN          |        751 ± 669 |
| Tea                |         0 ± NaN |                    0 ± NaN |          527897 ± 37174 | 0 ± NaN                 |       0 ± NaN |        0 ± NaN | 2753 ± 2831 | 0 ± NaN          |         97 ± 101 |
| Water              |         0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN                 |       0 ± NaN |        0 ± NaN |     0 ± NaN | 125 ± 129        |   891702 ± 62296 |

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
| Agrisiviculture            |           0 ± NaN |     31919 ± 8350 |                  3004 ± 3089 |
| Boundary\_Agrisilviculture |       3004 ± 6294 |      2378 ± 1756 |             4223180 ± 289893 |
| Mixed\_Agrisilviculture    |       4164 ± 8298 |     21397 ± 7203 |             1962350 ± 145650 |
| Strip\_Agrisilviculture    |           0 ± NaN |          0 ± NaN |                  2253 ± 2316 |
| Silvopastoral              |           0 ± NaN |        626 ± 643 |                      0 ± NaN |
| Plantation                 |   373919 ± 884565 |   220461 ± 23245 |                 25187 ± 7591 |
| Terrace                    |       3004 ± 6304 |     10046 ± 4807 |                      0 ± NaN |
| Natural\_Forest            |     12459 ± 23473 | 6936529 ± 139435 |                 25606 ± 8252 |
| Other\_LAND\_USE           |     28228 ± 52395 |  2676450 ± 90074 |             6665712 ± 716874 |

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

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |    Plantation |     Terrace | Natural\_Forest  | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | ------------: | ----------: | :--------------- | ---------------: |
| Banana             |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |         0 ± 0 |       0 ± 0 | 0 ± 0            |            0 ± 0 |
| Coconut            |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |         0 ± 0 |       0 ± 0 | 0 ± 0            |            0 ± 0 |
| Coffee             |       876 ± 899 |                      0 ± 0 |             2062 ± 1730 | 0 ± 0                   |         0 ± 0 |         0 ± 0 |       0 ± 0 | 0 ± 0            |      2128 ± 2184 |
| Fruit\_Nut         |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |         0 ± 0 |       0 ± 0 | 0 ± 0            |            0 ± 0 |
| Oil\_Palm          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |         0 ± 0 |       0 ± 0 | 0 ± 0            |            0 ± 0 |
| Pulpwood           |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 | 87124 ± 14795 |       0 ± 0 | 0 ± 0            |        125 ± 128 |
| Rubber             |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 | 97632 ± 15733 | 7919 ± 4297 | 0 ± 0            |        125 ± 128 |
| Rice               |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |         0 ± 0 |       0 ± 0 | 0 ± 0            |      1377 ± 1184 |
| Tea                |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |         0 ± 0 |       0 ± 0 | 0 ± 0            |         97 ± 101 |
| Other\_Crop        |    24159 ± 7034 |                  375 ± 385 |             7528 ± 3625 | 0 ± 0                   |         0 ± 0 |         0 ± 0 | 1627 ± 1670 | 0 ± 0            |  2235405 ± 87184 |
| Other\_Palm        |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |         0 ± 0 |       0 ± 0 | 125 ± 128        |            0 ± 0 |
| Other\_Tree        |     6258 ± 4209 |                  626 ± 463 |             7961 ± 3812 | 0 ± 0                   |         0 ± 0 |  19581 ± 6498 |   375 ± 385 | 4927063 ± 106592 |      3189 ± 1580 |
| Other\_Shrub       |       626 ± 463 |                  375 ± 385 |             2128 ± 2184 | 0 ± 0                   |         0 ± 0 |    1092 ± 775 |       0 ± 0 | 118926 ± 7791    |   244768 ± 23193 |
| Herbaceous         |           0 ± 0 |                1001 ± 1028 |             1718 ± 1267 | 0 ± 0                   |     626 ± 642 |   5154 ± 2858 |   125 ± 128 | 12253 ± 4580     |   143997 ± 18051 |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |         0 ± 0 |       0 ± 0 | 0 ± 0            |      4267 ± 2298 |

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

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |   Plantation | Terrace | Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | -----------: | ------: | :-------------- | ---------------: |
| Non\_vegetated     |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 | 14313 ± 5278 |   0 ± 0 | 528438 ± 21375  |     34638 ± 7894 |
| Built\_up          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |        0 ± 0 |   0 ± 0 | 0 ± 0           |      9862 ± 2854 |
| Water              |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |        0 ± 0 |   0 ± 0 | 125 ± 128       |      5402 ± 3362 |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |  2378 ± 2441 |   0 ± 0 | 0 ± 0           |      4267 ± 2298 |

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

| Crops        | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |     Plantation |     Terrace | Natural\_Forest  | Other\_LAND\_USE |
| :----------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | -------------: | ----------: | :--------------- | ---------------: |
| Banana       |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |       0 ± 0 | 0 ± 0            |            0 ± 0 |
| Coconut      |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |       0 ± 0 | 0 ± 0            |            0 ± 0 |
| Coffee       |       876 ± 899 |                      0 ± 0 |             2062 ± 1730 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |       0 ± 0 | 0 ± 0            |      2128 ± 2184 |
| Fruit\_Nut   |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |    3004 ± 3083 |       0 ± 0 | 0 ± 0            |            0 ± 0 |
| Oil\_Palm    |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |    1112 ± 1156 |       0 ± 0 | 0 ± 0            |            0 ± 0 |
| Pulpwood     |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 | 313407 ± 26430 |       0 ± 0 | 0 ± 0            |        125 ± 128 |
| Rubber       |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 | 119812 ± 17140 | 7919 ± 4297 | 0 ± 0            |        751 ± 655 |
| Rice         |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |       0 ± 0 | 0 ± 0            |      1377 ± 1184 |
| Tea          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 | 2753 ± 2826 | 0 ± 0            |         97 ± 101 |
| Other\_Crop  |    24159 ± 7034 |                2628 ± 2344 |             8495 ± 3759 | 0 ± 0                   |         0 ± 0 |          0 ± 0 | 1627 ± 1670 | 0 ± 0            |  2239786 ± 87232 |
| Other\_Palm  |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |          0 ± 0 |       0 ± 0 | 125 ± 128        |            0 ± 0 |
| Other\_Tree  |     6258 ± 4209 |                 1377 ± 898 |            11158 ± 4899 | 0 ± 0                   |         0 ± 0 | 77833 ± 548911 |   626 ± 463 | 4936768 ± 106640 |      5885 ± 2318 |
| Other\_Shrub |       626 ± 463 |                  375 ± 385 |             2128 ± 2184 | 0 ± 0                   |         0 ± 0 |   11767 ± 4092 |       0 ± 0 | 119177 ± 7794    |   247285 ± 23209 |
| Herbaceous   |           0 ± 0 |                1001 ± 1028 |             1718 ± 1267 | 0 ± 0                   |     626 ± 642 |   47626 ± 9614 |   125 ± 128 | 13380 ± 4718     |   152951 ± 18422 |

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

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Strip\_Agrisilviculture | Silvopastoral |   Plantation | Terrace | Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :---------------------- | ------------: | -----------: | ------: | :-------------- | ---------------: |
| Non\_vegetated     |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 | 14313 ± 5278 |   0 ± 0 | 528438 ± 21375  |     34638 ± 7894 |
| Built\_up          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |        0 ± 0 |   0 ± 0 | 0 ± 0           |      9862 ± 2854 |
| Water              |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |        0 ± 0 |   0 ± 0 | 125 ± 128       |      5402 ± 3362 |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0                   |         0 ± 0 |  2378 ± 2441 |   0 ± 0 | 0 ± 0           |      4267 ± 2298 |

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

    ## [1] 114

``` r
unique(rawData$UNDERSTORY_COVER)
```

    ## [1] NA           "Other_Crop" "Coffee"

114 points in other crops and coffee exist as understory. Will exclude,
from carbon numbers, as it’s already included in the agroforestry
landuse by default. But maybe should mention the area.

``` r
underCrops <- c("Coffee", "Other_Crop")
treeCrops <- c("Coconut", "Fruit_Nut" ,"Pulpwood", "Rubber", "Oil_Palm", 
               "Other_Tree", "Other_Palm", "Other_Crop")

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

uResults <- bind_rows(uy_occResults$Cover)
colnames(uResults) <- c("understory", "overstory", "present", "PercentCover", "SE")

uResults %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas), 
         SE = SE * sum(stratumAreas)) %>% 
  mutate(Pretty = paste(round(PercentCover), round(SE), sep = " \u00B1 ")) %>% 
  select(understory, overstory, Pretty) %>% 
  spread(understory, Pretty)  
```

    ## # A tibble: 5 x 3
    ##   overstory  Coffee      Other_Crop 
    ##   <chr>      <chr>       <chr>      
    ## 1 Oil_Palm   0 ± 0       0 ± 0      
    ## 2 Other_Crop 0 ± 0       1877 ± 1927
    ## 3 Other_Palm 0 ± 0       0 ± 0      
    ## 4 Other_Tree 8806 ± 4713 1126 ± 1156
    ## 5 Rubber     0 ± 0       0 ± 0

## Carbon Analysis

``` r
# calculate Carbon values for areas --------------------------------------------
carbonMono <- c("Coffee" = 5.4, # From Vietnam
                "Pulpwood" = 23, # from Vietnam
                "Rubber" = 31.83, # mean of time average from Vietnam & Thailand
                "Rice" = 1.05, # From Thailand
                "Tea" = 15.53, # from Vietnam
                "Other_Crop" = 6.82, # mean from several sources
                "Other_Tree" = 43.28, # mean of trees for Cambodia
                "Other_Shrub" = 10.46, # mean of shrubs
                "Herbaceous" = 6.82) # same as "other crops"

carbonAF <- c("Coffee" = 11, # From Vietnam
              "Pulpwood" = 23, # from Vietnam
              "Rubber" = 31.83, # mean of time averages from Vietnam & Thailand
              "Rice" = 1.05, # From Thailand
              "Tea" = 22, # from Vietnam
              "Other_Crop" = 20, # from Vietnam
              "Other_Tree" = 43.28, #mean of trees for Cambodia
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
| Coffee       |         5.4 |         11.0 |
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

| Commodity    |    MgC |     SE |
| :----------- | -----: | -----: |
| Coffee       |  32320 |  21450 |
| Pulpwood     |      0 |      0 |
| Rubber       |      0 |      0 |
| Rice         |      0 |      0 |
| Tea          |      0 |      0 |
| Other\_Crop  | 641258 | 158445 |
| Other\_Tree  | 642474 | 246586 |
| Other\_Shrub |  51628 |  37381 |
| Herbaceous   |  54379 |  32625 |

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

| Commodity    |      MgC |     SE |
| :----------- | -------: | -----: |
| Coffee       |    11489 |  11794 |
| Pulpwood     |  2006725 | 340291 |
| Rubber       |  3363650 | 500809 |
| Rice         |     1446 |   1243 |
| Tea          |     1501 |   1561 |
| Other\_Crop  | 15256562 | 594593 |
| Other\_Tree  |   863704 | 281221 |
| Other\_Shrub |    11421 |   8105 |
| Herbaceous   |    36003 |  19489 |

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
