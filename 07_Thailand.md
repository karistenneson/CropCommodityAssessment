Thailand Analysis Script
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
country <- "Thailand"

strata <- c(1, 2, 3)

stratumAreas <- c("Strata1 Area" = 178405, "Strata2 Area" = 1334277, 
                  "Strata3 Area" = 49901653)        

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
              "Mixed_Agrisilviculture", "Silvopastoral", "Plantation", "Terrace", 
              "Natural_Forest", "Other_LAND_USE")
```

``` r
# Data input and cleaning ------------------------------------------------------
rawData <- read_csv("data/Corrected/Thailand.csv", col_types = "ddddldcdcdddcdccccc")

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

    ## [1] "There are 221 samples in stratum 1, 545 samples in stratum 2, and 250 samples in stratum 3"

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
| Bamboo             |      8066 |         3397 |
| Coconut            |      5466 |         3037 |
| Coffee             |       991 |         1020 |
| Fruit\_Nut         |     15779 |         5185 |
| Oil\_Palm          |    160100 |        16787 |
| Pulpwood           |   3346445 |       135075 |
| Rubber             |   3884728 |        92277 |
| Rice               |   4539671 |       199623 |
| Tea                |        99 |          102 |
| Other\_Crop        |   7212957 |       214837 |
| Other\_Palm        |    568452 |        24962 |
| Other\_Tree        |  10614524 |      1170035 |
| Other\_Shrub       |   5176306 |       108522 |
| Herbaceous         |   7130469 |       171606 |
| Non\_vegetated     |   8188820 |       153425 |
| Built\_up          |     16399 |         2968 |
| Water              |    574532 |        25131 |
| Other\_LAND\_COVER |    148936 |      1274345 |

Table R1. The estimated area in each land cover type for Thailand in the
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
| Agrisiviculture            |  13493375 |       339836 |
| Boundary\_Agrisilviculture |      7094 |         3486 |
| Mixed\_Agrisilviculture    |    110953 |        14776 |
| Silvopastoral              |      7873 |         3813 |
| Plantation                 |   7744059 |       932347 |
| Terrace                    |       198 |          204 |
| Natural\_Forest            |   6734795 |       219678 |
| Other\_LAND\_USE           |  23494394 |      1002854 |

Table R2. The estimated area in each land use type for Thailand in the
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
| Forest\_Commodity            | 15044611 |       965450 |
| Natural\_Forest              | 11954283 |       297129 |
| Other\_LAND\_USE\_YEAR\_2000 | 24593847 |      1002358 |

Table R3. The estimated area in each land use type for Thailand in the
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
| Not\_Tree |  24593847 |      1002358 |
| Tree      |  26998893 |      1002321 |

Table R4. The estimated area of tree canopy cover in Thailand for the
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

    ## Warning in sqrt(strataVar): NaNs produced

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

| Cover              |  Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |       Plantation |   Terrace |  Natural\_Forest | Other\_LAND\_USE |
| :----------------- | ---------------: | -------------------------: | ----------------------: | :------------ | ---------------: | --------: | ---------------: | :--------------- |
| Bamboo             |          0 ± NaN |                    0 ± NaN |               495 ± 519 | 0 ± NaN       |        779 ± 818 |   0 ± NaN |      6792 ± 3280 | 0 ± NaN          |
| Coconut            |          0 ± NaN |                    0 ± NaN |               779 ± 821 | 0 ± NaN       |      4687 ± 3035 |   0 ± NaN |          0 ± NaN | 0 ± NaN          |
| Coffee             |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |          0 ± NaN |   0 ± NaN |          0 ± NaN | 991 ± 1022       |
| Fruit\_Nut         |          0 ± NaN |                    0 ± NaN |             6705 ± 4005 | 0 ± NaN       |      8423 ± 3788 |   0 ± NaN |          0 ± NaN | 651 ± 582        |
| Oil\_Palm          |          0 ± NaN |                    0 ± NaN |            30290 ± 9685 | 0 ± NaN       |   129810 ± 26896 |   0 ± NaN |          0 ± NaN | 0 ± NaN          |
| Pulpwood           |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       | 3346445 ± 585575 |   0 ± NaN |          0 ± NaN | 0 ± NaN          |
| Rubber             |      1883 ± 1939 |                    0 ± NaN |               793 ± 830 | 0 ± NaN       | 3882052 ± 667387 |   0 ± NaN |          0 ± NaN | 0 ± NaN          |
| Rice               |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |          0 ± NaN |   0 ± NaN |          0 ± NaN | 4539671 ± 339038 |
| Tea                |          0 ± NaN |                    0 ± NaN |                99 ± 104 | 0 ± NaN       |          0 ± NaN |   0 ± NaN |          0 ± NaN | 0 ± NaN          |
| Other\_Crop        | 6108674 ± 303160 |                2937 ± 2620 |           52406 ± 14168 | 0 ± NaN       |          0 ± NaN | 198 ± 441 |          0 ± NaN | 1048741 ± 74724  |
| Other\_Palm        |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |        396 ± 329 |   0 ± NaN |         99 ± 102 | 567956 ± 42076   |
| Other\_Tree        |  1705042 ± 92406 |                1287 ± 1510 |             7243 ± 2948 | 1608 ± 1683   |   83034 ± 904401 |   0 ± NaN | 6528573 ± 375159 | 2287739 ± 817694 |
| Other\_Shrub       |   756480 ± 45577 |                  528 ± 655 |             2568 ± 1466 | 552 ± 704     |     11835 ± 4086 |   0 ± NaN |      6089 ± 2064 | 4398253 ± 287232 |
| Herbaceous         | 3594578 ± 191428 |                2310 ± 2452 |             8485 ± 2916 | 5713 ± 5034   |    54016 ± 12635 |   0 ± NaN |      4220 ± 1821 | 3461147 ± 230126 |
| Non\_vegetated     |  1326717 ± 65115 |                    32 ± 41 |              1090 ± 678 | 0 ± NaN       |   221987 ± 39457 |   0 ± NaN |   189021 ± 12050 | 6449971 ± 415404 |
| Built\_up          |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |          0 ± NaN |   0 ± NaN |          0 ± NaN | 16399 ± 3128     |
| Water              |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |        595 ± 467 |   0 ± NaN |          0 ± NaN | 573938 ± 42798   |
| Other\_LAND\_COVER |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |          0 ± NaN |   0 ± NaN |          0 ± NaN | 148936 ± 1269788 |

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
| Bamboo             |         779 ± 810 |      6629 ± 3270 |                    658 ± 538 |
| Coconut            |       4687 ± 2958 |        779 ± 808 |                      0 ± NaN |
| Coffee             |           0 ± NaN |       991 ± 1021 |                      0 ± NaN |
| Fruit\_Nut         |       3680 ± 2373 |     10844 ± 4492 |                  1254 ± 1228 |
| Oil\_Palm          |     52355 ± 10838 |    91798 ± 13718 |                 15946 ± 5790 |
| Pulpwood           |  3144895 ± 330608 |   171140 ± 19143 |                 30410 ± 8109 |
| Rubber             |  2621460 ± 260320 |  1239447 ± 80252 |                 23821 ± 7216 |
| Rice               |           0 ± NaN |          0 ± NaN |             4539671 ± 326526 |
| Tea                |           0 ± NaN |          0 ± NaN |                     99 ± 102 |
| Other\_Crop        |  6060852 ± 589205 |   136488 ± 16330 |              1015617 ± 70551 |
| Other\_Palm        |         793 ± 525 |         99 ± 102 |               567560 ± 41173 |
| Other\_Tree        | 1545835 ± 1013368 | 6575445 ± 382320 |             2493244 ± 800243 |
| Other\_Shrub       |    199102 ± 20167 |   609658 ± 25292 |             4367546 ± 273092 |
| Herbaceous         |    430415 ± 41248 | 1944991 ± 110703 |             4755063 ± 314080 |
| Non\_vegetated     |    976522 ± 98265 |  1153222 ± 39393 |             6059077 ± 375325 |
| Built\_up          |        2047 ± 770 |      7960 ± 2183 |                  6392 ± 1985 |
| Water              |        1189 ± 653 |      4564 ± 2709 |               568780 ± 40890 |
| Other\_LAND\_COVER |           0 ± NaN |        227 ± 236 |             148709 ± 1274745 |

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

| Condition          |  Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |       Plantation |   Terrace |   Natural\_Forest | Other\_LAND\_USE |
| :----------------- | ---------------: | -------------------------: | ----------------------: | :------------ | ---------------: | --------: | ----------------: | :--------------- |
| Bamboo             |          0 ± NaN |                    0 ± NaN |               495 ± 591 | 0 ± NaN       |        779 ± 936 |   0 ± NaN |       6792 ± 5283 | 0 ± NaN          |
| Built\_up          |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |          0 ± NaN |   0 ± NaN |           0 ± NaN | 16399 ± 5141     |
| Coconut            |          0 ± NaN |                    0 ± NaN |              779 ± 1015 | 0 ± NaN       |      4687 ± 4803 |   0 ± NaN |           0 ± NaN | 0 ± NaN          |
| Coffee             |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |          0 ± NaN |   0 ± NaN |           0 ± NaN | 991 ± 1767       |
| Fruit\_Nut         |          0 ± NaN |                    0 ± NaN |             6705 ± 5039 | 0 ± NaN       |      8423 ± 5344 |   0 ± NaN |           0 ± NaN | 651 ± 655        |
| Herbaceous         | 3594578 ± 187670 |                2310 ± 1779 |             8485 ± 2916 | 5713 ± 2974   |     54016 ± 8853 |   0 ± NaN |       4220 ± 1816 | 3461147 ± 215321 |
| Non\_vegetated     |  1326717 ± 56944 |                    32 ± 34 |              1090 ± 684 | 0 ± NaN       |   221987 ± 13350 |   0 ± NaN |     189021 ± 9709 | 6449971 ± 348182 |
| Oil\_Palm          |          0 ± NaN |                    0 ± NaN |            30290 ± 8993 | 0 ± NaN       |   129810 ± 25647 |   0 ± NaN |           0 ± NaN | 0 ± NaN          |
| Other\_Crop        | 6108674 ± 332395 |                2937 ± 1533 |           52406 ± 10046 | 0 ± NaN       |          0 ± NaN | 198 ± 204 |           0 ± NaN | 1048741 ± 62160  |
| Other\_LAND\_COVER |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |          0 ± NaN |   0 ± NaN |           0 ± NaN | 148936 ± 2209032 |
| Other\_Palm        |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |        396 ± 324 |   0 ± NaN |          99 ± 102 | 567956 ± 24942   |
| Other\_Shrub       |   756480 ± 34543 |                  528 ± 511 |             2568 ± 1323 | 552 ± 572     |     11835 ± 3575 |   0 ± NaN |       6089 ± 2051 | 4398253 ± 163534 |
| Other\_Tree        | 1705042 ± 274122 |                 1287 ± 891 |             7243 ± 2611 | 1608 ± 962    |   83034 ± 907604 |   0 ± NaN | 6528573 ± 1037549 | 2287739 ± 873646 |
| Pulpwood           |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       | 3346445 ± 252853 |   0 ± NaN |           0 ± NaN | 0 ± NaN          |
| Rice               |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |          0 ± NaN |   0 ± NaN |           0 ± NaN | 4539671 ± 345815 |
| Rubber             |      1883 ± 1939 |                    0 ± NaN |               793 ± 817 | 0 ± NaN       |    3882052 ± NaN |   0 ± NaN |           0 ± NaN | 0 ± NaN          |
| Tea                |          0 ± NaN |                    0 ± NaN |                99 ± 229 | 0 ± NaN       |          0 ± NaN |   0 ± NaN |           0 ± NaN | 0 ± NaN          |
| Water              |          0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN       |        595 ± 458 |   0 ± NaN |           0 ± NaN | 573938 ± NaN     |

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
| Agrisiviculture            |  9076681 ± 819450 |    60787 ± 11254 |             4355907 ± 317026 |
| Boundary\_Agrisilviculture |         779 ± 810 |        198 ± 204 |                  6117 ± 3406 |
| Mixed\_Agrisilviculture    |       7051 ± 3430 |    61108 ± 11414 |                 42794 ± 9807 |
| Silvopastoral              |       2378 ± 2458 |      3157 ± 2580 |                  2337 ± 1398 |
| Plantation                 | 5934409 ± 1078786 | 1731992 ± 103750 |                77658 ± 13293 |
| Terrace                    |         198 ± 205 |          0 ± NaN |                      0 ± NaN |
| Natural\_Forest            |       8471 ± 3573 | 6721663 ± 223981 |                  4661 ± 2339 |
| Other\_LAND\_USE           |      14643 ± 4824 | 3375377 ± 135679 |           20104373 ± 1583942 |

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

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |      Plantation | Terrace |  Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :------------ | --------------: | ------: | ---------------: | :--------------- |
| Banana             |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |           0 ± 0 |   0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coconut            |           0 ± 0 |                      0 ± 0 |               779 ± 807 | 0 ± 0         |           0 ± 0 |   0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coffee             |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |           0 ± 0 |   0 ± 0 |            0 ± 0 | 991 ± 1020       |
| Fruit\_Nut         |           0 ± 0 |                      0 ± 0 |             6243 ± 3779 | 0 ± 0         |     4049 ± 2345 |   0 ± 0 |            0 ± 0 | 552 ± 572        |
| Oil\_Palm          |           0 ± 0 |                      0 ± 0 |            28911 ± 7576 | 0 ± 0         |   62887 ± 11327 |   0 ± 0 |            0 ± 0 | 0 ± 0            |
| Pulpwood           |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |  171140 ± 18125 |   0 ± 0 |            0 ± 0 | 0 ± 0            |
| Rubber             |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         | 1239447 ± 51945 |   0 ± 0 |            0 ± 0 | 0 ± 0            |
| Rice               |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |           0 ± 0 |   0 ± 0 |            0 ± 0 | 0 ± 0            |
| Tea                |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |           0 ± 0 |   0 ± 0 |            0 ± 0 | 0 ± 0            |
| Other\_Crop        |   58027 ± 10665 |                      0 ± 0 |            21180 ± 6630 | 0 ± 0         |           0 ± 0 |   0 ± 0 |            0 ± 0 | 57280 ± 10197    |
| Other\_Palm        |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |           0 ± 0 |   0 ± 0 |         99 ± 102 | 0 ± 0            |
| Other\_Tree        |      1570 ± 748 |                  198 ± 204 |              1276 ± 679 | 198 ± 204     |   52553 ± 10118 |   0 ± 0 | 6517836 ± 212056 | 1813 ± 820       |
| Other\_Shrub       |           0 ± 0 |                      0 ± 0 |               851 ± 571 | 552 ± 572     |     2997 ± 1704 |   0 ± 0 |      5343 ± 1971 | 599916 ± 26137   |
| Herbaceous         |     1189 ± 1224 |                      0 ± 0 |              1174 ± 714 | 2407 ± 2257   |     6265 ± 2837 |   0 ± 0 |       2734 ± 974 | 1931222 ± 83577  |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |           0 ± 0 |   0 ± 0 |            0 ± 0 | 227 ± 235        |

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

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |     Plantation | Terrace | Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :------------ | -------------: | ------: | --------------: | :--------------- |
| Non\_vegetated     |  945900 ± 41589 |                      0 ± 0 |               694 ± 620 | 0 ± 0         | 221690 ± 11309 |   0 ± 0 |   189021 ± 8317 | 772439 ± 33634   |
| Built\_up          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |          0 ± 0 |   0 ± 0 |           0 ± 0 | 10007 ± 2279     |
| Water              |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |      595 ± 456 |   0 ± 0 |           0 ± 0 | 5158 ± 2741      |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |          0 ± 0 |   0 ± 0 |           0 ± 0 | 227 ± 235        |

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

| Crops        |  Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |       Plantation |   Terrace |  Natural\_Forest | Other\_LAND\_USE |
| :----------- | ---------------: | -------------------------: | ----------------------: | :------------ | ---------------: | --------: | ---------------: | :--------------- |
| Banana       |            0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |            0 ± 0 |     0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coconut      |            0 ± 0 |                      0 ± 0 |               779 ± 807 | 0 ± 0         |      4687 ± 2928 |     0 ± 0 |            0 ± 0 | 0 ± 0            |
| Coffee       |            0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |            0 ± 0 |     0 ± 0 |            0 ± 0 | 991 ± 1020       |
| Fruit\_Nut   |            0 ± 0 |                      0 ± 0 |             6640 ± 3799 | 0 ± 0         |      7234 ± 3289 |     0 ± 0 |            0 ± 0 | 651 ± 581        |
| Oil\_Palm    |            0 ± 0 |                      0 ± 0 |            28911 ± 7576 | 0 ± 0         |   115242 ± 14542 |     0 ± 0 |            0 ± 0 | 0 ± 0            |
| Pulpwood     |            0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         | 3316035 ± 134951 |     0 ± 0 |            0 ± 0 | 0 ± 0            |
| Rubber       |      1883 ± 1938 |                      0 ± 0 |               793 ± 816 | 0 ± 0         |  3858231 ± 92109 |     0 ± 0 |            0 ± 0 | 0 ± 0            |
| Rice         |            0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |            0 ± 0 |     0 ± 0 |            0 ± 0 | 0 ± 0            |
| Tea          |            0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |            0 ± 0 |     0 ± 0 |            0 ± 0 | 0 ± 0            |
| Other\_Crop  | 6107492 ± 210422 |                  747 ± 774 |            25530 ± 7026 | 0 ± 0         |            0 ± 0 | 198 ± 204 |            0 ± 0 | 63373 ± 10668    |
| Other\_Palm  |            0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |        396 ± 322 |     0 ± 0 |         99 ± 102 | 396 ± 408        |
| Other\_Tree  |  1513940 ± 66540 |                  231 ± 207 |             2561 ± 1244 | 991 ± 841     |   77014 ± 914075 |     0 ± 0 | 6524302 ± 212067 | 2242 ± 880       |
| Other\_Shrub |    189021 ± 8317 |                      0 ± 0 |               851 ± 571 | 552 ± 572     |     11835 ± 3558 |     0 ± 0 |      5862 ± 2040 | 600638 ± 26138   |
| Herbaceous   |   379232 ± 16679 |                      0 ± 0 |              1401 ± 750 | 3993 ± 2782   |     52663 ± 8596 |     0 ± 0 |      4220 ± 1810 | 1933898 ± 83595  |

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

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |     Plantation | Terrace | Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :------------ | -------------: | ------: | --------------: | :--------------- |
| Non\_vegetated     |  945900 ± 41589 |                      0 ± 0 |               694 ± 620 | 0 ± 0         | 221690 ± 11309 |   0 ± 0 |   189021 ± 8317 | 772439 ± 33634   |
| Built\_up          |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |          0 ± 0 |   0 ± 0 |           0 ± 0 | 10007 ± 2279     |
| Water              |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |      595 ± 456 |   0 ± 0 |           0 ± 0 | 5158 ± 2741      |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |          0 ± 0 |   0 ± 0 |           0 ± 0 | 227 ± 235        |

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

    ## [1] 16

``` r
unique(rawData$UNDERSTORY_COVER)
```

    ## [1] NA           "Other_Crop"

Sixteen points in other crop exist as understory. Will exclude, as it’s
already included in the agroforestry landuse by default.

## Carbon Analysis

``` r
# calculate Carbon values for areas --------------------------------------------
carbonMono <- c(#"Banana" = 5.7, #from Philippines
                "Coconut" = 48.9, #From Thailand
                "Coffee" = 5.4, # From Vietnam
                "Fruit_Nut" = 67, # from Thailand
                "Oil_Palm" = 38.97, # from Indonesia
                "Pulpwood" = 23, # from Vietnam
                "Rubber" = 31.83, # mean of time average from Vietnam & Thailand
                #"Rice" = 4.2, # From Thailand
                #"Tea" = 15.53, # from Vietnam
                "Other_Crop" = 8.08, # mean from several sources
                "Other_Tree" = 43.28, # mean of trees
                #"Other_Palm" = 22.92, # average of palms
                "Other_Shrub" = 10.46, # mean of shrubs
                "Herbaceous" = 6.82) # same as "other crops"

carbonAF <- c(#"Banana" = 5.7, #from Philippines
              "Coconut" = 56.28, #From Thailand
              "Coffee" = 11, # From Vietnam
              "Fruit_Nut" = 67, # From Thailand
              "Oil_Palm" = 38.97, # from Indonesia
              "Pulpwood" = 23, # from Vietnam
              "Rubber" = 31.83, # mean of time averages from Vietnam & Thailand
              #"Rice" = 1.05, # From Thailand
              #"Tea" = 22, # from Vietnam
              "Other_Crop" = 10.1, # from Thailand
              "Other_Tree" = 43.28, #mean of trees
              #"Other_Palm" = 28.66, # average of palms
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
| Coconut      |        48.9 |         56.3 |
| Coffee       |         5.4 |         11.0 |
| Fruit\_Nut   |        67.0 |         67.0 |
| Oil\_Palm    |        39.0 |         39.0 |
| Pulpwood     |        23.0 |         23.0 |
| Rubber       |        31.8 |         31.8 |
| Other\_Crop  |         8.1 |         10.1 |
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
| Coconut      |   43846 |  45433 |
| Coffee       |       0 |      0 |
| Fruit\_Nut   |  418299 | 253179 |
| Oil\_Palm    | 1126674 | 295226 |
| Pulpwood     |       0 |      0 |
| Rubber       |       0 |      0 |
| Other\_Crop  |  799993 | 126834 |
| Other\_Tree  |  131771 |  44622 |
| Other\_Shrub |   14039 |   9416 |
| Herbaceous   |   47259 |  28339 |

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
ffOPT <- ffO[,4] + ffP[,4] #+ ffT[,4]

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

| Commodity    |      MgC |      SE |
| :----------- | -------: | ------: |
| Coconut      |        0 |       0 |
| Coffee       |     5351 |    5508 |
| Fruit\_Nut   |   308279 |  161722 |
| Oil\_Palm    |  2450702 |  441420 |
| Pulpwood     |  3936219 |  416872 |
| Rubber       | 39451582 | 1653397 |
| Other\_Crop  |   462825 |   82389 |
| Other\_Tree  |  2274503 |  437920 |
| Other\_Shrub |    31347 |   17819 |
| Herbaceous   |    42730 |   19351 |

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
