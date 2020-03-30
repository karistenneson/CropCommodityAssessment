Philippines Analysis Script
================
MS Patterson, <tertiarymatt@gmail.com>
August 29, 2019

Set working directory to where data is being stored.

``` r
#setwd("~/R/GIA/")
```

### Required packages

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.2

    ## -- Attaching packages -------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.3.1     v forcats 0.3.0

    ## Warning: package 'ggplot2' was built under R version 3.5.2

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## Warning: package 'tidyr' was built under R version 3.5.2

    ## Warning: package 'readr' was built under R version 3.5.2

    ## Warning: package 'purrr' was built under R version 3.5.2

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## Warning: package 'stringr' was built under R version 3.5.2

    ## Warning: package 'forcats' was built under R version 3.5.2

    ## -- Conflicts ----------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 3.5.2

``` r
library(rmarkdown)
```

    ## Warning: package 'rmarkdown' was built under R version 3.5.2

``` r
source("00_GIA_functions.R")
```

``` r
# User needs to supply variables -----------------------------------------------

# Area of strata and sample sizes
country <- "Philippines"

strata <- c(1, 2, 3)

stratumAreas <- c("Strata1 Area" = 166007, "Strata2 Area" = 686867, 
                  "Strata3 Area" = 29147220)        

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
              "Mixed_Agrisilviculture", "Silvopastoral", "Plantation", 
              "Terrace", "Natural_Forest", "Other_LAND_USE")
```

``` r
# Data input and cleaning ------------------------------------------------------
rawData <- read_csv("data/Corrected/Philippines.csv", 
                    col_types = "ddddldcdcdddcdccccc")

# Add a tree canopy cover variable
rawData <- rawData %>% 
  mutate(TCC = case_when( 
    LAND_USE_YEAR_2000 == "Forest_Commodity" ~ "Tree",
    LAND_USE_YEAR_2000 == "Natural_Forest" ~ "Tree",
    LAND_USE_YEAR_2000 == "Other_LAND_USE_YEAR_2000" ~ "Not_Tree"
  )
  )
```

    ## Warning: package 'bindrcpp' was built under R version 3.5.2

``` r
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

    ## [1] "There are 234 samples in stratum 1, 554 samples in stratum 2, and 257 samples in stratum 3"

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

| cover              |  Area (ha)|  SE Area (ha)|
|:-------------------|----------:|-------------:|
| Aquaculture        |        228|           236|
| Banana             |     234130|        160100|
| Bamboo             |      47695|         25423|
| Coconut            |    1541680|        378457|
| Coffee             |     127820|        113784|
| Fruit\_Nut         |     162737|        117609|
| Oil\_Palm          |    1108926|        318469|
| Pulpwood           |    1904991|        427346|
| Rubber             |     565222|        249555|
| Rice               |      10793|          3251|
| Tea                |      86723|         85070|
| Other\_Crop        |   10950937|        821122|
| Other\_Palm        |       6788|          1912|
| Other\_Tree        |    6683005|        654381|
| Other\_Shrub       |    4282716|        565003|
| Herbaceous         |     593957|        142903|
| Non\_vegetated     |     869383|        265968|
| Built\_up          |     353841|        102040|
| Water              |       5527|          4736|
| Other\_LAND\_COVER |     462994|        161717|

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

| use                        |  Area (ha)|  SE Area (ha)|
|:---------------------------|----------:|-------------:|
| Agrisiviculture            |    2248696|        430385|
| Boundary\_Agrisilviculture |     530104|        229507|
| Mixed\_Agrisilviculture    |    5539425|        692275|
| Silvopastoral              |     332552|        175699|
| Plantation                 |    5431961|        682574|
| Terrace                    |     113413|        113413|
| Natural\_Forest            |    5683895|        666539|
| Other\_LAND\_USE           |   10120047|        781982|

``` r
captionQ3 <- paste("Table R3. The estimated  area in each land use type for", 
                   country, "in the year 2000. This data was collected", 
                   "using question 3 in the Collect Earth Online survey,", 
                   "based on Landsat time series imagery.")
kable(use2000Area, digits = 0, caption = captionQ3)
```

|                              |  Area\_ha|  SE\_Area\_ha|
|------------------------------|---------:|-------------:|
| Forest\_Commodity            |   3051307|        549430|
| Natural\_Forest              |  23424943|        746896|
| Other\_LAND\_USE\_YEAR\_2000 |   3523844|        576603|

``` r
captionQ4 <- paste("Table R4. The estimated  area of tree canopy cover in",
                   country, "for the year 2000. This data was collected using",
                   "question 3 in the Collect Earth Online survey,",
                   "based on Landsat time series imagery.")
kable(tcc2000Area, digits = 0, caption = captionQ4)
```

|           |  Area (ha)|  SE Area (ha)|
|-----------|----------:|-------------:|
| Not\_Tree |    3523844|        576603|
| Tree      |   26476250|        576591|

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

| Cover              |   Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral   |        Plantation|          Terrace|   Natural\_Forest| Other\_LAND\_USE |
|:-------------------|-----------------:|---------------------------:|------------------------:|:----------------|-----------------:|----------------:|-----------------:|:-----------------|
| Aquaculture        |           0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 228 ± 238        |
| Banana             |         756 ± 801|                   804 ± 818|              1310 ± 1265| 0 ± NaN         |   230908 ± 165383|          0 ± NaN|           0 ± NaN| 353 ± 364        |
| Bamboo             |         252 ± 267|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |           0 ± NaN|          0 ± NaN|     46746 ± 26683| 697 ± 545        |
| Coconut            |       2545 ± 1268|                   705 ± 761|          677871 ± 286476| 202 ± 256       |   860055 ± 322532|          0 ± NaN|           0 ± NaN| 302 ± 233        |
| Coffee             |        1343 ± 872|                     0 ± NaN|          124527 ± 113657| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 1950 ± 1302      |
| Fruit\_Nut         |     16041 ± 15030|                   453 ± 542|          144882 ± 117216| 0 ± NaN         |       1360 ± 1031|          0 ± NaN|           0 ± NaN| 0 ± NaN          |
| Oil\_Palm          |   151730 ± 126649|                     0 ± NaN|          739504 ± 305077| 0 ± NaN         |   217138 ± 140695|          0 ± NaN|           0 ± NaN| 554 ± 351        |
| Pulpwood           |           0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |  1791492 ± 523592|          0 ± NaN|           0 ± NaN| 113499 ± 114104  |
| Rubber             |           0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |   451506 ± 237242|  113413 ± 113413|           0 ± NaN| 302 ± 312        |
| Rice               |           0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 10793 ± 3458     |
| Tea                |           0 ± NaN|               85564 ± 99961|                  0 ± NaN| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 1159 ± 1195      |
| Other\_Crop        |  1926222 ± 710005|             177737 ± 157604|         3504570 ± 928362| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 5342408 ± 627402 |
| Other\_Palm        |         453 ± 481|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |         867 ± 673|          0 ± NaN|       3943 ± 1654| 1525 ± 844       |
| Other\_Tree        |     90435 ± 63679|             156815 ± 132405|          172967 ± 103914| 80536 ± 104306  |  1761269 ± 406717|          0 ± NaN|  4381449 ± 984927| 39535 ± 11161    |
| Other\_Shrub       |     47501 ± 44878|               35322 ± 34727|              9195 ± 5338| 217376 ± 215030 |       8342 ± 5019|          0 ± NaN|  1126958 ± 352751| 2838023 ± 611648 |
| Herbaceous         |       4320 ± 1645|                 5236 ± 3726|           152665 ± 94287| 34439 ± 43623   |     65092 ± 40423|          0 ± NaN|    104691 ± 53215| 227514 ± 85058   |
| Non\_vegetated     |       6947 ± 5244|               62742 ± 74558|             11429 ± 9758| 0 ± NaN         |     43932 ± 35458|          0 ± NaN|     20109 ± 11708| 724223 ± 270732  |
| Built\_up          |           0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 353841 ± 109340  |
| Water              |         101 ± 107|                 4726 ± 5544|                  50 ± 52| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 650 ± 299        |
| Other\_LAND\_COVER |           50 ± 53|                     0 ± NaN|                453 ± 424| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 462491 ± 162254  |

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

| Cover              |  Forest\_Commodity|   Natural\_Forest|  Other\_LAND\_USE\_YEAR\_2000|
|:-------------------|------------------:|-----------------:|-----------------------------:|
| Aquaculture        |            0 ± NaN|           0 ± NaN|                     228 ± 242|
| Banana             |    117192 ± 113428|   114975 ± 113596|                   1963 ± 1156|
| Bamboo             |          151 ± 160|     36690 ± 23723|                  10853 ± 9844|
| Coconut            |    136466 ± 105531|  1398870 ± 373303|                   6344 ± 2596|
| Coffee             |          857 ± 905|   114506 ± 113591|                 12457 ± 10005|
| Fruit\_Nut         |         1109 ± 721|   158706 ± 117921|                   2922 ± 1456|
| Oil\_Palm          |    214172 ± 153411|   859864 ± 289838|                 34890 ± 34923|
| Pulpwood           |   1689262 ± 610169|   120311 ± 113642|                 95418 ± 94362|
| Rubber             |    341096 ± 216373|   115437 ± 113605|               108688 ± 108486|
| Rice               |            0 ± NaN|         585 ± 460|                  10208 ± 3987|
| Tea                |            0 ± NaN|       1159 ± 1189|                 85564 ± 84916|
| Other\_Crop        |            0 ± NaN|  9843985 ± 805562|              1106952 ± 479478|
| Other\_Palm        |         1297 ± 757|       3516 ± 1480|                   1976 ± 1091|
| Other\_Tree        |    373850 ± 212699|  5354626 ± 687679|               954529 ± 248797|
| Other\_Shrub       |       16906 ± 8778|  3681009 ± 576112|               584801 ± 211551|
| Herbaceous         |     100086 ± 64635|    176101 ± 65189|               317771 ± 154713|
| Non\_vegetated     |      58814 ± 42402|   660616 ± 252504|                149952 ± 94256|
| Built\_up          |            50 ± 53|   350654 ± 103825|                   3137 ± 1441|
| Water              |            0 ± NaN|           0 ± NaN|                   5527 ± 4751|
| Other\_LAND\_COVER |            0 ± NaN|   433331 ± 159482|                 29663 ± 29232|

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

| Condition          |   Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral   |        Plantation|          Terrace|   Natural\_Forest| Other\_LAND\_USE |
|:-------------------|-----------------:|---------------------------:|------------------------:|:----------------|-----------------:|----------------:|-----------------:|:-----------------|
| Aquaculture        |           0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 228 ± 457        |
| Bamboo             |         252 ± 321|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |           0 ± NaN|          0 ± NaN|     46746 ± 45361| 697 ± 755        |
| Banana             |        756 ± 1065|                  804 ± 1014|              1310 ± 1776| 0 ± NaN         |   230908 ± 274758|          0 ± NaN|           0 ± NaN| 353 ± 497        |
| Built\_up          |           0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 353841 ± 202518  |
| Coconut            |       2545 ± 1383|                   705 ± 674|          677871 ± 350789| 202 ± 218       |   860055 ± 428140|          0 ± NaN|           0 ± NaN| 302 ± 254        |
| Coffee             |       1343 ± 1867|                     0 ± NaN|          124527 ± 110902| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 1950 ± 2772      |
| Fruit\_Nut         |     16041 ± 21715|                   453 ± 657|          144882 ± 188921| 0 ± NaN         |       1360 ± 1716|          0 ± NaN|           0 ± NaN| 0 ± NaN          |
| Herbaceous         |       4320 ± 1870|                 5236 ± 2601|          152665 ± 106748| 34439 ± 35504   |     65092 ± 46440|          0 ± NaN|    104691 ± 61938| 227514 ± 137105  |
| Non\_vegetated     |       6947 ± 5691|               62742 ± 67256|            11429 ± 10720| 0 ± NaN         |     43932 ± 40149|          0 ± NaN|     20109 ± 14094| 724223 ± 406867  |
| Oil\_Palm          |   151730 ± 133184|                     0 ± NaN|          739504 ± 403263| 0 ± NaN         |   217138 ± 164431|          0 ± NaN|           0 ± NaN| 554 ± 413        |
| Other\_Crop        |  1926222 ± 448627|             177737 ± 104378|         3504570 ± 681480| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 5342408 ± 846695 |
| Other\_LAND\_COVER |           50 ± 57|                     0 ± NaN|                453 ± 473| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 462491 ± 161542  |
| Other\_Palm        |         453 ± 499|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |         867 ± 778|          0 ± NaN|       3943 ± 2211| 1525 ± 1014      |
| Other\_Shrub       |     47501 ± 43476|               35322 ± 26335|              9195 ± 5337| 217376 ± 140865 |       8342 ± 5220|          0 ± NaN|  1126958 ± 356800| 2838023 ± 728900 |
| Other\_Tree        |     90435 ± 58985|              156815 ± 83506|          172967 ± 101137| 80536 ± 81105   |  1761269 ± 388442|          0 ± NaN|  4381449 ± 840890| 39535 ± 11541    |
| Pulpwood           |           0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |  1791492 ± 759208|          0 ± NaN|           0 ± NaN| 113499 ± 119094  |
| Rice               |           0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 10793 ± 5650     |
| Rubber             |           0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN         |   451506 ± 382633|  113413 ± 133709|           0 ± NaN| 302 ± 363        |
| Tea                |           0 ± NaN|              85564 ± 149346|                  0 ± NaN| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 1159 ± 1999      |
| Water              |         101 ± 160|                4726 ± 10112|                  50 ± 81| 0 ± NaN         |           0 ± NaN|          0 ± NaN|           0 ± NaN| 650 ± 838        |

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

| Cover                      |  Forest\_Commodity|   Natural\_Forest|  Other\_LAND\_USE\_YEAR\_2000|
|:---------------------------|------------------:|-----------------:|-----------------------------:|
| Agrisiviculture            |        3404 ± 2100|  2194379 ± 446238|                 50913 ± 35524|
| Boundary\_Agrisilviculture |        3326 ± 2153|     60481 ± 44940|               466298 ± 252762|
| Mixed\_Agrisilviculture    |      57903 ± 56688|  4770694 ± 704486|               710829 ± 356797|
| Silvopastoral              |        1209 ± 1278|   330789 ± 176669|                     554 ± 582|
| Plantation                 |  2547698 ± 1021726|  2645965 ± 516297|               238299 ± 159933|
| Terrace                    |    113413 ± 117499|           0 ± NaN|                       0 ± NaN|
| Natural\_Forest            |    179518 ± 141342|  4735665 ± 630475|               768712 ± 353838|
| Other\_LAND\_USE           |    144837 ± 126221|  8686970 ± 760879|              1288239 ± 521711|

``` r
# Former Forest only -----------------------------------------------------------
```

Triple conditional for area of commodities that were forested in the year 2000.

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

| Crops              |   Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral   |        Plantation|  Terrace|   Natural\_Forest| Other\_LAND\_USE |
|:-------------------|-----------------:|---------------------------:|------------------------:|:----------------|-----------------:|--------:|-----------------:|:-----------------|
| Banana             |             0 ± 0|                       0 ± 0|              1209 ± 1240| 0 ± 0           |   113413 ± 113413|    0 ± 0|             0 ± 0| 353 ± 362        |
| Coconut            |        1334 ± 711|                       0 ± 0|          662542 ± 259347| 0 ± 0           |   734994 ± 264399|    0 ± 0|             0 ± 0| 0 ± 0            |
| Coffee             |             0 ± 0|                       0 ± 0|          113413 ± 113413| 0 ± 0           |             0 ± 0|    0 ± 0|             0 ± 0| 1093 ± 940       |
| Fruit\_Nut         |     15386 ± 14231|                       0 ± 0|          143219 ± 116803| 0 ± 0           |         101 ± 103|    0 ± 0|             0 ± 0| 0 ± 0            |
| Oil\_Palm          |   117041 ± 113434|                       0 ± 0|          739504 ± 267983| 0 ± 0           |       3218 ± 1636|    0 ± 0|             0 ± 0| 101 ± 103        |
| Pulpwood           |             0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0           |   120311 ± 113446|    0 ± 0|             0 ± 0| 0 ± 0            |
| Rubber             |             0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0           |   115135 ± 113421|    0 ± 0|             0 ± 0| 302 ± 310        |
| Rice               |             0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0           |             0 ± 0|    0 ± 0|             0 ± 0| 585 ± 459        |
| Tea                |             0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0           |             0 ± 0|    0 ± 0|             0 ± 0| 1159 ± 1188      |
| Other\_Crop        |  1922023 ± 399287|               43538 ± 42543|         3037287 ± 533651| 0 ± 0           |             0 ± 0|    0 ± 0|             0 ± 0| 4841137 ± 602637 |
| Other\_Palm        |         453 ± 465|                       0 ± 0|                    0 ± 0| 0 ± 0           |         171 ± 177|    0 ± 0|       2892 ± 1379| 0 ± 0            |
| Other\_Tree        |     86588 ± 57621|               15386 ± 14195|            70570 ± 53862| 80334 ± 80334   |  1557641 ± 378757|    0 ± 0|  3539461 ± 522804| 4646 ± 2088      |
| Other\_Shrub       |     44294 ± 42541|                   202 ± 207|               1159 ± 794| 217376 ± 134901 |         199 ± 180|    0 ± 0|  1074705 ± 283221| 2343075 ± 459997 |
| Herbaceous         |        1573 ± 653|                   802 ± 585|                883 ± 589| 33079 ± 33079   |           85 ± 66|    0 ± 0|     76136 ± 45472| 63544 ± 33466    |
| Other\_LAND\_COVER |           50 ± 52|                       0 ± 0|                453 ± 416| 0 ± 0           |             0 ± 0|    0 ± 0|             0 ± 0| 432827 ± 159506  |

``` r
# Support FF in 2000 ----------------------------------------------------------
```

Triple conditional for "support area" of commodities that were had forest cover in the year 2000.

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

| Crops              |  Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral |     Plantation|  Terrace|  Natural\_Forest| Other\_LAND\_USE |
|:-------------------|----------------:|---------------------------:|------------------------:|:--------------|--------------:|--------:|----------------:|:-----------------|
| Non\_vegetated     |      6040 ± 4798|                  1058 ± 767|                453 ± 465| 0 ± 0         |  43932 ± 34376|    0 ± 0|      5832 ± 4775| 662115 ± 250640  |
| Built\_up          |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |          0 ± 0|    0 ± 0|            0 ± 0| 350705 ± 102032  |
| Water              |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |          0 ± 0|    0 ± 0|            0 ± 0| 0 ± 0            |
| Other\_LAND\_COVER |          50 ± 52|                       0 ± 0|                453 ± 416| 0 ± 0         |          0 ± 0|    0 ± 0|            0 ± 0| 432827 ± 159506  |

``` r
# Crops in TCC in 2000 ---------------------------------------------------------
```

Triple conditional for area of commodities that were had tree cover in the year 2000.

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

| Crops        |   Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral   |        Plantation|          Terrace|   Natural\_Forest| Other\_LAND\_USE |
|:-------------|-----------------:|---------------------------:|------------------------:|:----------------|-----------------:|----------------:|-----------------:|:-----------------|
| Banana       |             0 ± 0|                       0 ± 0|              1209 ± 1240| 0 ± 0           |   230606 ± 160091|            0 ± 0|             0 ± 0| 353 ± 362        |
| Coconut      |       2394 ± 1053|                       0 ± 0|          677231 ± 259595| 202 ± 207       |   855308 ± 283092|            0 ± 0|             0 ± 0| 202 ± 207        |
| Coffee       |             0 ± 0|                       0 ± 0|          113413 ± 113413| 0 ± 0           |             0 ± 0|            0 ± 0|             0 ± 0| 1950 ± 1285      |
| Fruit\_Nut   |     15688 ± 14234|                   453 ± 465|          143219 ± 116803| 0 ± 0           |         453 ± 376|            0 ± 0|             0 ± 0| 0 ± 0            |
| Oil\_Palm    |   117243 ± 113434|                       0 ± 0|          739504 ± 267983| 0 ± 0           |   216936 ± 133990|            0 ± 0|             0 ± 0| 353 ± 278        |
| Pulpwood     |             0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0           |  1696074 ± 404465|            0 ± 0|             0 ± 0| 113499 ± 113413  |
| Rubber       |             0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0           |   342818 ± 195675|  113413 ± 113413|             0 ± 0| 302 ± 310        |
| Rice         |             0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0           |             0 ± 0|            0 ± 0|             0 ± 0| 585 ± 459        |
| Tea          |             0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0           |             0 ± 0|            0 ± 0|             0 ± 0| 1159 ± 1188      |
| Other\_Crop  |  1922023 ± 399287|               43538 ± 42543|         3037287 ± 533651| 0 ± 0           |             0 ± 0|            0 ± 0|             0 ± 0| 4841137 ± 602637 |
| Other\_Palm  |         453 ± 465|                       0 ± 0|                    0 ± 0| 0 ± 0           |         313 ± 325|            0 ± 0|       3744 ± 1505| 302 ± 263        |
| Other\_Tree  |     87162 ± 57622|               16091 ± 14214|            70850 ± 53863| 80334 ± 80334   |  1755031 ± 400465|            0 ± 0|  3712965 ± 533755| 6042 ± 2212      |
| Other\_Shrub |     44445 ± 42541|                  1310 ± 879|               1159 ± 794| 217376 ± 134901 |       8040 ± 4932|            0 ± 0|  1079716 ± 283191| 2345870 ± 459998 |
| Herbaceous   |        2285 ± 828|                  1356 ± 732|            43816 ± 42536| 34087 ± 33095   |     44150 ± 33450|            0 ± 0|     76136 ± 45472| 74357 ± 34751    |

``` r
# Support TCC in 2000 ----------------------------------------------------------
```

Triple conditional for "support area" of commodities that were had tree cover in the year 2000.

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

| Crops              |  Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral |     Plantation|  Terrace|  Natural\_Forest| Other\_LAND\_USE |
|:-------------------|----------------:|---------------------------:|------------------------:|:--------------|--------------:|--------:|----------------:|:-----------------|
| Non\_vegetated     |      6040 ± 4798|                  1058 ± 767|                453 ± 465| 0 ± 0         |  43932 ± 34376|    0 ± 0|      5832 ± 4775| 662115 ± 250640  |
| Built\_up          |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |          0 ± 0|    0 ± 0|            0 ± 0| 350705 ± 102032  |
| Water              |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |          0 ± 0|    0 ± 0|            0 ± 0| 0 ± 0            |
| Other\_LAND\_COVER |          50 ± 52|                       0 ± 0|                453 ± 416| 0 ± 0         |          0 ± 0|    0 ± 0|            0 ± 0| 432827 ± 159506  |

Understory analysis
-------------------

Investigate presence of understory

``` r
# Understory -------------------------------------------------------------------
sum(rawData$UNDERSTORY_PRESENT == "Yes")
```

    ## [1] 111

``` r
unique(rawData$UNDERSTORY_COVER)
```

    ## [1] NA           "Other_Crop" "Coffee"

111 points in other crops and coffee exist as understory. Will exclude, from carbon numbers, as it's already included in the agroforestry landuse by default. But maybe should mention the area.

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
    ## 1 Oil_Palm   252 ± 258   0 ± 0     
    ## 2 Other_Crop 0 ± 0       1069 ± 736
    ## 3 Other_Palm 0 ± 0       0 ± 0     
    ## 4 Other_Tree 1713 ± 1206 221 ± 135 
    ## 5 Rubber     0 ± 0       0 ± 0

Carbon Analysis
---------------

``` r
# calculate Carbon values for areas --------------------------------------------
carbonMono <- c("Banana" = 5.7, #from Philippines
                "Coconut" = 24.1, #From Philippines
                "Coffee" = 5.4, # From Vietnam
                "Fruit_Nut" = 43.81, # mean of rambutan, mango, santol
                "Pulpwood" = 23, # from Vietnam
                "Rubber" = 107.3, # mean of time averaged 
                "Oil_Palm" = 38.97, # from Indonesia
                "Rice" = 3.1, # From Philippines
                "Tea" = 15.53, # from Vietnam
                "Other_Crop" = 5.14, # Various crops in Philippines
                "Other_Tree" = 49.55, # mean of trees
                "Other_Palm" = 22.92, # average of palms
                "Other_Shrub" = 10.46, # mean of shrubs
                "Herbaceous" = 6.52) # Philippines grasslands

carbonAF <- c("Banana" = 5.7, #from Philippines
              "Coffee" = 30.8, # From Vietnam
              "Coconut" = 41.3, #From Philippines
              "Fruit_Nut" = 44.33, # mean of rambutan, mango, santol
              "Pulpwood" = 23, # from Vietnam
              "Rubber" = 107.3, # mean of time averaged
              "Oil_Palm" = 38.97, # from Indonesia
              "Rice" = 3.1, # From Philippines
              "Tea" = 22, # from Vietnam
              "Other_Crop" = 20, # from Vietnam
              "Other_Tree" = 49.68, #mean of trees
              "Other_Palm" = 28.66, # average of palms
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

|              |  Monoculture|  Agroforestry|
|--------------|------------:|-------------:|
| Banana       |          5.7|           5.7|
| Coconut      |         24.1|          30.8|
| Coffee       |          5.4|          41.3|
| Fruit\_Nut   |         43.8|          44.3|
| Pulpwood     |         23.0|          23.0|
| Rubber       |        107.3|         107.3|
| Oil\_Palm    |         39.0|          39.0|
| Rice         |          3.1|           3.1|
| Tea          |         15.5|          22.0|
| Other\_Crop  |          5.1|          20.0|
| Other\_Tree  |         49.5|          49.7|
| Other\_Palm  |         22.9|          28.7|
| Other\_Shrub |         10.5|          16.5|
| Herbaceous   |          6.5|          20.0|

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
print(ffAF)
```

    ## # A tibble: 15 x 3
    ##    commodity         Area_ha   SE_ha
    ##    <chr>               <dbl>   <dbl>
    ##  1 Banana              1209.   1240.
    ##  2 Coconut           663876. 259347.
    ##  3 Coffee            113413. 113413.
    ##  4 Fruit_Nut         158605. 117667.
    ##  5 Herbaceous          3257.   1056.
    ##  6 Oil_Palm          856545. 291002.
    ##  7 Other_Crop       5002848. 667849.
    ##  8 Other_LAND_COVER     504.    420.
    ##  9 Other_Palm           453.    465.
    ## 10 Other_Shrub        45654.  42549.
    ## 11 Other_Tree        172544.  80142.
    ## 12 Pulpwood               0       0 
    ## 13 Rice                   0       0 
    ## 14 Rubber                 0       0 
    ## 15 Tea                    0       0

``` r
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

| Commodity    |        MgC|        SE|
|:-------------|----------:|---------:|
| Banana       |       6893|      7067|
| Coffee       |    3493130|   3493130|
| Coconut      |   27418093|  10711051|
| Fruit\_Nut   |    7030957|   5216165|
| Pulpwood     |          0|         0|
| Rubber       |          0|         0|
| Oil\_Palm    |   33379574|  11340340|
| Rice         |          0|         0|
| Tea          |          0|         0|
| Other\_Crop  |  100056953|  13356988|
| Other\_Tree  |    8571986|   3981465|
| Other\_Palm  |      12997|     13325|
| Other\_Shrub |     753290|    702056|
| Herbaceous   |      65142|     21124|

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
                     Tr = pull(ffT[,5])^2) %>%
  rowwise() %>%
  transmute(SE_ha = sum(O, P, Tr, na.rm = T)) %>%
  sqrt()

# Assemble names, areas, SE
ffMono <- bind_cols(commodity = ffO$cover, Area_ha = pull(ffOPT), SE_ha = ffOPTSE)
print(ffMono)
```

    ## # A tibble: 15 x 3
    ##    commodity          Area_ha    SE_ha
    ##    <chr>                <dbl>    <dbl>
    ##  1 Banana            113766.  113414. 
    ##  2 Coconut           734994.  264399. 
    ##  3 Coffee              1093.     940. 
    ##  4 Fruit_Nut            101.     103. 
    ##  5 Herbaceous            85.4     66.0
    ##  6 Oil_Palm            3318.    1639. 
    ##  7 Other_Crop       4841137.  602637. 
    ##  8 Other_LAND_COVER  432827.  159506. 
    ##  9 Other_Palm           171.     177. 
    ## 10 Other_Shrub          199.     180. 
    ## 11 Other_Tree       1557641.  378757. 
    ## 12 Pulpwood          120311.  113446. 
    ## 13 Rice                 585.     459. 
    ## 14 Rubber            115437.  113422. 
    ## 15 Tea                 1159.    1188.

``` r
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

| Commodity    |       MgC|        SE|
|:-------------|---------:|---------:|
| Banana       |    648466|    646459|
| Coconut      |  17713354|   6372022|
| Coffee       |      5903|      5074|
| Fruit\_Nut   |      4415|      4526|
| Pulpwood     |   2767160|   2609263|
| Rubber       |  12386419|  12170154|
| Oil\_Palm    |    129314|     63886|
| Rice         |      1812|      1422|
| Tea          |     17998|     18452|
| Other\_Crop  |  24883446|   3097554|
| Other\_Tree  |  77181104|  18767403|
| Other\_Palm  |      3914|      4065|
| Other\_Shrub |      2084|      1879|
| Herbaceous   |       557|       430|

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
