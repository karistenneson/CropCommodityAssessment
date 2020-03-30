Viet Nam Analysis Script
================
MS Patterson, <tertiarymatt@gmail.com>
August 22, 2019

Set working directory to where data is being stored.

``` r
#setwd("~/R/GIA/")
```

### Required packages

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.2

    ## -- Attaching packages ---------------------------------------------------------------------- tidyverse 1.2.1 --

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

    ## -- Conflicts ------------------------------------------------------------------------- tidyverse_conflicts() --
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
country <- "Viet Nam"

strata <- c(1, 2, 3)

stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799, 
                  "Strata3 Area" = 31059507)

# Metadata to save, and Grouping Variables
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")

groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

# Survey Questions
questions <-  c("LAND_COVER", "UNDERSTORY_PRESENT", "UNDERSTORY_COVER",
                "LAND_USE", "LAND_USE_YEAR_2000", "TCC")

covOrder <- c("Aquaculture", "Bamboo","Coconut","Coffee","Fruit_Nut", 
              "Oil_Palm", "Pulpwood", "Rubber", "Rice", "Tea", "Other_Crop",
              "Other_Palm", "Other_Tree", "Other_Shrub", "Herbaceous", 
              "Non_vegetated", "Built_up", "Water", "Other_LAND_COVER")

useOrder <- c("Agrisiviculture", "Boundary_Agrisilviculture", 
              "Mixed_Agrisilviculture", "Silvopastoral", "Plantation", 
              "Terrace", "Natural_Forest", "Other_LAND_USE")
```

``` r
# Data input and cleaning ------------------------------------------------------
rawData <- read_csv("data/Corrected/Vietnam.csv", col_types = "ddddldcdcdddcdccccc")

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
```

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
| Bamboo             |      16052|          5028|
| Coffee             |      44718|          7671|
| Fruit\_Nut         |      69860|         15179|
| Pulpwood           |     373524|         24514|
| Rubber             |     241710|         20297|
| Rice               |       2093|          1623|
| Tea                |      90341|         68158|
| Other\_Crop        |     343474|        115666|
| Other\_Tree        |    7985101|        845007|
| Other\_Shrub       |     114632|         12941|
| Herbaceous         |     461921|        170506|
| Non\_vegetated     |     220511|        103309|
| Built\_up          |      59808|         32064|
| Water              |     143817|         88811|
| Other\_LAND\_COVER |   22560137|        881641|

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
| Agrisiviculture            |     221156|         19103|
| Boundary\_Agrisilviculture |      17842|          6073|
| Mixed\_Agrisilviculture    |     584717|        249261|
| Silvopastoral              |      31328|          6972|
| Plantation                 |    6554011|        775045|
| Terrace                    |       9726|          4725|
| Natural\_Forest            |     623894|        183926|
| Other\_LAND\_USE           |   24685026|        810195|

``` r
captionQ3 <- paste("Table R3. The estimated  area in each land use type for", 
                   country, "in the year 2000. This data was collected", 
                   "using question 3 in the Collect Earth Online survey,", 
                   "based on Landsat time series imagery.")
kable(use2000Area, digits = 0, caption = captionQ3)
```

|                              |  Area\_ha|  SE\_Area\_ha|
|------------------------------|---------:|-------------:|
| Forest\_Commodity            |   6262852|        774949|
| Natural\_Forest              |   1315461|        254629|
| Other\_LAND\_USE\_YEAR\_2000 |  25149387|        799094|

``` r
captionQ4 <- paste("Table R4. The estimated  area of tree canopy cover in",
                   country, "for the year 2000. This data was collected using",
                   "question 3 in the Collect Earth Online survey,",
                   "based on Landsat time series imagery.")
kable(tcc2000Area, digits = 0, caption = captionQ4)
```

|           |  Area (ha)|  SE Area (ha)|
|-----------|----------:|-------------:|
| Not\_Tree |   25149387|        799094|
| Tree      |    7578313|        799073|

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

| Cover              |  Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral |        Plantation|      Terrace|  Natural\_Forest| Other\_LAND\_USE  |
|:-------------------|----------------:|---------------------------:|------------------------:|:--------------|-----------------:|------------:|----------------:|:------------------|
| Bamboo             |          0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN       |         764 ± 796|      0 ± NaN|     14821 ± 7917| 468 ± 286         |
| Coffee             |     24285 ± 6180|                   186 ± 165|              7570 ± 5639| 0 ± NaN       |           0 ± NaN|      0 ± NaN|          0 ± NaN| 12677 ± 4476      |
| Fruit\_Nut         |      4810 ± 3277|                     0 ± NaN|            55676 ± 36547| 0 ± NaN       |       8618 ± 3773|      0 ± NaN|          0 ± NaN| 757 ± 473         |
| Pulpwood           |        764 ± 795|                     0 ± NaN|                757 ± 729| 0 ± NaN       |    362839 ± 64986|      0 ± NaN|          0 ± NaN| 9165 ± 4254       |
| Rubber             |        810 ± 631|                 3268 ± 3011|              7351 ± 5614| 0 ± NaN       |    227130 ± 42917|  2291 ± 2618|          0 ± NaN| 859 ± 885         |
| Rice               |          0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN       |           0 ± NaN|      0 ± NaN|          0 ± NaN| 2093 ± 1626       |
| Tea                |          0 ± NaN|                 2291 ± 2359|            70130 ± 79969| 0 ± NaN       |           0 ± NaN|  3437 ± 2476|          0 ± NaN| 14483 ± 5297      |
| Other\_Crop        |   148381 ± 25955|                 3437 ± 3207|          159073 ± 149550| 0 ± NaN       |           0 ± NaN|  2291 ± 2921|          0 ± NaN| 30292 ± 7070      |
| Other\_Tree        |      4723 ± 2080|                 1655 ± 1188|            70693 ± 79989| 3154 ± 2029   |  5910164 ± 703492|  1134 ± 1228|  584709 ± 326979| 1408871 ± 416941  |
| Other\_Shrub       |     11454 ± 3930|                   955 ± 871|            16094 ± 14589| 23798 ± 10151 |       6500 ± 3245|    286 ± 286|     11058 ± 5261| 44487 ± 7331      |
| Herbaceous         |      4517 ± 2084|                 5763 ± 3929|          196248 ± 131670| 3804 ± 2781   |      17297 ± 5475|      0 ± NaN|     12214 ± 6213| 222078 ± 101964   |
| Non\_vegetated     |     19025 ± 6108|                   286 ± 327|               1080 ± 870| 573 ± 485     |      20608 ± 6882|    286 ± 365|        950 ± 534| 177702 ± 103352   |
| Built\_up          |          0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN       |           0 ± NaN|      0 ± NaN|          0 ± NaN| 59808 ± 32184     |
| Water              |          0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN       |           93 ± 96|      0 ± NaN|        142 ± 124| 143582 ± 89061    |
| Other\_LAND\_COVER |      2387 ± 2382|                     0 ± NaN|                  47 ± 55| 0 ± NaN       |           0 ± NaN|      0 ± NaN|          0 ± NaN| 22557704 ± 870931 |

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

| Cover              |  Forest\_Commodity|  Natural\_Forest|  Other\_LAND\_USE\_YEAR\_2000|
|:-------------------|------------------:|----------------:|-----------------------------:|
| Bamboo             |          382 ± 398|     15485 ± 6579|                     186 ± 190|
| Coffee             |          764 ± 797|     21806 ± 8152|                  22149 ± 5487|
| Fruit\_Nut         |        5641 ± 3054|     18678 ± 7650|                 45542 ± 14055|
| Pulpwood           |     183050 ± 37275|   149364 ± 44419|                  41110 ± 9533|
| Rubber             |     100014 ± 22656|    63206 ± 20293|                 78490 ± 12884|
| Rice               |            0 ± NaN|        279 ± 295|                   1814 ± 1600|
| Tea                |        2005 ± 2092|    69557 ± 75381|                  18779 ± 6152|
| Other\_Crop        |        6559 ± 3716|    86998 ± 26564|               249917 ± 115649|
| Other\_Tree        |  5920357 ± 1303087|  595568 ± 163932|              1469176 ± 413078|
| Other\_Shrub       |        8377 ± 3565|    70311 ± 20038|                  35944 ± 7716|
| Herbaceous         |       23997 ± 7090|    79044 ± 44300|               358879 ± 167952|
| Non\_vegetated     |        9944 ± 4247|   122460 ± 83650|                 88107 ± 52880|
| Built\_up          |         1097 ± 861|      8047 ± 3017|                 50664 ± 32129|
| Water              |          475 ± 413|      4908 ± 3600|                138434 ± 89000|
| Other\_LAND\_COVER |          191 ± 199|      9750 ± 4380|            22550197 ± 1376132|

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

| Condition          |  Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral |         Plantation|      Terrace|  Natural\_Forest| Other\_LAND\_USE   |
|:-------------------|----------------:|---------------------------:|------------------------:|:--------------|------------------:|------------:|----------------:|:-------------------|
| Bamboo             |          0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN       |          764 ± 805|      0 ± NaN|     14821 ± 8390| 468 ± 353          |
| Built\_up          |          0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN       |            0 ± NaN|      0 ± NaN|          0 ± NaN| 59808 ± 98755      |
| Coffee             |     24285 ± 8026|                   186 ± 156|              7570 ± 3799| 0 ± NaN       |            0 ± NaN|      0 ± NaN|          0 ± NaN| 12677 ± 5412       |
| Fruit\_Nut         |      4810 ± 3548|                     0 ± NaN|              55676 ± NaN| 0 ± NaN       |        8618 ± 4469|      0 ± NaN|          0 ± NaN| 757 ± 526          |
| Herbaceous         |      4517 ± 3096|                 5763 ± 4046|          196248 ± 115893| 3804 ± 3185   |      17297 ± 10178|      0 ± NaN|     12214 ± 7310| 222078 ± 154059    |
| Non\_vegetated     |    19025 ± 13803|                   286 ± 350|               1080 ± 922| 573 ± 580     |      20608 ± 14934|    286 ± 350|        950 ± 725| 177702 ± 156462    |
| Other\_Crop        |   148381 ± 72552|                 3437 ± 3101|          159073 ± 126603| 0 ± NaN       |            0 ± NaN|  2291 ± 2597|          0 ± NaN| 30292 ± 16011      |
| Other\_LAND\_COVER |      2387 ± 2362|                     0 ± NaN|                  47 ± 48| 0 ± NaN       |            0 ± NaN|      0 ± NaN|          0 ± NaN| 22557704 ± 1527560 |
| Other\_Shrub       |     11454 ± 4058|                   955 ± 724|             16094 ± 8511| 23798 ± 7221  |        6500 ± 3270|    286 ± 224|     11058 ± 3115| 44487 ± 7983       |
| Other\_Tree        |      4723 ± 2103|                  1655 ± 882|            70693 ± 55467| 3154 ± 1711   |  5910164 ± 1175711|   1134 ± 852|  584709 ± 203584| 1408871 ± 458514   |
| Pulpwood           |        764 ± 789|                     0 ± NaN|                757 ± 572| 0 ± NaN       |     362839 ± 24197|      0 ± NaN|          0 ± NaN| 9165 ± 4322        |
| Rice               |          0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN       |            0 ± NaN|      0 ± NaN|          0 ± NaN| 2093 ± 2688        |
| Rubber             |        810 ± 627|                 3268 ± 2589|              7351 ± 3561| 0 ± NaN       |     227130 ± 36839|  2291 ± 2101|          0 ± NaN| 859 ± 889          |
| Tea                |          0 ± NaN|                 2291 ± 3213|           70130 ± 101326| 0 ± NaN       |            0 ± NaN|  3437 ± 4458|          0 ± NaN| 14483 ± 16358      |
| Water              |          0 ± NaN|                     0 ± NaN|                  0 ± NaN| 0 ± NaN       |           93 ± 125|      0 ± NaN|        142 ± 165| 143582 ± 148100    |

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

| Cover                      |  Forest\_Commodity|  Natural\_Forest|  Other\_LAND\_USE\_YEAR\_2000|
|:---------------------------|------------------:|----------------:|-----------------------------:|
| Agrisiviculture            |        3217 ± 2505|   121876 ± 36512|                 96062 ± 14112|
| Boundary\_Agrisilviculture |        4583 ± 3423|      2879 ± 1858|                  10380 ± 4842|
| Mixed\_Agrisilviculture    |       13158 ± 5690|  146136 ± 134118|               425423 ± 217167|
| Silvopastoral              |            0 ± NaN|     25856 ± 9572|                   5472 ± 2853|
| Plantation                 |  6181561 ± 1340488|   233275 ± 67366|                139175 ± 17387|
| Terrace                    |        4583 ± 3426|      2387 ± 2449|                   2757 ± 2407|
| Natural\_Forest            |       32214 ± 9712|  556197 ± 169544|                  35482 ± 8048|
| Other\_LAND\_USE           |       23537 ± 7409|  226853 ± 119615|            24434636 ± 1403017|

``` r
# Former Forest only -----------------------------------------------------------
```

Triple conditional for area of commodities that were forested in the year 2000.

``` r
questions2 <- c("LAND_COVER", "LAND_USE", "LAND_USE_YEAR_2000")

crops <- c("Aquaculture", "Coconut", "Coffee", "Fruit_Nut" ,"Pulpwood", "Rice", 
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

| Crops              |  Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral |      Plantation|      Terrace|  Natural\_Forest| Other\_LAND\_USE |
|:-------------------|----------------:|---------------------------:|------------------------:|:--------------|---------------:|------------:|----------------:|:-----------------|
| Aquaculture        |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |           0 ± 0|        0 ± 0|            0 ± 0| 0 ± 0            |
| Coconut            |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |           0 ± 0|        0 ± 0|            0 ± 0| 0 ± 0            |
| Coffee             |     16466 ± 4756|                     47 ± 47|              1697 ± 1229| 0 ± 0         |           0 ± 0|        0 ± 0|            0 ± 0| 3596 ± 2522      |
| Fruit\_Nut         |      2710 ± 2395|                       0 ± 0|             12234 ± 4495| 0 ± 0         |     3454 ± 2512|        0 ± 0|            0 ± 0| 279 ± 242        |
| Oil\_Palm          |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |           0 ± 0|        0 ± 0|            0 ± 0| 0 ± 0            |
| Pulpwood           |        764 ± 786|                       0 ± 0|                279 ± 285| 0 ± 0         |  147844 ± 16635|        0 ± 0|            0 ± 0| 477 ± 491        |
| Rubber             |        810 ± 623|                   977 ± 996|              1623 ± 1669| 0 ± 0         |   58936 ± 10196|        0 ± 0|            0 ± 0| 859 ± 884        |
| Rice               |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |           0 ± 0|        0 ± 0|            0 ± 0| 279 ± 285        |
| Tea                |            0 ± 0|                   286 ± 295|            67838 ± 67838| 0 ± 0         |           0 ± 0|  1432 ± 1473|            0 ± 0| 0 ± 0            |
| Other\_Crop        |    80889 ± 12102|                       0 ± 0|                    0 ± 0| 0 ± 0         |           0 ± 0|        0 ± 0|            0 ± 0| 6109 ± 2387      |
| Other\_Palm        |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |           0 ± 0|        0 ± 0|            0 ± 0| 0 ± 0            |
| Other\_Tree        |      3836 ± 1910|                   140 ± 142|            53319 ± 52188| 2390 ± 1446   |     5795 ± 3021|    668 ± 687|  524956 ± 183554| 4464 ± 1650      |
| Other\_Shrub       |      6353 ± 3079|                       0 ± 0|              7733 ± 5605| 21285 ± 5739  |     1489 ± 1186|    286 ± 219|      6740 ± 1924| 26425 ± 5575     |
| Herbaceous         |      1648 ± 1161|                 1430 ± 1123|                947 ± 744| 1799 ± 1398   |     3073 ± 1290|        0 ± 0|      9685 ± 3389| 60462 ± 36974    |
| Other\_LAND\_COVER |            0 ± 0|                       0 ± 0|                  47 ± 47| 0 ± 0         |           0 ± 0|        0 ± 0|            0 ± 0| 9703 ± 3491      |

``` r
# Crops in TCC in 2000 ---------------------------------------------------------
```

Triple conditional for area of commodities that were had tree cover in the year 2000.

``` r
questions3 <- c("LAND_COVER", "LAND_USE", "TCC")

crops <- c("Aquaculture", "Coconut", "Coffee", "Fruit_Nut" ,"Pulpwood", "Rice", 
           "Rubber", "Oil_Palm", "Tea", "Other_Crop", "Other_Tree", "Other_Palm",
           "Other_Shrub", "Herbaceous")

y_occ2 <- build_yocc(rawData, mtdt = metaNames, qstns = questions3, 
                     cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                     cndtnfld2 = "TCC", covers = crops, 
                     conditions1 = NULL, conditions2 = "Tree")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")
    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
y_occResults2 <- do_yocc_analysis(y_occ2, mtdt = metaNames, strata = strata, 
                                  areas = stratumAreas, ns = sampSize, 
                                  qstns = questions3, grplst = groupList)


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
      caption = c("Table R10. Estimate of area and standard error of each 
      commodity land cover (in hectares), by land use, in areas that had tree 
      cover in the year 2000 and experienced tree canopy cover loss between 
      2001  and 2015. ‘Other Crops’ contains any agricultural or crop land
      covers that could not be identified during the photo interpretation 
      process.  ‘Other Land Use’ contains all other land covers that did not fit
      into the other categories."))
```

| Crops        |  Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral |        Plantation|      Terrace|  Natural\_Forest| Other\_LAND\_USE |
|:-------------|----------------:|---------------------------:|------------------------:|:--------------|-----------------:|------------:|----------------:|:-----------------|
| Aquaculture  |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |             0 ± 0|        0 ± 0|            0 ± 0| 0 ± 0            |
| Coconut      |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |             0 ± 0|        0 ± 0|            0 ± 0| 0 ± 0            |
| Coffee       |     16466 ± 4756|                     47 ± 47|              1697 ± 1229| 0 ± 0         |             0 ± 0|        0 ± 0|            0 ± 0| 4360 ± 2641      |
| Fruit\_Nut   |      4810 ± 3223|                       0 ± 0|             13475 ± 4669| 0 ± 0         |       5754 ± 2896|        0 ± 0|            0 ± 0| 279 ± 242        |
| Oil\_Palm    |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |             0 ± 0|        0 ± 0|            0 ± 0| 0 ± 0            |
| Pulpwood     |        764 ± 786|                       0 ± 0|                757 ± 568| 0 ± 0         |    325166 ± 23400|        0 ± 0|            0 ± 0| 5728 ± 3324      |
| Rubber       |        810 ± 623|                 3268 ± 2559|              7351 ± 3445| 0 ± 0         |    148639 ± 16699|  2291 ± 2083|            0 ± 0| 859 ± 884        |
| Rice         |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |             0 ± 0|        0 ± 0|            0 ± 0| 279 ± 285        |
| Tea          |            0 ± 0|                   286 ± 295|            67838 ± 67838| 0 ± 0         |             0 ± 0|  3437 ± 2532|            0 ± 0| 0 ± 0            |
| Other\_Crop  |    82006 ± 12148|                 2291 ± 2357|                286 ± 295| 0 ± 0         |             0 ± 0|        0 ± 0|            0 ± 0| 8973 ± 3353      |
| Other\_Palm  |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |             0 ± 0|        0 ± 0|            0 ± 0| 0 ± 0            |
| Other\_Tree  |      3836 ± 1910|                   140 ± 142|            53797 ± 52190| 2390 ± 1446   |  5896890 ± 774549|    668 ± 687|  553645 ± 183643| 4559 ± 1653      |
| Other\_Shrub |      6353 ± 3079|                       0 ± 0|             10119 ± 5879| 21285 ± 5739  |       3158 ± 1786|    286 ± 219|      8405 ± 2113| 29081 ± 5968     |
| Herbaceous   |      1648 ± 1161|                 1430 ± 1123|              3317 ± 1424| 1799 ± 1398   |      15196 ± 4523|        0 ± 0|     10973 ± 3462| 68679 ± 37127    |

``` r
# Support TCC in 2000 ----------------------------------------------------------
```

Triple conditional for "support area" of commodities that were had tree cover in the year 2000.

``` r
questions4 <- c("LAND_COVER", "LAND_USE", "TCC")

support <- c("Built_up", "Non_vegetated", "Other_LAND_COVER", "Water")

y_occ3 <- build_yocc(rawData, mtdt = metaNames, qstns = questions4, 
                     cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
                     cndtnfld2 = "TCC", covers = support, 
                     conditions1 = NULL, conditions2 = "Tree")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")
    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
y_occResults3 <- do_yocc_analysis(y_occ3, mtdt = metaNames, strata = strata, 
                                  areas = stratumAreas, ns = sampSize, 
                                  qstns = questions4, grplst = groupList)

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
      caption = c("Table R11. Estimate of area and standard error of each 
      commodity \'support\' land cover (in hectares), by land use, in areas that
      had tree cover in the year 2000 and experienced tree canopy loss between 
      2001  and 2015. ‘Other Crops’ contains any agricultural or crop land
      covers that could not be identified during the photo interpretation 
      process.  ‘Other Land Use’ contains all other land covers that did not fit
      into the other categories."))
```

| Crops              |  Agrisiviculture|  Boundary\_Agrisilviculture|  Mixed\_Agrisilviculture| Silvopastoral |    Plantation|    Terrace|  Natural\_Forest| Other\_LAND\_USE |
|:-------------------|----------------:|---------------------------:|------------------------:|:--------------|-------------:|----------:|----------------:|:-----------------|
| Non\_vegetated     |      8400 ± 3375|                       0 ± 0|                610 ± 470| 382 ± 393     |  19176 ± 5904|  286 ± 295|        521 ± 263| 103029 ± 88821   |
| Built\_up          |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |         0 ± 0|      0 ± 0|            0 ± 0| 9144 ± 2210      |
| Water              |            0 ± 0|                       0 ± 0|                    0 ± 0| 0 ± 0         |       93 ± 95|      0 ± 0|          47 ± 47| 5244 ± 3357      |
| Other\_LAND\_COVER |            0 ± 0|                       0 ± 0|                  47 ± 47| 0 ± 0         |         0 ± 0|      0 ± 0|            0 ± 0| 9894 ± 3496      |

Understory analysis
-------------------

Not necessary in Viet Nam, as no crops were associated with an understory layer in the sample.

``` r
# Understory -------------------------------------------------------------------
```

``` r
# calculate Carbon values for areas --------------------------------------------
carbonMono <- c("Coffee" = 5, 
                "Fruit_Nut" = 34.07, 
                "Pulpwood" = 23, 
                "Rubber" = 31.74,
                "Rice" = 1, 
                "Tea" = 15.3,
                "Other_Crop" = 4,
                "Other_Tree" = 29.5, # mean of trees
                "Other_Shrub" = 10.15, # mean of shrubs
                "Herbaceous" = 4) # same as "other crops"

carbonAF <- c("Coffee" = 11, 
              "Fruit_Nut" = 34.07,
              "Pulpwood" = 23,
              "Rubber" = 31.74,
              "Rice" = 1,
              "Tea" = 22,
              "Other_Crop" = 20,
              "Other_Tree" = 29.5, #mean of trees
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
| Coffee       |          5.0|          11.0|
| Fruit\_Nut   |         34.1|          34.1|
| Pulpwood     |         23.0|          23.0|
| Rubber       |         31.7|          31.7|
| Rice         |          1.0|           1.0|
| Tea          |         15.3|          22.0|
| Other\_Crop  |          4.0|          20.0|
| Other\_Tree  |         29.5|          29.5|
| Other\_Shrub |         10.2|          16.5|
| Herbaceous   |          4.0|          20.0|

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

| Commodity    |      MgC|       SE|
|:-------------|--------:|--------:|
| Coffee       |   200299|    54036|
| Fruit\_Nut   |   509152|   173519|
| Pulpwood     |    23987|    19218|
| Rubber       |   108246|    64791|
| Rice         |        0|        0|
| Tea          |  1498744|  1492457|
| Other\_Crop  |  1617781|   242041|
| Other\_Tree  |  1690210|  1540572|
| Other\_Shrub |   232414|   105516|
| Herbaceous   |    80498|    35571|

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

| Commodity    |      MgC|      SE|
|:-------------|--------:|-------:|
| Coffee       |    17982|   12612|
| Fruit\_Nut   |   127206|   85983|
| Pulpwood     |  3411395|  382774|
| Rubber       |  1897903|  324841|
| Rice         |      279|     285|
| Tea          |    21911|   22537|
| Other\_Crop  |    24436|    9548|
| Other\_Tree  |   190660|   91405|
| Other\_Shrub |    18019|   12239|
| Herbaceous   |    12293|    5160|

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
