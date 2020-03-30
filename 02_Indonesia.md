Indonesia Analysis Script
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
country <- c("Indonesia")
strata <- c(1, 2, 3)
stratumAreas <- c("Strata1 Area" = 8324441, "Strata2 Area" =    12512668, 
                  "Strata3 Area" = 169619791)

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
dataPath <- c("data/Indonesia")
files <- dir(dataPath)

rawData <- files %>% 
  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))


# clean data
rawData <- clean_data(rawData, c(15:19))

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
              length(unique(rawData[which(rawData$PL_STRATUM == 3),]$PL_PLOTID))
              )
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
colnames(coverArea) <- c("Area (ha)", "SE Area (ha)")

useArea <- t(generalResults$Cover$LAND_USE) * sum(stratumAreas)
colnames(useArea) <- c("Area (ha)", "SE Area (ha)")

use2000Area <- t(generalResults$Cover$LAND_USE_YEAR_2000) * sum(stratumAreas)
colnames(use2000Area) <- c("Area (ha)", "SE Area (ha)")

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
| Aquaculture        |    315632 |       290251 |
| Bamboo             |    455910 |       246446 |
| Coconut            |   1463257 |       622653 |
| Coffee             |    162571 |        42577 |
| Fruit\_Nut         |   1155259 |       617645 |
| Oil\_Palm          |  12328456 |      1851024 |
| Pulpwood           |   1554441 |       622007 |
| Rubber             |    332855 |        71632 |
| Rice               |   4638154 |      1257930 |
| Tea                |     43446 |        26082 |
| Other\_Crop        |  13511706 |      1924360 |
| Other\_Palm        |    238952 |        58921 |
| Other\_Tree        | 117042437 |      3783960 |
| Other\_Shrub       |  15684879 |      1915370 |
| Herbaceous         |   2752372 |       640616 |
| Non\_vegetated     |   4301615 |      1154890 |
| Built\_up          |   6864414 |      1387440 |
| Water              |   4762921 |      1324713 |
| Other\_LAND\_COVER |   2847622 |       764917 |

Table R1. The estimated area in each land cover type for Indonesia in
the period after 2015. This data was collected using question 1 in the
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
| Agrisiviculture            |  10735942 |      1761606 |
| Boundary\_Agrisilviculture |    718593 |       206232 |
| Mixed\_Agrisilviculture    |  11610015 |      1843341 |
| Silvopastoral              |    747608 |       446114 |
| Plantation                 |  16087259 |      2121506 |
| Terrace                    |     37840 |        24169 |
| Natural\_Forest            | 111903077 |      4024735 |
| Other\_LAND\_USE           |  38616566 |      3106350 |

Table R2. The estimated area in each land use type for Indonesia in the
period after 2015. This data was collected using question 2 in the
Collect Earth Online survey, based on Digital Globe and Bing imagery.

``` r
captionQ3 <- paste("Table R3. The estimated  area in each land use type for", 
                   country, "in the year 2000. This data was collected", 
                   "using question 3 in the Collect Earth Online survey,", 
                   "based on Landsat time series imagery.")
kable(use2000Area, digits = 0, caption = captionQ3)
```

|                              | Area (ha) | SE Area (ha) |
| ---------------------------- | --------: | -----------: |
| Forest\_Commodity            |   1133807 |       135968 |
| Natural\_Forest              | 160824149 |      2738086 |
| Other\_LAND\_USE\_YEAR\_2000 |  28498945 |      3023671 |

Table R3. The estimated area in each land use type for Indonesia in the
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
| Not\_Tree |  28498945 |      3023671 |
| Tree      | 161957955 |      2735793 |

Table R4. The estimated area of tree canopy cover in Indonesia for the
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

| Cover              |   Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral   |        Plantation |       Terrace |     Natural\_Forest | Other\_LAND\_USE   |
| :----------------- | ----------------: | -------------------------: | ----------------------: | :-------------- | ----------------: | ------------: | ------------------: | :----------------- |
| Aquaculture        |     16818 ± 17718 |                    0 ± NaN |                 0 ± NaN | 0 ± NaN         |           0 ± NaN |       0 ± NaN |             0 ± NaN | 298814 ± 291894    |
| Bamboo             |           0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN         |           0 ± NaN |       0 ± NaN |     434888 ± 247019 | 21022 ± 16928      |
| Coconut            |     11912 ± 12642 |                4204 ± 3539 |         885970 ± 558729 | 0 ± NaN         |   561171 ± 358651 |       0 ± NaN |             0 ± NaN | 0 ± NaN            |
| Coffee             |     67972 ± 32023 |                1402 ± 1522 |           61665 ± 29046 | 0 ± NaN         |     25927 ± 19686 |       0 ± NaN |           701 ± 727 | 4905 ± 5110        |
| Fruit\_Nut         |   265842 ± 160580 |                7708 ± 8548 |         857184 ± 588242 | 0 ± NaN         |     23824 ± 18597 |       0 ± NaN |           701 ± 727 | 0 ± NaN            |
| Oil\_Palm          | 2009251 ± 1004801 |              12614 ± 14028 |       4309321 ± 1605624 | 0 ± NaN         | 5979752 ± 2050218 |       0 ± NaN |           701 ± 722 | 16818 ± 17414      |
| Pulpwood           |           0 ± NaN |                    0 ± NaN |               701 ± 738 | 0 ± NaN         |  1553741 ± 686388 |       0 ± NaN |             0 ± NaN | 0 ± NaN            |
| Rubber             |           0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN         |    322344 ± 92887 |       0 ± NaN |         2102 ± 2166 | 8409 ± 8706        |
| Rice               |   951896 ± 617101 |              15416 ± 17147 |                 0 ± NaN | 0 ± NaN         |           0 ± NaN |       0 ± NaN |             0 ± NaN | 3670843 ± 1200398  |
| Tea                |           0 ± NaN |              11912 ± 13250 |                 0 ± NaN | 0 ± NaN         |           0 ± NaN | 14716 ± 14228 |             0 ± NaN | 16818 ± 17412      |
| Other\_Crop        | 5971717 ± 2263617 |              55359 ± 35228 |       3541970 ± 1495526 | 0 ± NaN         |    109316 ± 45016 | 21022 ± 27587 |           701 ± 726 | 3811622 ± 1057935  |
| Other\_Palm        |     77081 ± 40025 |                  701 ± 779 |           39943 ± 23905 | 0 ± NaN         |     67272 ± 32448 |       0 ± NaN |       52554 ± 29116 | 1402 ± 1451        |
| Other\_Tree        |   180922 ± 109106 |            275671 ± 227000 |        1451933 ± 757468 | 13314 ± 17767   | 7177557 ± 1425950 |       0 ± NaN | 107743992 ± 7291167 | 199048 ± 50712     |
| Other\_Shrub       |   555528 ± 463776 |             158366 ± 82668 |         239804 ± 158390 | 516139 ± 537935 |     29430 ± 15684 |       0 ± NaN |    2546628 ± 706827 | 11638985 ± 2227338 |
| Herbaceous         |   559031 ± 463615 |              46250 ± 33628 |           23825 ± 12029 | 218155 ± 236850 |    146454 ± 48095 |       0 ± NaN |     490192 ± 213097 | 1268465 ± 401272   |
| Non\_vegetated     |     63067 ± 34212 |              76379 ± 47146 |          108650 ± 55687 | 0 ± NaN         |     83463 ± 75617 |   2102 ± 3065 |     624313 ± 468339 | 3343641 ± 1183549  |
| Built\_up          |       4905 ± 3790 |                    0 ± NaN |           59619 ± 42753 | 0 ± NaN         |       1402 ± 1052 |       0 ± NaN |           701 ± 722 | 6797787 ± 1586649  |
| Water              |           0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN         |       2803 ± 2931 |       0 ± NaN |         4204 ± 2717 | 4755913 ± 1430934  |
| Other\_LAND\_COVER |           0 ± NaN |              52612 ± 63954 |           29431 ± 16822 | 0 ± NaN         |       2803 ± 2340 |       0 ± NaN |           701 ± 722 | 2762075 ± 777843   |

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

| Cover              | Forest\_Commodity |     Natural\_Forest | Other\_LAND\_USE\_YEAR\_2000 |
| :----------------- | ----------------: | ------------------: | ---------------------------: |
| Aquaculture        |           0 ± NaN |       18218 ± 17478 |              297413 ± 293142 |
| Bamboo             |           0 ± NaN |     435589 ± 246104 |                20321 ± 17019 |
| Coconut            |           0 ± NaN |    1062723 ± 548708 |              400534 ± 305054 |
| Coffee             |           0 ± NaN |       30832 ± 15674 |               131739 ± 43971 |
| Fruit\_Nut         |       2803 ± 2069 |     981307 ± 607681 |              171149 ± 119203 |
| Oil\_Palm          |   418343 ± 125446 |   9625303 ± 1690115 |             2284810 ± 931047 |
| Pulpwood           |    277494 ± 86424 |    1178843 ± 618442 |                98104 ± 42266 |
| Rubber             |     72878 ± 36291 |      194107 ± 56488 |                65870 ± 31494 |
| Rice               |           0 ± NaN |    1942406 ± 856897 |             2695748 ± 927395 |
| Tea                |           0 ± NaN |             0 ± NaN |                43446 ± 26849 |
| Other\_Crop        |     32935 ± 25113 |   8887017 ± 1575033 |            4591754 ± 1527303 |
| Other\_Palm        |     15417 ± 11301 |      220733 ± 58198 |                  2803 ± 1816 |
| Other\_Tree        |    156969 ± 54019 | 114645591 ± 5698263 |             2239878 ± 697684 |
| Other\_Shrub       |     19621 ± 11936 |  13329798 ± 1754569 |             2335461 ± 825618 |
| Herbaceous         |    110017 ± 34113 |    2085685 ± 606823 |              556670 ± 232937 |
| Non\_vegetated     |       6307 ± 4155 |    2858051 ± 886353 |             1437257 ± 816212 |
| Built\_up          |      16818 ± 9815 |    1653176 ± 470771 |            5194420 ± 1533735 |
| Water              |       4205 ± 3318 |     234953 ± 167608 |            4523764 ± 1301746 |
| Other\_LAND\_COVER |           0 ± NaN |    1439816 ± 425166 |             1407805 ± 708208 |

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

| Condition          |   Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral   |        Plantation |       Terrace |     Natural\_Forest | Other\_LAND\_USE   |
| :----------------- | ----------------: | -------------------------: | ----------------------: | :-------------- | ----------------: | ------------: | ------------------: | :----------------- |
| Aquaculture        |     16818 ± 27313 |                    0 ± NaN |                 0 ± NaN | 0 ± NaN         |           0 ± NaN |       0 ± NaN |             0 ± NaN | 298814 ± 501232    |
| Bamboo             |           0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN         |           0 ± NaN |       0 ± NaN |     434888 ± 422906 | 21022 ± 23257      |
| Built\_up          |       4905 ± 3882 |                    0 ± NaN |           59619 ± 43998 | 0 ± NaN         |       1402 ± 1096 |       0 ± NaN |           701 ± 748 | 6797787 ± 2376658  |
| Coconut            |     11912 ± 14268 |                4204 ± 4103 |         885970 ± 748227 | 0 ± NaN         |   561171 ± 494661 |       0 ± NaN |             0 ± NaN | 0 ± NaN            |
| Coffee             |     67972 ± 37576 |                1402 ± 1535 |           61665 ± 34305 | 0 ± NaN         |     25927 ± 22010 |       0 ± NaN |           701 ± 773 | 4905 ± 5403        |
| Fruit\_Nut         |   265842 ± 249314 |                7708 ± 9843 |         857184 ± 496361 | 0 ± NaN         |     23824 ± 25577 |       0 ± NaN |           701 ± 899 | 0 ± NaN            |
| Herbaceous         |   559031 ± 480386 |              46250 ± 31579 |           23825 ± 13382 | 218155 ± 186981 |    146454 ± 62691 |       0 ± NaN |     490192 ± 268384 | 1268465 ± 548710   |
| Non\_vegetated     |     63067 ± 39092 |              76379 ± 44820 |          108650 ± 63366 | 0 ± NaN         |     83463 ± 79733 |   2102 ± 2299 |     624313 ± 529389 | 3343641 ± 1661385  |
| Oil\_Palm          |  2009251 ± 900246 |              12614 ± 13253 |       4309321 ± 1372427 | 0 ± NaN         | 5979752 ± 1909815 |       0 ± NaN |           701 ± 736 | 16818 ± 17672      |
| Other\_Crop        | 5971717 ± 1782916 |              55359 ± 28580 |       3541970 ± 1259496 | 0 ± NaN         |    109316 ± 45816 | 21022 ± 17416 |           701 ± 739 | 3811622 ± 1312103  |
| Other\_LAND\_COVER |           0 ± NaN |              52612 ± 57857 |           29431 ± 19085 | 0 ± NaN         |       2803 ± 2516 |       0 ± NaN |           701 ± 769 | 2762075 ± 744321   |
| Other\_Palm        |     77081 ± 44750 |                  701 ± 761 |           39943 ± 26403 | 0 ± NaN         |     67272 ± 40404 |       0 ± NaN |       52554 ± 34779 | 1402 ± 1525        |
| Other\_Shrub       |   555528 ± 453724 |             158366 ± 57060 |         239804 ± 153994 | 516139 ± 299105 |     29430 ± 15615 |       0 ± NaN |    2546628 ± 831176 | 11638985 ± 2662310 |
| Other\_Tree        |   180922 ± 100301 |            275671 ± 181943 |        1451933 ± 681105 | 13314 ± 13715   | 7177557 ± 1478743 |       0 ± NaN | 107743992 ± 6733242 | 199048 ± 46399     |
| Pulpwood           |           0 ± NaN |                    0 ± NaN |               701 ± 823 | 0 ± NaN         | 1553741 ± 1292846 |       0 ± NaN |             0 ± NaN | 0 ± NaN            |
| Rice               |   951896 ± 682262 |              15416 ± 17024 |                 0 ± NaN | 0 ± NaN         |           0 ± NaN |       0 ± NaN |             0 ± NaN | 3670843 ± 1802182  |
| Rubber             |           0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN         |   322344 ± 133277 |       0 ± NaN |         2102 ± 2270 | 8409 ± 9029        |
| Tea                |           0 ± NaN |              11912 ± 15983 |                 0 ± NaN | 0 ± NaN         |           0 ± NaN | 14716 ± 19656 |             0 ± NaN | 16818 ± 22505      |
| Water              |           0 ± NaN |                    0 ± NaN |                 0 ± NaN | 0 ± NaN         |       2803 ± 3088 |       0 ± NaN |         4204 ± 3176 | 4755913 ± 2292231  |

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

| Cover                      | Forest\_Commodity |     Natural\_Forest | Other\_LAND\_USE\_YEAR\_2000 |
| :------------------------- | ----------------: | ------------------: | ---------------------------: |
| Agrisiviculture            |           0 ± NaN |   6173695 ± 1364640 |            4562247 ± 1352751 |
| Boundary\_Agrisilviculture |     15417 ± 15881 |     560871 ± 195373 |               142305 ± 69878 |
| Mixed\_Agrisilviculture    |     16818 ± 17304 |   8554971 ± 1620916 |            3038225 ± 1150153 |
| Silvopastoral              |     16818 ± 17591 |     705563 ± 446088 |                25227 ± 19689 |
| Plantation                 |   925683 ± 245631 |  13613035 ± 2122845 |             1548541 ± 553353 |
| Terrace                    |           0 ± NaN |         5606 ± 5807 |                32235 ± 23933 |
| Natural\_Forest            |     67973 ± 36762 | 111322167 ± 3997374 |              512937 ± 117389 |
| Other\_LAND\_USE           |     91098 ± 41466 |  19888241 ± 2239113 |           18637227 ± 4738105 |

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

| Crops              |   Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral   |        Plantation |     Terrace |     Natural\_Forest | Other\_LAND\_USE  |
| :----------------- | ----------------: | -------------------------: | ----------------------: | :-------------- | ----------------: | ----------: | ------------------: | :---------------- |
| Aquaculture        |             0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0           |             0 ± 0 |       0 ± 0 |               0 ± 0 | 18218 ± 17472     |
| Coconut            |     11912 ± 12336 |                      0 ± 0 |         754100 ± 506848 | 0 ± 0           |   296712 ± 211026 |       0 ± 0 |               0 ± 0 | 0 ± 0             |
| Coffee             |       5606 ± 4212 |                      0 ± 0 |           24525 ± 15071 | 0 ± 0           |             0 ± 0 |       0 ± 0 |           701 ± 726 | 0 ± 0             |
| Fruit\_Nut         |     94693 ± 90528 |                7708 ± 7932 |         857184 ± 600410 | 0 ± 0           |     21021 ± 17942 |       0 ± 0 |           701 ± 726 | 0 ± 0             |
| Oil\_Palm          |   562350 ± 348297 |                      0 ± 0 |        3521865 ± 955124 | 0 ± 0           | 5540388 ± 1380170 |       0 ± 0 |           701 ± 721 | 0 ± 0             |
| Pulpwood           |             0 ± 0 |                      0 ± 0 |               701 ± 721 | 0 ± 0           |  1178142 ± 617091 |       0 ± 0 |               0 ± 0 | 0 ± 0             |
| Rubber             |             0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0           |    192005 ± 55829 |       0 ± 0 |         2102 ± 2163 | 0 ± 0             |
| Rice               |   426499 ± 433849 |                      0 ± 0 |                   0 ± 0 | 0 ± 0           |             0 ± 0 |       0 ± 0 |               0 ± 0 | 1515907 ± 737676  |
| Tea                |             0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0           |             0 ± 0 |       0 ± 0 |               0 ± 0 | 0 ± 0             |
| Other\_Crop        | 3890160 ± 1038250 |               9811 ± 10096 |        2106908 ± 789413 | 0 ± 0           |     67271 ± 32343 | 5606 ± 5805 |           701 ± 726 | 2806561 ± 936526  |
| Other\_Palm        |     77081 ± 35746 |                      0 ± 0 |           39943 ± 22130 | 0 ± 0           |     53257 ± 27966 |       0 ± 0 |       50451 ± 28944 | 0 ± 0             |
| Other\_Tree        |     49089 ± 37478 |            263058 ± 181370 |         869058 ± 511085 | 0 ± 0           | 6096688 ± 1385169 |       0 ± 0 | 107301829 ± 4033197 | 65868 ± 23087     |
| Other\_Shrub       |   449623 ± 434186 |             157665 ± 50074 |         231395 ± 148130 | 490912 ± 284342 |     19620 ± 11332 |       0 ± 0 |    2438714 ± 691547 | 9541869 ± 1552371 |
| Herbaceous         |   538709 ± 443382 |              46250 ± 27667 |             9110 ± 5149 | 214651 ± 171401 |     67271 ± 26264 |       0 ± 0 |     467067 ± 211055 | 742628 ± 311545   |
| Other\_LAND\_COVER |             0 ± 0 |                      0 ± 0 |           29431 ± 15449 | 0 ± 0           |       2803 ± 2279 |       0 ± 0 |           701 ± 721 | 1406881 ± 424774  |

Table R9. Estimate of area and standard error of each commodity land
cover (in hectares), by land use, in areas that were forested in the
year 2000 and experienced forest cover loss between 2001 and 2015.
‘Other Crops’ contains any agricultural or crop land covers that could
not be identified during the photo interpretation process. ‘Other Land
Use’ contains all other land covers that did not fit into the other
categories.

``` r
# Crops in TCC in 2000 ---------------------------------------------------------
```

Triple conditional for area of commodities that were had tree cover in
the year 2000.

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

| Crops        |   Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral   |        Plantation |     Terrace |     Natural\_Forest | Other\_LAND\_USE  |
| :----------- | ----------------: | -------------------------: | ----------------------: | :-------------- | ----------------: | ----------: | ------------------: | :---------------- |
| Aquaculture  |             0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0           |             0 ± 0 |       0 ± 0 |               0 ± 0 | 18218 ± 17472     |
| Coconut      |     11912 ± 12336 |                      0 ± 0 |         754100 ± 506848 | 0 ± 0           |   296712 ± 211026 |       0 ± 0 |               0 ± 0 | 0 ± 0             |
| Coffee       |       5606 ± 4212 |                      0 ± 0 |           24525 ± 15071 | 0 ± 0           |             0 ± 0 |       0 ± 0 |           701 ± 726 | 0 ± 0             |
| Fruit\_Nut   |     94693 ± 90528 |                7708 ± 7932 |         857184 ± 600410 | 0 ± 0           |     23824 ± 18055 |       0 ± 0 |           701 ± 726 | 0 ± 0             |
| Oil\_Palm    |   562350 ± 348297 |              12614 ± 12980 |        3521865 ± 955124 | 0 ± 0           | 5929300 ± 1382175 |       0 ± 0 |           701 ± 721 | 16818 ± 17307     |
| Pulpwood     |             0 ± 0 |                      0 ± 0 |               701 ± 721 | 0 ± 0           |  1455637 ± 620818 |       0 ± 0 |               0 ± 0 | 0 ± 0             |
| Rubber       |             0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0           |    264883 ± 64968 |       0 ± 0 |         2102 ± 2163 | 0 ± 0             |
| Rice         |   426499 ± 433849 |                      0 ± 0 |                   0 ± 0 | 0 ± 0           |             0 ± 0 |       0 ± 0 |               0 ± 0 | 1515907 ± 737676  |
| Tea          |             0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0           |             0 ± 0 |       0 ± 0 |               0 ± 0 | 0 ± 0             |
| Other\_Crop  | 3890160 ± 1038250 |               9811 ± 10096 |        2106908 ± 789413 | 0 ± 0           |     84089 ± 36656 | 5606 ± 5805 |           701 ± 726 | 2822678 ± 936670  |
| Other\_Palm  |     77081 ± 35746 |                      0 ± 0 |           39943 ± 22130 | 0 ± 0           |     67272 ± 29886 |       0 ± 0 |       51853 ± 28979 | 0 ± 0             |
| Other\_Tree  |     49089 ± 37478 |            265861 ± 181393 |         878168 ± 511169 | 13314 ± 13701   | 6151347 ± 1385399 |       0 ± 0 | 107359991 ± 4033249 | 84789 ± 26895     |
| Other\_Shrub |   449623 ± 434186 |             157665 ± 50074 |         231395 ± 148130 | 490912 ± 284342 |     29430 ± 14746 |       0 ± 0 |    2444320 ± 691554 | 9546074 ± 1552370 |
| Herbaceous   |   538709 ± 443382 |              46250 ± 27667 |            16818 ± 9448 | 218155 ± 171439 |    145754 ± 39515 |       0 ± 0 |     469870 ± 211066 | 760146 ± 311791   |

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

| Crops              | Agrisiviculture | Boundary\_Agrisilviculture | Mixed\_Agrisilviculture | Silvopastoral |    Plantation | Terrace | Natural\_Forest | Other\_LAND\_USE |
| :----------------- | --------------: | -------------------------: | ----------------------: | :------------ | ------------: | ------: | --------------: | :--------------- |
| Non\_vegetated     |   63067 ± 30899 |              76379 ± 34172 |          105847 ± 47957 | 0 ± 0         | 83463 ± 72573 |   0 ± 0 | 618707 ± 468432 | 1916895 ± 750628 |
| Built\_up          |     4905 ± 3620 |                      0 ± 0 |             4905 ± 2788 | 0 ± 0         |   1402 ± 1019 |   0 ± 0 |       701 ± 721 | 1658081 ± 469140 |
| Water              |           0 ± 0 |                      0 ± 0 |                   0 ± 0 | 0 ± 0         |   2803 ± 2884 |   0 ± 0 |     4204 ± 2709 | 232150 ± 167383  |
| Other\_LAND\_COVER |           0 ± 0 |                      0 ± 0 |           29431 ± 15449 | 0 ± 0         |   2803 ± 2279 |   0 ± 0 |       701 ± 721 | 1406881 ± 424774 |

Table R11. Estimate of area and standard error of each commodity
‘support’ land cover (in hectares), by land use, in areas that had
tree cover in the year 2000 and experienced tree canopy loss between
2001 and 2015. ‘Other Crops’ contains any agricultural or crop land
covers that could not be identified during the photo interpretation
process. ‘Other Land Use’ contains all other land covers that did not
fit into the other categories.

``` r
# Carbon Factors ---------------------------------------------------------------

# Note, Fruit/Nut value is temporary, should really be a range, not average
# Remember to adjust for commodities present in each country. 
carbonMono <- c("Coconut" = 32, 
                "Coffee" = 18.9, 
                "Fruit_Nut" = 42.1, 
                "Oil_Palm" = 38.97,
                "Pulpwood" = 38.2, 
                "Rubber" = 38.2,
                "Rice" = 1, 
                "Other_Crop" = 5,
                "Other_Palm" = 32,
                "Other_Tree" = 39,
                "Other_Shrub" = 22.5,
                "Herbaceous" = 5)


carbonAF <- c("Coconut" = 32, 
              "Coffee" = 32.8, 
              "Fruit_Nut" = 42.1,
              "Oil_Palm" = 38.97, 
              "Pulpwood" = 38.2,
              "Rubber" = 38.2,
              "Rice" = 1,
              "Other_Crop" = 5,
              "Other_Palm" = 32,
              "Other_Tree" = 39,
              "Other_Shrub" = 32.8,
              "Herbaceous" = 5)

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
| Coconut      |        32.0 |         32.0 |
| Coffee       |        18.9 |         32.8 |
| Fruit\_Nut   |        42.1 |         42.1 |
| Oil\_Palm    |        39.0 |         39.0 |
| Pulpwood     |        38.2 |         38.2 |
| Rubber       |        38.2 |         38.2 |
| Rice         |         1.0 |          1.0 |
| Other\_Crop  |         5.0 |          5.0 |
| Other\_Palm  |        32.0 |         32.0 |
| Other\_Tree  |        39.0 |         39.0 |
| Other\_Shrub |        22.5 |         32.8 |
| Herbaceous   |         5.0 |          5.0 |

Table R12. Carbon factors used to calculate the aboveground biomass for
each commodity. Values for commodities were compiled for monoculture and
agroforestry systems from peer-reviewed and grey literature.
Time-averaged values are often used to estimate the carbon storage of
rotational commodity crops because they allow for the “averaging” of a
mixture of freshly replanted and mature commodity areas across the
landscape. Sources for the carbon values presented here are included in
Appendix 1.

``` r
# calculate Carbon values for previously tree-covered areas --------------------

# make results into an area
tccAreas <- arrange(cResults2, match(cover, covOrder)) %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas),
         SE = SE * sum(stratumAreas))

## Agroforestry Carbon
tccA <- tccAreas[seq(1,112,8),][-c(1,9),] #keep all elements
  
tccBA <- tccAreas[seq(2,112,8),][-c(1,9),] #keep all elements

tccMA <- tccAreas[seq(3,112,8),][-c(1,9),] #keep all elements

tccSP <- tccAreas[seq(7,112,8),][-c(1,9),]
#remove herbaceous, other palm, other shrub, other tree
tccSP[,c(4,5)] <- tccSP[,c(4,5)] * c(1,1,1,1,1,1,1,1,0,0,0,0) 

tccAA <- tccA[,4] + tccBA[,4] + tccMA[,4] + tccSP[,4]

tccAASE <- bind_cols(A = pull(tccA[,5])^2, BA = pull(tccBA[,5])^2, 
                    MA = pull(tccMA[,5])^2, SP = pull(tccSP[,5])^2) %>% 
  rowwise() %>% 
  transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>% 
  sqrt()

tccAF <- bind_cols(commodity = pull(tccAreas[seq(1,112,8),1][-c(1,9),]), 
                   Area_ha = pull(tccAA), 
                   SE_ha = tccAASE)

tccAF <- transmute(tccAF,  commodity = commodity, Mg_C = Area_ha * carbonAF, 
                   SE = SE_ha * carbonAF)
 
captiontccAF <- c("Table R13. Aboveground biomass carbon values (1,000s Mg C) 
 associated with the area of commodities in agroforestry land uses that occur in
 formerly tree covered areas.")

kable(tccAF, caption = captiontccAF, digits = 0,
       col.names = c("Commodity", "MgC", "SE"))
```

| Commodity    |       MgC |       SE |
| :----------- | --------: | -------: |
| Coconut      |  24512373 | 16223938 |
| Coffee       |    988309 |   513284 |
| Fruit\_Nut   |  40398545 | 25565156 |
| Oil\_Palm    | 159653386 | 39621968 |
| Pulpwood     |     26769 |    27546 |
| Rubber       |         0 |        0 |
| Rice         |    426499 |   433849 |
| Other\_Crop  |  30034397 |  6521577 |
| Other\_Palm  |   3744772 |  1345330 |
| Other\_Tree  |  46531613 | 21204027 |
| Other\_Shrub |  27508783 | 15136683 |
| Herbaceous   |   3008885 |  2221722 |

Table R13. Aboveground biomass carbon values (1,000s Mg C) associated
with the area of commodities in agroforestry land uses that occur in
formerly tree covered areas.

``` r
## Calculate carbon in associated with non-AF areas
tccO <- tccAreas[seq(5,112,8),][-c(1,9),]
#remove herbaceous, other palm, other shrub, other tree
tccO[,c(4,5)] <- tccO[,c(4,5)] * c(1,1,1,1,1,1,1,1,0,0,0,0)

tccP <- tccAreas[seq(6,112,8),][-c(1,9),] #keep all elements

tccT <- tccAreas[seq(8,112,8),][-c(1,9),] #keep all elements

tccOPT <- tccO[,4] + tccP[,4] + tccT[,4]

tccOPTSE <- bind_cols(O = pull(tccO[,5])^2, P = pull(tccP[,5])^2, 
                     Tr = pull(tccT[,5])^2) %>% 
  rowwise() %>% 
  transmute(SE_ha = sum(O, P, Tr, na.rm = T)) %>% 
  sqrt()

tccMono <- bind_cols(commodity = pull(tccAreas[seq(1,112,8),1][-c(1,9),]), 
                     Area_ha = pull(tccOPT), 
                     SE_ha = tccOPTSE)

tccMono <- transmute(tccMono, commodity = commodity, 
                   Mg_C = Area_ha * carbonMono, SE = SE_ha * carbonMono)


captiontccMono <- c("Table R14. Aboveground biomass carbon values (1,000s Mg C) 
  associated with the area of commodities in monoculture plantation and terrace 
  land that occur in formerly tree-covered areas.")

kable(tccMono, caption = captiontccMono, digits = 0, 
     col.names = c("Commodity", "MgC", "SE"))
```

| Commodity    |       MgC |       SE |
| :----------- | --------: | -------: |
| Coconut      |   9494775 |  6752829 |
| Coffee       |         0 |        0 |
| Fruit\_Nut   |   1003001 |   760110 |
| Oil\_Palm    | 231720211 | 53867601 |
| Pulpwood     |  55605327 | 23715230 |
| Rubber       |  10118531 |  2481777 |
| Rice         |   1515907 |   737676 |
| Other\_Crop  |  14561863 |  4687027 |
| Other\_Palm  |   2152717 |   956346 |
| Other\_Tree  | 239902526 | 54030549 |
| Other\_Shrub |    662184 |   331786 |
| Herbaceous   |    728768 |   197574 |

Table R14. Aboveground biomass carbon values (1,000s Mg C) associated
with the area of commodities in monoculture plantation and terrace land
that occur in formerly tree-covered areas.

``` r
# Calculate Carbon for Formerly Forested Areas ---------------------------------

# # Calculate total carbon associated with agroforestry systems
# comA <- coverInUseArea[seq(1, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comBA <- coverInUseArea[seq(2, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comMA <- coverInUseArea[seq(3, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comSP <- coverInUseArea[seq(7, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comAA <- comA[,3] + comBA[,3] + comMA[,3] + comSP[,3]
# 
# comAASE <- bind_cols(A = pull(comA[,4])^2, BA = pull(comBA[,4])^2, 
#                      MA = pull(comMA[,4])^2, SP = pull(comSP[,4])^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>% 
#   sqrt()
# 
# allAF <- bind_cols(commodity = carbonNames, Area_ha = pull(comAA), 
#                    SE_ha = comAASE)
# 
# draftAF <- transmute(allAF,  commodity = commodity, Mg_C = Area_ha * carbonAF, 
#                      SE = SE_ha * carbonAF)
# 
# captionAF <- c("Table X. Aboveground biomass carbon values associated with 
#                the area of commodities in agroforestry land uses in the period 
#                after 2015.")
# kable(draftAF, caption = captionAF, digits = 0,
#       col.names = c("Commodity", "MgC", "SE"))
# 
# 
# # Calculate carbon in associated with plantation areas
# comO <- coverInUseArea[seq(5, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comP <- coverInUseArea[seq(6, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comT <- coverInUseArea[seq(8, 152, 8), ][c(4, 5, 6, 9, 10, 15, 16, 17),]
# comOPT <- comO[,3] + comP[,3] + comT[,3]
# comOPTSE <- bind_cols(O = pull(comO[,4])^2, P = pull(comP[,4])^2, 
#                       Tr = pull(comT[,4])^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(O, P, Tr, na.rm = T)) %>% 
#   sqrt()
# 
# allMono <- bind_cols(commodity = carbonNames, Area_ha = pull(comOPT), 
#                      SE_ha = comOPTSE)
# 
# draftMono <- transmute(allMono, commodity = commodity, 
#                        Mg_C = Area_ha * carbonMono, SE = SE_ha * carbonMono)
# 
# 
# captionMono <- c("Table X. Aboveground biomass carbon values associated with 
#                  the area of commodities in monoculture plantation and terrace land 
#                  uses in the period after 2015.")
# kable(draftMono, caption = captionMono, digits = 0, 
#       col.names = c("Commodity", "MgC", "SE"))

# calculate Carbon values for previously forested areas ------------------------
# make results into an area
# ffAreas <- arrange(cResults, cover) %>% 
#   mutate(PercentCover = PercentCover * sum(stratumAreas),
#          SE = SE * sum(stratumAreas))
# 
# # agroforestry carbon
# ffA <- ffAreas[seq(1,72,8),][-9,]
# ffBA <- ffAreas[seq(2,72,8),][-9,]
# ffMA <- ffAreas[seq(3,72,8),][-9,]
# ffSP <- ffAreas[seq(7,72,8),][-9,]
# ffAA <- ffA[,4] + ffBA[,4] + ffMA[,4] + ffSP[,4]
# 
# ffAASE <- bind_cols(A = pull(ffA[,5])^2, BA = pull(ffBA[,5])^2, 
#                      MA = pull(ffMA[,5])^2, SP = pull(ffSP[,5])^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(A, BA, MA, SP, na.rm = T)) %>% 
#   sqrt()
# 
# ffAF <- bind_cols(commodity = carbonNames, Area_ha = pull(ffAA), 
#                    SE_ha = ffAASE)
# 
# ffAF <- transmute(ffAF,  commodity = commodity, Mg_C = Area_ha * carbonAF, 
#                      SE = SE_ha * carbonAF)
# 
# captionffAF <- c("Table X. Aboveground biomass carbon values associated with 
#                the area of commodities in agroforestry land uses that occur in
#                  formerly forested areas.")
# kable(ffAF, caption = captionffAF, digits = 0,
#       col.names = c("Commodity", "MgC", "SE"))
# 
# # Calculate carbon in associated with non-AF areas
# ffO <- ffAreas[seq(5,72,8),][-9,]
# ffP <- ffAreas[seq(6,72,8),][-9,]
# ffT <- ffAreas[seq(8,72,8),][-9,]
# ffOPT <- ffO[,4] + ffP[,4] + ffT[,4]
# ffOPTSE <- bind_cols(O = pull(ffO[,5])^2, P = pull(ffP[,5])^2, 
#                       Tr = pull(ffT[,5])^2) %>% 
#   rowwise() %>% 
#   transmute(SE_ha = sum(O, P, Tr, na.rm = T)) %>% 
#   sqrt()
# 
# ffMono <- bind_cols(commodity = carbonNames, Area_ha = pull(ffOPT), 
#                      SE_ha = ffOPTSE)
# 
# ffMono <- transmute(ffMono, commodity = commodity, 
#                        Mg_C = Area_ha * carbonMono, SE = SE_ha * carbonMono)
# 
# 
# captionffMono <- c("Table X. Aboveground biomass carbon values associated with 
#                  the area of commodities in monoculture plantation and terrace land 
#                  that occur in formerly forested areas.")
# kable(ffMono, caption = captionffMono, digits = 0, 
#       col.names = c("Commodity", "MgC", "SE"))
```

``` r
# Understory -------------------------------------------------------------------
```

## Understory analysis

The understory areas are small, but contain a small areas that need to
be accounted for in the analysis.

``` r
questions3 <- c("UNDERSTORY_PRESENT", "UNDERSTORY_COVER")
understoryArea <- t(generalResults$Cover$UNDERSTORY_PRESENT) * sum(stratumAreas)
colnames(understoryArea) <- c("Area_ha", "SE_Area_ha")


coverInUnder <- do_yoc_analysis(rawData, mtdt = metaNames, qstns = questions3, 
                                grplst = groupList, strata = strata, ns = sampSize,
                                areas = stratumAreas, cvrfld = "UNDERSTORY_COVER", 
                                cndtnfld = "UNDERSTORY_PRESENT")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

    ## Warning in sqrt(colSums(strataVar)): NaNs produced

``` r
coverInUnderArea <- calcError(coverInUnder, understoryArea, 3, 4, 1, 2)

coverInUnderArea %>% 
  select(Cover, Condition, Value) %>% 
  spread(Condition, Value) %>% 
  kable(., digits = 0, caption = c("Table X. Total area of each land cover from
                                   the period after 2015 occuring in the 
                                   understory."))
```

| Cover       | No |   Yes |
| :---------- | -: | ----: |
| Coffee      | NA | 89694 |
| Other\_Crop | NA | 43445 |

Table X. Total area of each land cover from the period after 2015
occuring in the understory.

``` r
coverInUnderArea %>% 
  select(Cover, Condition, Error) %>% 
  spread(Condition, Error) %>% 
  kable(., digits = 0, caption = c("Table X. Standard error of each land cover from
                                   the period after 2015 occuring in the understory."))
```

| Cover       | No |   Yes |
| :---------- | -: | ----: |
| Coffee      | NA | 69472 |
| Other\_Crop | NA | 25556 |

Table X. Standard error of each land cover from the period after 2015
occuring in the understory.

``` r
coverInUnderArea %>% 
  mutate(Pretty = paste(round(Value), round(Error), sep = " \u00B1 ")) %>% 
  select(Cover, Condition, Pretty) %>% 
  spread(Condition, Pretty) %>% 
  kable(., align = "lrrr", caption = c("Table X. Area and standard error of each 
                                       land cover from the period after 2015 
                                       occuring in the understory."))
```

| Cover       |      No |           Yes |
| :---------- | ------: | ------------: |
| Coffee      | NA ± NA | 89694 ± 69472 |
| Other\_Crop | NA ± NA | 43445 ± 25556 |

Table X. Area and standard error of each land cover from the period
after 2015 occuring in the understory.

``` r
#understory associations
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

uResults %>% 
  select(cover, condition1, PercentCover) %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas)) %>% 
  spread(cover, PercentCover) %>% 
  kable(., digits = 0, col.names = c("Land Use", sort(underCrops)), 
        caption = c("Table X. Total area of each understory commodity crop, 
        in each of the associated overstory crops."))
```

| Land Use    | Coffee | Other\_Crop |
| :---------- | -----: | ----------: |
| Oil\_Palm   |      0 |           0 |
| Other\_Crop |      0 |       30831 |
| Other\_Palm |      0 |           0 |
| Other\_Tree |  45547 |       12613 |
| Rubber      |      0 |           0 |

Table X. Total area of each understory commodity crop, in each of the
associated overstory crops.

``` r
uResults %>% 
  select(cover, condition1, SE) %>% 
  mutate(SE = SE * sum(stratumAreas)) %>% 
  spread(cover,SE) %>% 
  kable(., digits = 0, col.names = c("Land Use", sort(underCrops)), 
        caption = c("Table X. Total area of each understory commodity crop, 
        in each of the associated overstory crops."))
```

| Land Use    | Coffee | Other\_Crop |
| :---------- | -----: | ----------: |
| Oil\_Palm   |      0 |           0 |
| Other\_Crop |      0 |       20730 |
| Other\_Palm |      0 |           0 |
| Other\_Tree |  23713 |        7295 |
| Rubber      |      0 |           0 |

Table X. Total area of each understory commodity crop, in each of the
associated overstory crops.

``` r
## Raw Tables 
# kable(coverIn2000Area)
# kable(coverInUseArea)
# kable(useInCoverArea)
# kable(useInUse2000Area)
```
