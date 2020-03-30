Old Scratch File for Development
================
MS Patterson, <tertiarymatt@gmail.com>
June 14, 2019

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
dataPath <- c("data/Vietnam")
files <- dir(dataPath)

test <- files %>% 
  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))

# remove unneeded columns 
dataNames <- colnames(test)
keep <- c(1,2, 5, 7, 10, 11, 13:19)
test <- test[,keep]

# remove Potapov plots
test <- subset(test, PL_STRATUM != 3)

# Clean up the data
test <- clean_data(test, c(9:13))
```

``` r
# Test presence, using Tea
result <- presence(test, lblfld = "LAND_COVER", cmmdty = 'Tea')$Tea

# determine frequency of Tea
sum(result)
```

    ## [1] 248

``` r
# Test question table builder
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")

landCover <- build_question(test, mtdt = metaNames, qstn = "LAND_COVER")

# Test analysis table builder
commodityNames <- c("Rubber", "Tea", "Coffee", "Pulpwood", "Coconut", "Oil_Palm")
groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

toAnalyze <- build_yc(landCover, lblfld = 'LAND_COVER', 
                           cmmdtylst = commodityNames)

# Test plot level summarization
pSummary <- plot_means(toAnalyze, grplst = groupList, cmmdtylst = commodityNames)

# Stratum summarization
sSummary <- stratum_means(toAnalyze, grplst = groupList, cmmdtylst = commodityNames)

sError <- stratum_SE(toAnalyze, grplst = groupList, cmmdtylst = commodityNames)

stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799, 
                  "Strata3 Area" = 1604577)
#stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799) 

sampSize <- c(312, 576, 425)
#sampSize <- c(312, 576)

# Overall proportion of cover and variance
p_hat_sub_c <- overall_prop(sSummary, areas = stratumAreas)
se_hat_sub_c <- overall_SE(pSummary, c(1,2), areas = stratumAreas, ns = sampSize)

p_hat_sub_c * 100
```

    ##     Rubber        Tea     Coffee   Pulpwood    Coconut   Oil_Palm 
    ##  7.3854942  0.6875607  1.3663654 11.4130939  0.0000000  0.0000000

``` r
se_hat_sub_c * 100
```

    ##    Rubber       Tea    Coffee  Pulpwood   Coconut  Oil_Palm 
    ## 0.6050383 0.1958464 0.2286939 0.7321189 0.0000000 0.0000000

``` r
# Doing full analysis

# Extra tidbits needed for totals
ntotal <- nrow(test)/24
nstrata1 <- length(which(test$PL_STRATUM == 1))/24
nstrata2 <- length(which(test$PL_STRATUM == 2))/24
nstrata3 <- length(which(test$PL_STRATUM == 3))/24

nplots <- c("Total Plots" = ntotal, "Strata1 Plots" = nstrata1, 
            "Strata2 Plots" = nstrata2, "Strata3 Plots" = nstrata3)

kable(nplots) 
```

|               |   x |
| ------------- | --: |
| Total Plots   | 888 |
| Strata1 Plots | 312 |
| Strata2 Plots | 576 |
| Strata3 Plots |   0 |

``` r
countryArea <- 32727700

metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")

groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799, 
                  "Strata3 Area" = 1604577)

strata <- sort(unique(test$PL_STRATUM))

sampSize <- c(312, 576, 425)

questions <- colnames(test[,9:13])

results <- do_yc_analysis(table = test, mtdt = metaNames, strata = strata,
           ns = sampSize, areas = stratumAreas, qstns = questions, 
           grplst = groupList)
```

## Developing “Cover in Condition” Workflow

``` r
# Test analysis table builder
metaNames <- c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID",         
               "PL_COUNTRY", "PL_STRATUM")

commodityNames <- c("Coffee", "Fruit_Nut", "Pulpwood", "Rice", "Rubber", "Tea")

groupList <- c("PL_COUNTRY", "PL_STRATUM", "PLOT_ID")

questions <- colnames(test[,9:13])

stratumAreas <- c("Strata1 Area" = 348394, "Strata2 Area" = 1319799) 

strata <- sort(unique(test$PL_STRATUM))

sampSize <- c(312, 576)

#Build tables for doing o_in_c analysis for use types in commodities
landcover <- build_question(test, mtdt = metaNames, qstn = c("LAND_COVER"))
landuse <- build_question(test, mtdt = metaNames, qstn = c("LAND_USE"))
conditions <- sort(unique(landcover$LAND_COVER))
covers <- sort(unique(landuse$LAND_USE))

y_cTable <- build_yc(landcover, lblfld = 'LAND_COVER', cmmdtylst = conditions)


y_ocTable <- build_yoc(table = test, mtdt = metaNames, qstns = questions, 
                      grplst = groupList, cvrfld = "LAND_USE", 
                      cndtnfld = "LAND_COVER", conditions = conditions, 
                      covers = covers)
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
p_hat_o_in_c <- cond_prop(y_ocTable, y_cTable, areas = stratumAreas, grplst = groupList, 
          conditions = conditions)

se_p_hat_o_in_c <- cond_SE(yoctable = y_ocTable, yctable = y_cTable, strata = strata, 
        grplst = groupList, conditions = conditions, covers = covers, 
        areas = stratumAreas, ns = sampSize)

#cbind(p_hat_o_in_c, se_p_hat_o_in_c) * 100
use_in_rubber <- cbind(p_hat_o_in_c[seq(13, 120, 15)], se_p_hat_o_in_c[seq(13, 120, 15)])

#Build tables for doing o_in_c analysis for commodities in use types
covers2 <- sort(unique(landcover$LAND_COVER))
conditions2 <- sort(unique(landuse$LAND_USE))

y_cTable2 <- build_yc(landuse, lblfld = 'LAND_USE', cmmdtylst = conditions2)


y_ocTable2 <- build_yoc(table = test, mtdt = metaNames, qstns = questions, 
                        grplst = groupList, cvrfld = "LAND_COVER", 
                        cndtnfld = "LAND_USE", conditions = conditions2, 
                        covers = covers2)
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
p_hat_o_in_c2 <- cond_prop(y_ocTable2, y_cTable2, areas = stratumAreas, grplst = groupList, 
                          conditions = conditions2)

se_p_hat_o_in_c2 <- cond_SE(yoctable = y_ocTable2, yctable = y_cTable2, strata = strata, 
                           grplst = groupList, conditions = conditions2, covers = covers2, 
                           areas = stratumAreas, ns = sampSize)

#cbind(p_hat_o_in_c2, se_p_hat_o_in_c2) * 100
com_in_agslv <- cbind(p_hat_o_in_c2[seq(1, 120, 8)], se_p_hat_o_in_c2[seq(1, 120, 8)])


#Build tables for doing o_in_c analysis for commodities in year 2000 covers
landuse2000 <- build_question(test, mtdt = metaNames, qstn = c("LAND_USE_YEAR_2000"))

covers3 <- sort(unique(landcover$LAND_COVER))
conditions3 <- sort(unique(landuse2000$LAND_USE_YEAR_2000))

y_cTable3 <- build_yc(landuse2000, lblfld = 'LAND_USE_YEAR_2000', cmmdtylst = conditions3)


y_ocTable3 <- build_yoc(table = test, mtdt = metaNames, qstns = questions, 
                         grplst = groupList, cvrfld = "LAND_COVER", 
                         cndtnfld = "LAND_USE_YEAR_2000", conditions = conditions3, 
                         covers = covers3)
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
p_hat_o_in_c3 <- cond_prop(y_ocTable3, y_cTable3, areas = stratumAreas, grplst = groupList, 
                           conditions = conditions3)

se_p_hat_o_in_c3 <- cond_SE(yoctable = y_ocTable3, yctable = y_cTable3, strata = strata, 
                            grplst = groupList, conditions = conditions3, covers = covers3, 
                            areas = stratumAreas, ns = sampSize)

com_in_2000 <- cbind(p_hat_o_in_c3, se_p_hat_o_in_c3)
com_in_forest <- cbind(p_hat_o_in_c3[seq(1, 45, 3)], se_p_hat_o_in_c3[seq(1, 45, 3)])

do_yoc_analysis(test, mtdt = metaNames, qstns = questions, 
                             grplst = groupList, strata = strata, ns = sampSize,
                             areas = stratumAreas, cover = "LAND_COVER",
                             condition = "LAND_USE_YEAR_2000")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

    ##                                              p_hat_o_in_c se_p_hat_o_in_c
    ## Bamboo-in-Forest_Commodity                   0.0010141051    0.0010116916
    ## Bamboo-in-Natural_Forest                     0.0201742973    0.0064405371
    ## Bamboo-in-Other_LAND_USE_YEAR_2000           0.0003551121    0.0003556735
    ## Built_up-in-Forest_Commodity                 0.0029123421    0.0021594114
    ## Built_up-in-Natural_Forest                   0.0104847053    0.0026551288
    ## Built_up-in-Other_LAND_USE_YEAR_2000         0.0269713801    0.0079429548
    ## Coffee-in-Forest_Commodity                   0.0020282102    0.0020233833
    ## Coffee-in-Natural_Forest                     0.0284097507    0.0071528905
    ## Coffee-in-Other_LAND_USE_YEAR_2000           0.0422619108    0.0103859850
    ## Fruit_Nut-in-Forest_Commodity                0.0149788829    0.0074250962
    ## Fruit_Nut-in-Natural_Forest                  0.0243349327    0.0072940146
    ## Fruit_Nut-in-Other_LAND_USE_YEAR_2000        0.0570269254    0.0144524514
    ## Herbaceous-in-Forest_Commodity               0.0637248456    0.0143220122
    ## Herbaceous-in-Natural_Forest                 0.0553930668    0.0096434849
    ## Herbaceous-in-Other_LAND_USE_YEAR_2000       0.1371363203    0.0220079477
    ## Non_vegetated-in-Forest_Commodity            0.0264068252    0.0103129793
    ## Non_vegetated-in-Natural_Forest              0.0439699621    0.0089126010
    ## Non_vegetated-in-Other_LAND_USE_YEAR_2000    0.0685450949    0.0149522656
    ## Other_Crop-in-Forest_Commodity               0.0174162726    0.0092777911
    ## Other_Crop-in-Natural_Forest                 0.1133470929    0.0150287858
    ## Other_Crop-in-Other_LAND_USE_YEAR_2000       0.1881112786    0.0290312656
    ## Other_LAND_COVER-in-Forest_Commodity         0.0005070525    0.0005094052
    ## Other_LAND_COVER-in-Natural_Forest           0.0127023952    0.0044211655
    ## Other_LAND_COVER-in-Other_LAND_USE_YEAR_2000 0.0133686687    0.0065951878
    ## Other_Shrub-in-Forest_Commodity              0.0222461118    0.0086530115
    ## Other_Shrub-in-Natural_Forest                0.0848073636    0.0109651807
    ## Other_Shrub-in-Other_LAND_USE_YEAR_2000      0.0586271897    0.0110749308
    ## Other_Tree-in-Forest_Commodity               0.0904991848    0.0219148519
    ## Other_Tree-in-Natural_Forest                 0.3204275266    0.0206527965
    ## Other_Tree-in-Other_LAND_USE_YEAR_2000       0.1049615888    0.0186711213
    ## Pulpwood-in-Forest_Commodity                 0.4860913651    0.0655191655
    ## Pulpwood-in-Natural_Forest                   0.1946025124    0.0237487265
    ## Pulpwood-in-Other_LAND_USE_YEAR_2000         0.0784421949    0.0168564314
    ## Rice-in-Forest_Commodity                     0.0000000000    0.0000000000
    ## Rice-in-Natural_Forest                       0.0003637117    0.0003640869
    ## Rice-in-Other_LAND_USE_YEAR_2000             0.0034612125    0.0029606719
    ## Rubber-in-Forest_Commodity                   0.2655895391    0.0441095708
    ## Rubber-in-Natural_Forest                     0.0823488058    0.0139600668
    ## Rubber-in-Other_LAND_USE_YEAR_2000           0.1497670866    0.0216424964
    ## Tea-in-Forest_Commodity                      0.0053240517    0.0053463922
    ## Tea-in-Natural_Forest                        0.0022389672    0.0019065835
    ## Tea-in-Other_LAND_USE_YEAR_2000              0.0358319523    0.0111482773
    ## Water-in-Forest_Commodity                    0.0012612113    0.0010497643
    ## Water-in-Natural_Forest                      0.0063949096    0.0042406036
    ## Water-in-Other_LAND_USE_YEAR_2000            0.0351320843    0.0105426669

## Figure out areas and what not

``` r
#coverHA <- round(t(results$Cover$LAND_COVER) * countryArea, 0)
coverHA <- round(t(results$Cover$LAND_COVER) * sum(stratumAreas), 0)
colnames(coverHA) <- c("Area_ha", "SE_Area_ha")

#useHA <- round(t(results$Cover$LAND_USE) * countryArea, 0)
useHA <- round(t(results$Cover$LAND_USE) * sum(stratumAreas), 0)
colnames(useHA) <- c("Area_ha", "SE_Area_ha")

#use2000HA <- round(t(results$Cover$LAND_USE_YEAR_2000) * countryArea, 0)
use2000HA <- round(t(results$Cover$LAND_USE_YEAR_2000) * sum(stratumAreas), 0)
colnames(use2000HA) <- c("Area_ha", "SE_Area_ha")

coverHA
```

    ##                  Area_ha SE_Area_ha
    ## Bamboo              8182       2495
    ## Built_up           11866       2321
    ## Coffee             22794       3815
    ## Fruit_Nut          27630       4830
    ## Herbaceous         70537       6782
    ## Non_vegetated      40582       5380
    ## Other_Crop         97939       8895
    ## Other_LAND_COVER    8638       2454
    ## Other_Shrub        53110       5290
    ## Other_Tree        170770      10324
    ## Pulpwood          190392      12213
    ## Rice                1067        805
    ## Rubber            123204      10093
    ## Tea                11470       3267
    ## Water              12129       3293

``` r
useHA
```

    ##                           Area_ha SE_Area_ha
    ## Agrisiviculture            113017       9522
    ## Boundary_Agrisilviculture    9094       3012
    ## Mixed_Agrisilviculture      42741       6151
    ## Natural_Forest             173422      11006
    ## Other_LAND_USE             154905      10349
    ## Plantation                 334258      14022
    ## Silvopastoral               17915       3732
    ## Terrace                      4958       2342

``` r
use2000HA
```

    ##                          Area_ha SE_Area_ha
    ## Forest_Commodity          191947      12615
    ## Natural_Forest            391228      14556
    ## Other_LAND_USE_YEAR_2000  267135      13842

``` r
# one commodity example
rubberAreas <- use_in_rubber[,1] * coverHA["Rubber", "Area_ha"]
rubberAreas_SE <- rubberAreas * sqrt((use_in_rubber[,2]/use_in_rubber[,1])^2 + 
                     (coverHA["Rubber", "SE_Area_ha"]/coverHA["Rubber", "Area_ha"])^2)

use_in_rubber_ha <- round(cbind(rubberAreas, rubberAreas_SE), 2) 
colnames(use_in_rubber_ha) <- c("Area_ha", "SE_Area_ha")
use_in_rubber_ha
```

    ##                                       Area_ha SE_Area_ha
    ## Agrisiviculture-in-Rubber              413.02     310.76
    ## Boundary_Agrisilviculture-in-Rubber   1665.95    1284.82
    ## Mixed_Agrisilviculture-in-Rubber      3747.09    1765.31
    ## Natural_Forest-in-Rubber                96.09      68.87
    ## Other_LAND_USE-in-Rubber               437.97     440.30
    ## Plantation-in-Rubber                114508.01   18094.57
    ## Silvopastoral-in-Rubber               1167.93    1166.43
    ## Terrace-in-Rubber                     1167.93    1041.09

``` r
com_in_agslv
```

    ##                                            [,1]         [,2]
    ## Bamboo-in-Agrisiviculture           0.000000000 0.0000000000
    ## Built_up-in-Agrisiviculture         0.000209842 0.0002097289
    ## Coffee-in-Agrisiviculture           0.108897327 0.0255869759
    ## Fruit_Nut-in-Agrisiviculture        0.021695617 0.0142550024
    ## Herbaceous-in-Agrisiviculture       0.020373813 0.0090133157
    ## Non_vegetated-in-Agrisiviculture    0.085806691 0.0258396098
    ## Other_Crop-in-Agrisiviculture       0.662755397 0.0988481501
    ## Other_LAND_COVER-in-Agrisiviculture 0.010764695 0.0104042730
    ## Other_Shrub-in-Agrisiviculture      0.051656740 0.0167109528
    ## Other_Tree-in-Agrisiviculture       0.021300411 0.0089578501
    ## Pulpwood-in-Agrisiviculture         0.003444702 0.0034719311
    ## Rice-in-Agrisiviculture             0.009440220 0.0072100758
    ## Rubber-in-Agrisiviculture           0.003654544 0.0027481100
    ## Tea-in-Agrisiviculture              0.000000000 0.0000000000
    ## Water-in-Agrisiviculture            0.000000000 0.0000000000

``` r
com_in_forest 
```

    ##                                              [,1]         [,2]
    ## Bamboo-in-Forest_Commodity           0.0010141051 0.0010116916
    ## Built_up-in-Forest_Commodity         0.0029123421 0.0021594114
    ## Coffee-in-Forest_Commodity           0.0020282102 0.0020233833
    ## Fruit_Nut-in-Forest_Commodity        0.0149788829 0.0074250962
    ## Herbaceous-in-Forest_Commodity       0.0637248456 0.0143220122
    ## Non_vegetated-in-Forest_Commodity    0.0264068252 0.0103129793
    ## Other_Crop-in-Forest_Commodity       0.0174162726 0.0092777911
    ## Other_LAND_COVER-in-Forest_Commodity 0.0005070525 0.0005094052
    ## Other_Shrub-in-Forest_Commodity      0.0222461118 0.0086530115
    ## Other_Tree-in-Forest_Commodity       0.0904991848 0.0219148519
    ## Pulpwood-in-Forest_Commodity         0.4860913651 0.0655191655
    ## Rice-in-Forest_Commodity             0.0000000000 0.0000000000
    ## Rubber-in-Forest_Commodity           0.2655895391 0.0441095708
    ## Tea-in-Forest_Commodity              0.0053240517 0.0053463922
    ## Water-in-Forest_Commodity            0.0012612113 0.0010497643

``` r
p_hat_o_in_c3 * use2000HA[,1]
```

    ##                   Bamboo-in-Forest_Commodity 
    ##                                    194.65443 
    ##                     Bamboo-in-Natural_Forest 
    ##                                   7892.74998 
    ##           Bamboo-in-Other_LAND_USE_YEAR_2000 
    ##                                     94.86286 
    ##                 Built_up-in-Forest_Commodity 
    ##                                    559.01533 
    ##                   Built_up-in-Natural_Forest 
    ##                                   4101.91027 
    ##         Built_up-in-Other_LAND_USE_YEAR_2000 
    ##                                   7204.99961 
    ##                   Coffee-in-Forest_Commodity 
    ##                                    389.30886 
    ##                     Coffee-in-Natural_Forest 
    ##                                  11114.68996 
    ##           Coffee-in-Other_LAND_USE_YEAR_2000 
    ##                                  11289.63555 
    ##                Fruit_Nut-in-Forest_Commodity 
    ##                                   2875.15163 
    ##                  Fruit_Nut-in-Natural_Forest 
    ##                                   9520.50704 
    ##        Fruit_Nut-in-Other_LAND_USE_YEAR_2000 
    ##                                  15233.88771 
    ##               Herbaceous-in-Forest_Commodity 
    ##                                  12231.79293 
    ##                 Herbaceous-in-Natural_Forest 
    ##                                  21671.31873 
    ##       Herbaceous-in-Other_LAND_USE_YEAR_2000 
    ##                                  36633.91093 
    ##            Non_vegetated-in-Forest_Commodity 
    ##                                   5068.71088 
    ##              Non_vegetated-in-Natural_Forest 
    ##                                  17202.28034 
    ##    Non_vegetated-in-Other_LAND_USE_YEAR_2000 
    ##                                  18310.79393 
    ##               Other_Crop-in-Forest_Commodity 
    ##                                   3343.00127 
    ##                 Other_Crop-in-Natural_Forest 
    ##                                  44344.55647 
    ##       Other_Crop-in-Other_LAND_USE_YEAR_2000 
    ##                                  50251.10641 
    ##         Other_LAND_COVER-in-Forest_Commodity 
    ##                                     97.32722 
    ##           Other_LAND_COVER-in-Natural_Forest 
    ##                                   4969.53267 
    ## Other_LAND_COVER-in-Other_LAND_USE_YEAR_2000 
    ##                                   3571.23931 
    ##              Other_Shrub-in-Forest_Commodity 
    ##                                   4270.07442 
    ##                Other_Shrub-in-Natural_Forest 
    ##                                  33179.01525 
    ##      Other_Shrub-in-Other_LAND_USE_YEAR_2000 
    ##                                  15661.37432 
    ##               Other_Tree-in-Forest_Commodity 
    ##                                  17371.04702 
    ##                 Other_Tree-in-Natural_Forest 
    ##                                 125360.22039 
    ##       Other_Tree-in-Other_LAND_USE_YEAR_2000 
    ##                                  28038.91404 
    ##                 Pulpwood-in-Forest_Commodity 
    ##                                  93303.77926 
    ##                   Pulpwood-in-Natural_Forest 
    ##                                  76133.95171 
    ##         Pulpwood-in-Other_LAND_USE_YEAR_2000 
    ##                                  20954.65574 
    ##                     Rice-in-Forest_Commodity 
    ##                                      0.00000 
    ##                       Rice-in-Natural_Forest 
    ##                                    142.29419 
    ##             Rice-in-Other_LAND_USE_YEAR_2000 
    ##                                    924.61101 
    ##                   Rubber-in-Forest_Commodity 
    ##                                  50979.11526 
    ##                     Rubber-in-Natural_Forest 
    ##                                  32217.15861 
    ##           Rubber-in-Other_LAND_USE_YEAR_2000 
    ##                                  40008.03068 
    ##                      Tea-in-Forest_Commodity 
    ##                                   1021.93576 
    ##                        Tea-in-Natural_Forest 
    ##                                    875.94668 
    ##              Tea-in-Other_LAND_USE_YEAR_2000 
    ##                                   9571.96856 
    ##                    Water-in-Forest_Commodity 
    ##                                    242.08573 
    ##                      Water-in-Natural_Forest 
    ##                                   2501.86769 
    ##            Water-in-Other_LAND_USE_YEAR_2000 
    ##                                   9385.00933

``` r
p_hat_o_in_c3 * use2000HA[,1] * sqrt((se_p_hat_o_in_c3/p_hat_o_in_c3)^2 + (use2000HA[,2]/use2000HA[,1])^2)
```

    ##                   Bamboo-in-Forest_Commodity 
    ##                                    194.61211 
    ##                     Bamboo-in-Natural_Forest 
    ##                                   2536.77266 
    ##           Bamboo-in-Other_LAND_USE_YEAR_2000 
    ##                                     95.13991 
    ##                 Built_up-in-Forest_Commodity 
    ##                                    416.11757 
    ##                   Built_up-in-Natural_Forest 
    ##                                   1049.91206 
    ##         Built_up-in-Other_LAND_USE_YEAR_2000 
    ##                                   2154.43528 
    ##                   Coffee-in-Forest_Commodity 
    ##                                    389.22421 
    ##                     Coffee-in-Natural_Forest 
    ##                                   2828.80070 
    ##           Coffee-in-Other_LAND_USE_YEAR_2000 
    ##                                   2835.46142 
    ##                Fruit_Nut-in-Forest_Commodity 
    ##                                   1437.69660 
    ##                  Fruit_Nut-in-Natural_Forest 
    ##                                   2875.52328 
    ##        Fruit_Nut-in-Other_LAND_USE_YEAR_2000 
    ##                                   3940.62604 
    ##               Herbaceous-in-Forest_Commodity 
    ##                                   2864.19419 
    ##                 Herbaceous-in-Natural_Forest 
    ##                                   3857.99843 
    ##       Herbaceous-in-Other_LAND_USE_YEAR_2000 
    ##                                   6177.94906 
    ##            Non_vegetated-in-Forest_Commodity 
    ##                                   2007.37901 
    ##              Non_vegetated-in-Natural_Forest 
    ##                                   3545.11218 
    ##    Non_vegetated-in-Other_LAND_USE_YEAR_2000 
    ##                                   4105.41645 
    ##               Other_Crop-in-Forest_Commodity 
    ##                                   1794.34578 
    ##                 Other_Crop-in-Natural_Forest 
    ##                                   6106.78009 
    ##       Other_Crop-in-Other_LAND_USE_YEAR_2000 
    ##                                   8180.71709 
    ##         Other_LAND_COVER-in-Forest_Commodity 
    ##                                     97.98780 
    ##           Other_LAND_COVER-in-Natural_Forest 
    ##                                   1739.53798 
    ## Other_LAND_COVER-in-Other_LAND_USE_YEAR_2000 
    ##                                   1771.49705 
    ##              Other_Shrub-in-Forest_Commodity 
    ##                                   1684.46126 
    ##                Other_Shrub-in-Natural_Forest 
    ##                                   4463.96698 
    ##      Other_Shrub-in-Other_LAND_USE_YEAR_2000 
    ##                                   3067.78302 
    ##               Other_Tree-in-Forest_Commodity 
    ##                                   4358.66002 
    ##                 Other_Tree-in-Natural_Forest 
    ##                                   9329.51550 
    ##       Other_Tree-in-Other_LAND_USE_YEAR_2000 
    ##                                   5195.00782 
    ##                 Pulpwood-in-Forest_Commodity 
    ##                                  13991.53084 
    ##                   Pulpwood-in-Natural_Forest 
    ##                                   9713.37202 
    ##         Pulpwood-in-Other_LAND_USE_YEAR_2000 
    ##                                   4632.00268 
    ##                     Rice-in-Forest_Commodity 
    ##                                          NaN 
    ##                       Rice-in-Natural_Forest 
    ##                                    142.53936 
    ##             Rice-in-Other_LAND_USE_YEAR_2000 
    ##                                    792.34888 
    ##                   Rubber-in-Forest_Commodity 
    ##                                   9105.50746 
    ##                     Rubber-in-Natural_Forest 
    ##                                   5591.56008 
    ##           Rubber-in-Other_LAND_USE_YEAR_2000 
    ##                                   6141.90684 
    ##                      Tea-in-Forest_Commodity 
    ##                                   1028.41938 
    ##                        Tea-in-Natural_Forest 
    ##                                    746.62050 
    ##              Tea-in-Other_LAND_USE_YEAR_2000 
    ##                                   3019.11446 
    ##                    Water-in-Forest_Commodity 
    ##                                    202.12625 
    ##                      Water-in-Natural_Forest 
    ##                                   1661.65216 
    ##            Water-in-Other_LAND_USE_YEAR_2000 
    ##                                   2857.99197

``` r
com_in_2000_Areas <- calcError(com_in_2000, use2000HA)

contents <- str_split(rownames(com_in_2000_Areas), "-", 3, simplify = T)
data <- tibble(condition = contents[,3], cover = contents[,1], 
                  area_ha = com_in_2000_Areas[,1], se_area = com_in_2000_Areas[,2])
conditions <- unique(data$condition)

prettyTable <- list()
for (c in seq_along(conditions)) {
  prettyTable[[c]] <- filter(data, condition == conditions[c])
  prettyTable[[c]] <- prettyTable[[c]][,-1]
  names(prettyTable)[c] <- conditions[c]
}

prettyTable
```

    ## $Forest_Commodity
    ## # A tibble: 15 x 3
    ##    cover            area_ha se_area
    ##    <chr>              <dbl>   <dbl>
    ##  1 Bamboo             195.    195. 
    ##  2 Built_up           559.    416. 
    ##  3 Coffee             389.    389. 
    ##  4 Fruit_Nut         2875.   1438. 
    ##  5 Herbaceous       12232.   2864. 
    ##  6 Non_vegetated     5069.   2007. 
    ##  7 Other_Crop        3343.   1794. 
    ##  8 Other_LAND_COVER    97.3    98.0
    ##  9 Other_Shrub       4270.   1684. 
    ## 10 Other_Tree       17371.   4359. 
    ## 11 Pulpwood         93304.  13992. 
    ## 12 Rice                 0     NaN  
    ## 13 Rubber           50979.   9106. 
    ## 14 Tea               1022.   1028. 
    ## 15 Water              242.    202. 
    ## 
    ## $Natural_Forest
    ## # A tibble: 15 x 3
    ##    cover            area_ha se_area
    ##    <chr>              <dbl>   <dbl>
    ##  1 Bamboo             7893.   2537.
    ##  2 Built_up           4102.   1050.
    ##  3 Coffee            11115.   2829.
    ##  4 Fruit_Nut          9521.   2876.
    ##  5 Herbaceous        21671.   3858.
    ##  6 Non_vegetated     17202.   3545.
    ##  7 Other_Crop        44345.   6107.
    ##  8 Other_LAND_COVER   4970.   1740.
    ##  9 Other_Shrub       33179.   4464.
    ## 10 Other_Tree       125360.   9330.
    ## 11 Pulpwood          76134.   9713.
    ## 12 Rice                142.    143.
    ## 13 Rubber            32217.   5592.
    ## 14 Tea                 876.    747.
    ## 15 Water              2502.   1662.
    ## 
    ## $Other_LAND_USE_YEAR_2000
    ## # A tibble: 15 x 3
    ##    cover            area_ha se_area
    ##    <chr>              <dbl>   <dbl>
    ##  1 Bamboo              94.9    95.1
    ##  2 Built_up          7205.   2154. 
    ##  3 Coffee           11290.   2835. 
    ##  4 Fruit_Nut        15234.   3941. 
    ##  5 Herbaceous       36634.   6178. 
    ##  6 Non_vegetated    18311.   4105. 
    ##  7 Other_Crop       50251.   8181. 
    ##  8 Other_LAND_COVER  3571.   1771. 
    ##  9 Other_Shrub      15661.   3068. 
    ## 10 Other_Tree       28039.   5195. 
    ## 11 Pulpwood         20955.   4632. 
    ## 12 Rice               925.    792. 
    ## 13 Rubber           40008.   6142. 
    ## 14 Tea               9572.   3019. 
    ## 15 Water             9385.   2858.

``` r
# Test triple conditional 

questions <- c("LAND_COVER", "LAND_USE", "LAND_USE_YEAR_2000")
crops <- c("Coffee", "Fruit_Nut" ,"Pulpwood", "Rubber", "Tea", "Other_Crop")

y_occ_test <- build_yocc(test, mtdt = metaNames, qstns = questions, 
            cvrfld = "LAND_COVER", cndtnfld1 = "LAND_USE", 
            cndtnfld2 = "LAND_USE_YEAR_2000", covers = crops, conditions1 = NULL,
            conditions2 = "Natural_Forest")
```

    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")
    ## Joining, by = c("PLOT_ID", "SAMPLE_ID", "USER_ID", "PL_PLOTID", "PL_PACKETID", "PL_COUNTRY", "PL_STRATUM")

``` r
sum(test$LAND_COVER == "Pulpwood" & test$LAND_USE == "Plantation" & 
      test$LAND_USE_YEAR_2000 == "Natural_Forest")
```

    ## [1] 1841

``` r
yoccaTest <- do_yocc_analysis(y_occ_test, mtdt = metaNames, strata = strata, 
                              areas = stratumAreas, ns = sampSize, qstns = questions, 
                              grplst = groupList)

cResults <- bind_rows(yoccaTest$Cover)

cResults %>% 
  select(cover, condition1, PercentCover) %>% 
  mutate(PercentCover = PercentCover * sum(stratumAreas)) %>% 
  spread(cover, PercentCover)
```

    ## # A tibble: 8 x 7
    ##   condition1              Coffee Fruit_Nut Other_Crop Pulpwood Rubber   Tea
    ##   <chr>                    <dbl>     <dbl>      <dbl>    <dbl>  <dbl> <dbl>
    ## 1 Agrisiviculture        16466.      2710.     79457.     764. 8.10e2    0 
    ## 2 Boundary_Agrisilvicul~    46.5        0          0        0  9.77e2  286.
    ## 3 Mixed_Agrisilviculture  1697.     12234.         0      279. 1.62e3    0 
    ## 4 Natural_Forest             0        233.         0     4678. 9.31e1    0 
    ## 5 Other_LAND_USE          3596.       279.      4899.     477. 8.59e2    0 
    ## 6 Plantation                 0       3222.      1210.  143166. 5.88e4    0 
    ## 7 Silvopastoral              0          0       1432.       0  0.        0 
    ## 8 Terrace                    0          0          0        0  0.     1432.
