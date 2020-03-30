QA/QC for Training Data
================
MS Patterson, <tertiarymatt@gmail.com>
April 18, 2019

Set working directory to where data is being stored.

``` r
setwd("~/R/GIA/")
```

### Required packages

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.5
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(irr)
```

    ## Loading required package: lpSolve

``` r
library(knitr)
```

### Import Data

``` r
hieu <- read_csv("data/ceo-hieu-nguyen-practice-v2-plot-data-2019-04-18.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   PLOT_ID = col_integer(),
    ##   SHAPE = col_character(),
    ##   FLAGGED = col_character(),
    ##   ANALYSES = col_integer(),
    ##   SAMPLE_POINTS = col_integer(),
    ##   USER_ID = col_character(),
    ##   COLLECTION_TIME = col_character(),
    ##   PL_PLOTID = col_integer(),
    ##   PL_PACKETID = col_integer(),
    ##   PL_COUNTRY = col_character(),
    ##   PL_STRATUM = col_integer()
    ## )

    ## See spec(...) for full column specifications.

``` r
hung <- read_csv("data/ceo-hung-le-practice-v2-plot-data-2019-04-18.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   PLOT_ID = col_integer(),
    ##   SHAPE = col_character(),
    ##   FLAGGED = col_character(),
    ##   ANALYSES = col_integer(),
    ##   SAMPLE_POINTS = col_integer(),
    ##   USER_ID = col_character(),
    ##   COLLECTION_TIME = col_character(),
    ##   PL_PLOTID = col_integer(),
    ##   PL_PACKETID = col_integer(),
    ##   PL_COUNTRY = col_character(),
    ##   PL_STRATUM = col_integer()
    ## )
    ## See spec(...) for full column specifications.

``` r
jan <- read_csv("data/ceo-jan-johnson-practice-v2-plot-data-2019-04-18.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   PLOT_ID = col_integer(),
    ##   SHAPE = col_character(),
    ##   FLAGGED = col_character(),
    ##   ANALYSES = col_integer(),
    ##   SAMPLE_POINTS = col_integer(),
    ##   USER_ID = col_character(),
    ##   COLLECTION_TIME = col_character(),
    ##   PL_PLOTID = col_integer(),
    ##   PL_PACKETID = col_integer(),
    ##   PL_COUNTRY = col_character(),
    ##   PL_STRATUM = col_integer()
    ## )
    ## See spec(...) for full column specifications.

``` r
mark <- read_csv("data/ceo-mark-hammond-practice-v2-plot-data-2019-04-18.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   PLOT_ID = col_integer(),
    ##   SHAPE = col_character(),
    ##   FLAGGED = col_character(),
    ##   ANALYSES = col_integer(),
    ##   SAMPLE_POINTS = col_integer(),
    ##   USER_ID = col_character(),
    ##   COLLECTION_TIME = col_character(),
    ##   PL_PLOTID = col_integer(),
    ##   PL_PACKETID = col_integer(),
    ##   PL_COUNTRY = col_character(),
    ##   PL_STRATUM = col_integer()
    ## )
    ## See spec(...) for full column specifications.

``` r
ryan <- read_csv("data/ceo-ryan-thomas-practice-v2-plot-data-2019-04-18.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   PLOT_ID = col_integer(),
    ##   SHAPE = col_character(),
    ##   FLAGGED = col_character(),
    ##   ANALYSES = col_integer(),
    ##   SAMPLE_POINTS = col_integer(),
    ##   USER_ID = col_character(),
    ##   COLLECTION_TIME = col_character(),
    ##   PL_PLOTID = col_integer(),
    ##   PL_PACKETID = col_integer(),
    ##   PL_COUNTRY = col_character(),
    ##   PL_STRATUM = col_integer()
    ## )
    ## See spec(...) for full column specifications.

``` r
vu <- read_csv("data/ceo-vu-quy-practice-v2-plot-data-2019-04-18.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   PLOT_ID = col_integer(),
    ##   SHAPE = col_character(),
    ##   FLAGGED = col_character(),
    ##   ANALYSES = col_integer(),
    ##   SAMPLE_POINTS = col_integer(),
    ##   USER_ID = col_character(),
    ##   COLLECTION_TIME = col_character(),
    ##   PL_PLOTID = col_integer(),
    ##   PL_PACKETID = col_integer(),
    ##   PL_COUNTRY = col_character(),
    ##   PL_STRATUM = col_integer()
    ## )
    ## See spec(...) for full column specifications.

``` r
crossData <- rbind(hieu, hung, vu, jan, mark, ryan)
crossData <- na.omit(crossData)
```

### Process data into form that can be used for irr

Provide overview of the process.

``` r
# Find and extract the class names. 
names(crossData)
```

    ##  [1] "PLOT_ID"                             
    ##  [2] "CENTER_LON"                          
    ##  [3] "CENTER_LAT"                          
    ##  [4] "SIZE_M"                              
    ##  [5] "SHAPE"                               
    ##  [6] "FLAGGED"                             
    ##  [7] "ANALYSES"                            
    ##  [8] "SAMPLE_POINTS"                       
    ##  [9] "USER_ID"                             
    ## [10] "COLLECTION_TIME"                     
    ## [11] "ANALYSIS_DURATION"                   
    ## [12] "PL_LONGITUDE"                        
    ## [13] "PL_LATITUDE"                         
    ## [14] "PL_PLOTID"                           
    ## [15] "PL_PACKETID"                         
    ## [16] "PL_COUNTRY"                          
    ## [17] "PL_STRATUM"                          
    ## [18] "LAND COVER:RUBBER"                   
    ## [19] "LAND COVER:PULP"                     
    ## [20] "LAND COVER:FRUIT/NUT"                
    ## [21] "LAND COVER:OTHER MATURE TREE"        
    ## [22] "LAND COVER:OTHER IMMATURE TREE"      
    ## [23] "LAND COVER:COFFEE"                   
    ## [24] "LAND COVER:TEA"                      
    ## [25] "LAND COVER:OTHER SHRUB"              
    ## [26] "LAND COVER:OIL PALM"                 
    ## [27] "LAND COVER:COCONUT"                  
    ## [28] "LAND COVER:BANANA"                   
    ## [29] "LAND COVER:BAMBOO"                   
    ## [30] "LAND COVER:OTHER MATURE PALM"        
    ## [31] "LAND COVER:IMMATURE PALM"            
    ## [32] "LAND COVER:HERBACEOUS"               
    ## [33] "LAND COVER:RICE"                     
    ## [34] "LAND COVER:OTHER CROP"               
    ## [35] "LAND COVER:NON-VEGETATED"            
    ## [36] "LAND COVER:AQUACULTURE"              
    ## [37] "LAND COVER:WATER"                    
    ## [38] "LAND COVER:BUILT-UP"                 
    ## [39] "LAND COVER:OTHER"                    
    ## [40] "UNDERSTORY PRESENT?:YES"             
    ## [41] "UNDERSTORY PRESENT?:NO"              
    ## [42] "UNDERSTORY COVER:COFFEE"             
    ## [43] "UNDERSTORY COVER:TEA"                
    ## [44] "UNDERSTORY COVER:RICE"               
    ## [45] "UNDERSTORY COVER:OTHER CROP"         
    ## [46] "UNDERSTORY COVER:WATER"              
    ## [47] "LAND USE:AGRISILVICULTURE"           
    ## [48] "LAND USE:PLANTATION/WOODLOT"         
    ## [49] "LAND USE:TERRACE"                    
    ## [50] "LAND USE:SILVAPASTORAL/PARKLAND"     
    ## [51] "LAND USE:SHADOW SYSTEMS"             
    ## [52] "LAND USE:HOME GARDENS"               
    ## [53] "LAND USE:OTHER AGROFORESTRY"         
    ## [54] "LAND USE:NATURAL FOREST"             
    ## [55] "LAND USE:NON-AGROFORESTRY"           
    ## [56] "LAND USE YEAR 2000:NATURAL FOREST"   
    ## [57] "LAND USE YEAR 2000:PLANTATION FOREST"
    ## [58] "LAND USE YEAR 2000:NON-FOREST"

``` r
# Set columns with classnames
classCol <- c(18:58)

#clean up
classes <- colnames(crossData[classCol]) %>% 
    gsub(":", "_", .) %>%
    gsub(" ", "_", .) %>% 
    gsub("/", "_", .)

classes
```

    ##  [1] "LAND_COVER_RUBBER"                   
    ##  [2] "LAND_COVER_PULP"                     
    ##  [3] "LAND_COVER_FRUIT_NUT"                
    ##  [4] "LAND_COVER_OTHER_MATURE_TREE"        
    ##  [5] "LAND_COVER_OTHER_IMMATURE_TREE"      
    ##  [6] "LAND_COVER_COFFEE"                   
    ##  [7] "LAND_COVER_TEA"                      
    ##  [8] "LAND_COVER_OTHER_SHRUB"              
    ##  [9] "LAND_COVER_OIL_PALM"                 
    ## [10] "LAND_COVER_COCONUT"                  
    ## [11] "LAND_COVER_BANANA"                   
    ## [12] "LAND_COVER_BAMBOO"                   
    ## [13] "LAND_COVER_OTHER_MATURE_PALM"        
    ## [14] "LAND_COVER_IMMATURE_PALM"            
    ## [15] "LAND_COVER_HERBACEOUS"               
    ## [16] "LAND_COVER_RICE"                     
    ## [17] "LAND_COVER_OTHER_CROP"               
    ## [18] "LAND_COVER_NON-VEGETATED"            
    ## [19] "LAND_COVER_AQUACULTURE"              
    ## [20] "LAND_COVER_WATER"                    
    ## [21] "LAND_COVER_BUILT-UP"                 
    ## [22] "LAND_COVER_OTHER"                    
    ## [23] "UNDERSTORY_PRESENT?_YES"             
    ## [24] "UNDERSTORY_PRESENT?_NO"              
    ## [25] "UNDERSTORY_COVER_COFFEE"             
    ## [26] "UNDERSTORY_COVER_TEA"                
    ## [27] "UNDERSTORY_COVER_RICE"               
    ## [28] "UNDERSTORY_COVER_OTHER_CROP"         
    ## [29] "UNDERSTORY_COVER_WATER"              
    ## [30] "LAND_USE_AGRISILVICULTURE"           
    ## [31] "LAND_USE_PLANTATION_WOODLOT"         
    ## [32] "LAND_USE_TERRACE"                    
    ## [33] "LAND_USE_SILVAPASTORAL_PARKLAND"     
    ## [34] "LAND_USE_SHADOW_SYSTEMS"             
    ## [35] "LAND_USE_HOME_GARDENS"               
    ## [36] "LAND_USE_OTHER_AGROFORESTRY"         
    ## [37] "LAND_USE_NATURAL_FOREST"             
    ## [38] "LAND_USE_NON-AGROFORESTRY"           
    ## [39] "LAND_USE_YEAR_2000_NATURAL_FOREST"   
    ## [40] "LAND_USE_YEAR_2000_PLANTATION_FOREST"
    ## [41] "LAND_USE_YEAR_2000_NON-FOREST"

``` r
colnames(crossData)[classCol] <- classes
names(crossData)
```

    ##  [1] "PLOT_ID"                             
    ##  [2] "CENTER_LON"                          
    ##  [3] "CENTER_LAT"                          
    ##  [4] "SIZE_M"                              
    ##  [5] "SHAPE"                               
    ##  [6] "FLAGGED"                             
    ##  [7] "ANALYSES"                            
    ##  [8] "SAMPLE_POINTS"                       
    ##  [9] "USER_ID"                             
    ## [10] "COLLECTION_TIME"                     
    ## [11] "ANALYSIS_DURATION"                   
    ## [12] "PL_LONGITUDE"                        
    ## [13] "PL_LATITUDE"                         
    ## [14] "PL_PLOTID"                           
    ## [15] "PL_PACKETID"                         
    ## [16] "PL_COUNTRY"                          
    ## [17] "PL_STRATUM"                          
    ## [18] "LAND_COVER_RUBBER"                   
    ## [19] "LAND_COVER_PULP"                     
    ## [20] "LAND_COVER_FRUIT_NUT"                
    ## [21] "LAND_COVER_OTHER_MATURE_TREE"        
    ## [22] "LAND_COVER_OTHER_IMMATURE_TREE"      
    ## [23] "LAND_COVER_COFFEE"                   
    ## [24] "LAND_COVER_TEA"                      
    ## [25] "LAND_COVER_OTHER_SHRUB"              
    ## [26] "LAND_COVER_OIL_PALM"                 
    ## [27] "LAND_COVER_COCONUT"                  
    ## [28] "LAND_COVER_BANANA"                   
    ## [29] "LAND_COVER_BAMBOO"                   
    ## [30] "LAND_COVER_OTHER_MATURE_PALM"        
    ## [31] "LAND_COVER_IMMATURE_PALM"            
    ## [32] "LAND_COVER_HERBACEOUS"               
    ## [33] "LAND_COVER_RICE"                     
    ## [34] "LAND_COVER_OTHER_CROP"               
    ## [35] "LAND_COVER_NON-VEGETATED"            
    ## [36] "LAND_COVER_AQUACULTURE"              
    ## [37] "LAND_COVER_WATER"                    
    ## [38] "LAND_COVER_BUILT-UP"                 
    ## [39] "LAND_COVER_OTHER"                    
    ## [40] "UNDERSTORY_PRESENT?_YES"             
    ## [41] "UNDERSTORY_PRESENT?_NO"              
    ## [42] "UNDERSTORY_COVER_COFFEE"             
    ## [43] "UNDERSTORY_COVER_TEA"                
    ## [44] "UNDERSTORY_COVER_RICE"               
    ## [45] "UNDERSTORY_COVER_OTHER_CROP"         
    ## [46] "UNDERSTORY_COVER_WATER"              
    ## [47] "LAND_USE_AGRISILVICULTURE"           
    ## [48] "LAND_USE_PLANTATION_WOODLOT"         
    ## [49] "LAND_USE_TERRACE"                    
    ## [50] "LAND_USE_SILVAPASTORAL_PARKLAND"     
    ## [51] "LAND_USE_SHADOW_SYSTEMS"             
    ## [52] "LAND_USE_HOME_GARDENS"               
    ## [53] "LAND_USE_OTHER_AGROFORESTRY"         
    ## [54] "LAND_USE_NATURAL_FOREST"             
    ## [55] "LAND_USE_NON-AGROFORESTRY"           
    ## [56] "LAND_USE_YEAR_2000_NATURAL_FOREST"   
    ## [57] "LAND_USE_YEAR_2000_PLANTATION_FOREST"
    ## [58] "LAND_USE_YEAR_2000_NON-FOREST"

``` r
# Process data into form that can be used for irr
# to do this, need to prepare a list of data frames with values for each class
# this is then fed to the irr functions. 
cross_tables <- rep(list(NA),length(classes))
names(cross_tables) <- classes

for (m in 1:length(cross_tables)) {
  cross_tables[[m]] <- select(crossData, "USER_ID", "PL_PLOTID", classes[m]) %>%
    spread(., "USER_ID", classes[m]) %>%
    .[,-1] %>%
    na.omit(.) %>% 
    as.matrix(.)
}
```

### Calculate Metrics of Agreement

**Iota** is used to calculate overall agreement between raters, and represents an overall look at how close they agree. A more granular approach is necessary to improve agreement, but iota provides a useful summary.

#### Citations

1.  Conger, A.J. (1980). Integration and generalisation of Kappas for multiple raters. Psychological Bulletin, 88, 322-328.
2.  Janson, H., & Olsson, U. (2001). A measure of agreement for interval or nominal multivariate observations. Educational and Psychological Measurement, 61, 277-289.

``` r
crossval_iota <- iota(cross_tables, scaledata = "q")
crossval_iota
```

    ##  iota for quantitative data (41 variables)
    ## 
    ##  Subjects = 22 
    ##    Raters = 6 
    ##      iota = 0.193

For checking agreement of individual classes, we can use several approaches.
The **intraclass correlation coefficient** and **mean bivariate Pearson's** are two. The ICC is used to measure consistency between two raters, and uses an F-test to test for significance.

#### Citations

1.  Bartko, J.J. (1966). The intraclass correlation coefficient as a measure of reliability. Psychological Reports, 19, 3-11.
2.  McGraw, K.O., & Wong, S.P. (1996), Forming inferences about some intraclass correlation coefficients. Psychological Methods, 1, 30-46.
3.  Shrout, P.E., & Fleiss, J.L. (1979), Intraclass correlation: uses in assessing rater reliability. Psychological Bulletin, 86, 420-428.

``` r
# Intraclass Correlation Coefficient
cross_icc <- list()
for (m in 1:length(cross_tables)) {
  cross_icc[[m]] <- icc(cross_tables[[m]], model = "oneway", type = "agreement")
}
names(cross_icc) <- classes

# make a "table" from data values in list
icc_values <- data_frame(length(classes), 7)
for (m in 1:length(cross_icc)) {
  icc_values[m,1] <- names(cross_icc[m])
  icc_values[m,2] <- round(cross_icc[[m]]$value, 4)
  icc_values[m,3] <- round(cross_icc[[m]]$lbound, 4)
  icc_values[m,4] <- round(cross_icc[[m]]$ubound, 4)
  icc_values[m,5] <- round(cross_icc[[m]]$p.value, 4)
  icc_values[m,6] <- cross_icc[[m]]$subjects
  icc_values[m,7] <- cross_icc[[m]]$raters
}

colnames(icc_values) <- c("Class", "ICC", "Lower", "Upper", "ICC Pvalue", "n", "raters")

# # Mean Bivariate Pearson's
# cross_cor <- list()
# for (m in 1:length(cross_tables)) {
#   cross_cor[[m]] <- meancor(cross_tables[[m]])
# }
# names(cross_cor) <- classes
# 
# # make a "table" from data values in list
# cor_values <- data_frame(length(classes), 3)
# for (m in 1:length(cross_icc)) {
#   cor_values[m,1] <- names(cross_cor[m])
#   cor_values[m,2] <- round(cross_cor[[m]]$value, 4)
#   cor_values[m,3] <- round(cross_cor[[m]]$p.value, 4)
# }
# colnames(cor_values) <- c("Class", "Cor", "Cor Pvalue")

# Assemble tables into one object for display. 
#kable(bind_cols(cor_values, icc_values[,2:7]))
kable(icc_values)
```

| Class                                     |      ICC|    Lower|   Upper|  ICC Pvalue|    n|  raters|
|:------------------------------------------|--------:|--------:|-------:|-----------:|----:|-------:|
| LAND\_COVER\_RUBBER                       |   0.7166|   0.5683|  0.8473|      0.0000|   22|       6|
| LAND\_COVER\_PULP                         |   0.0636|  -0.0390|  0.2486|      0.1301|   22|       6|
| LAND\_COVER\_FRUIT\_NUT                   |      NaN|      NaN|     NaN|         NaN|   22|       6|
| LAND\_COVER\_OTHER\_MATURE\_TREE          |   0.4515|   0.2743|  0.6590|      0.0000|   22|       6|
| LAND\_COVER\_OTHER\_IMMATURE\_TREE        |  -0.0030|  -0.0829|  0.1529|      0.4905|   22|       6|
| LAND\_COVER\_COFFEE                       |  -0.0244|  -0.0966|  0.1199|      0.6446|   22|       6|
| LAND\_COVER\_TEA                          |   0.1491|   0.0210|  0.3582|      0.0089|   22|       6|
| LAND\_COVER\_OTHER\_SHRUB                 |   0.1120|  -0.0056|  0.3123|      0.0324|   22|       6|
| LAND\_COVER\_OIL\_PALM                    |      NaN|      NaN|     NaN|         NaN|   22|       6|
| LAND\_COVER\_COCONUT                      |      NaN|      NaN|     NaN|         NaN|   22|       6|
| LAND\_COVER\_BANANA                       |   0.0000|  -0.0810|  0.1574|      0.4695|   22|       6|
| LAND\_COVER\_BAMBOO                       |      NaN|      NaN|     NaN|         NaN|   22|       6|
| LAND\_COVER\_OTHER\_MATURE\_PALM          |  -0.0080|  -0.0862|  0.1452|      0.5266|   22|       6|
| LAND\_COVER\_IMMATURE\_PALM               |      NaN|      NaN|     NaN|         NaN|   22|       6|
| LAND\_COVER\_HERBACEOUS                   |  -0.0119|  -0.0886|  0.1393|      0.5545|   22|       6|
| LAND\_COVER\_RICE                         |      NaN|      NaN|     NaN|         NaN|   22|       6|
| LAND\_COVER\_OTHER\_CROP                  |   0.0654|  -0.0378|  0.2511|      0.1243|   22|       6|
| LAND\_COVER\_NON-VEGETATED                |   0.1295|   0.0068|  0.3343|      0.0180|   22|       6|
| LAND\_COVER\_AQUACULTURE                  |      NaN|      NaN|     NaN|         NaN|   22|       6|
| LAND\_COVER\_WATER                        |   0.5781|   0.4044|  0.7557|      0.0000|   22|       6|
| LAND\_COVER\_BUILT-UP                     |  -0.0126|  -0.0891|  0.1381|      0.5601|   22|       6|
| LAND\_COVER\_OTHER                        |  -0.0719|  -0.1260|  0.0425|      0.9128|   22|       6|
| UNDERSTORY\_PRESENT?\_YES                 |      NaN|      NaN|     NaN|         NaN|   22|       6|
| UNDERSTORY\_PRESENT?\_NO                  |      NaN|      NaN|     NaN|         NaN|   22|       6|
| UNDERSTORY\_COVER\_COFFEE                 |      NaN|      NaN|     NaN|         NaN|   22|       6|
| UNDERSTORY\_COVER\_TEA                    |      NaN|      NaN|     NaN|         NaN|   22|       6|
| UNDERSTORY\_COVER\_RICE                   |      NaN|      NaN|     NaN|         NaN|   22|       6|
| UNDERSTORY\_COVER\_OTHER\_CROP            |      NaN|      NaN|     NaN|         NaN|   22|       6|
| UNDERSTORY\_COVER\_WATER                  |      NaN|      NaN|     NaN|         NaN|   22|       6|
| LAND\_USE\_AGRISILVICULTURE               |   0.1579|   0.0275|  0.3689|      0.0064|   22|       6|
| LAND\_USE\_PLANTATION\_WOODLOT            |  -0.0102|  -0.0876|  0.1419|      0.5425|   22|       6|
| LAND\_USE\_TERRACE                        |  -0.0080|  -0.0862|  0.1452|      0.5266|   22|       6|
| LAND\_USE\_SILVAPASTORAL\_PARKLAND        |      NaN|      NaN|     NaN|         NaN|   22|       6|
| LAND\_USE\_SHADOW\_SYSTEMS                |   0.0000|  -0.0810|  0.1574|      0.4695|   22|       6|
| LAND\_USE\_HOME\_GARDENS                  |   0.0000|  -0.0810|  0.1574|      0.4695|   22|       6|
| LAND\_USE\_OTHER\_AGROFORESTRY            |  -0.0244|  -0.0966|  0.1199|      0.6446|   22|       6|
| LAND\_USE\_NATURAL\_FOREST                |   0.4210|   0.2454|  0.6335|      0.0000|   22|       6|
| LAND\_USE\_NON-AGROFORESTRY               |  -0.0509|  -0.1131|  0.0775|      0.8158|   22|       6|
| LAND\_USE\_YEAR\_2000\_NATURAL\_FOREST    |   0.1885|   0.0502|  0.4045|      0.0018|   22|       6|
| LAND\_USE\_YEAR\_2000\_PLANTATION\_FOREST |  -0.0137|  -0.0898|  0.1365|      0.5677|   22|       6|
| LAND\_USE\_YEAR\_2000\_NON-FOREST         |   0.1770|   0.0416|  0.3913|      0.0030|   22|       6|
