Scratch File for Random Needs
================
MS Patterson, <tertiarymatt@gmail.com>
May 28, 2019

Set working directory to where data is being stored.

``` r
#setwd("~/R/GIA/")
```

### Required packages

``` r
library(rlang)
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
    ## x purrr::%@%()         masks rlang::%@%()
    ## x purrr::as_function() masks rlang::as_function()
    ## x dplyr::filter()      masks stats::filter()
    ## x purrr::flatten()     masks rlang::flatten()
    ## x purrr::flatten_chr() masks rlang::flatten_chr()
    ## x purrr::flatten_dbl() masks rlang::flatten_dbl()
    ## x purrr::flatten_int() masks rlang::flatten_int()
    ## x purrr::flatten_lgl() masks rlang::flatten_lgl()
    ## x purrr::flatten_raw() masks rlang::flatten_raw()
    ## x purrr::invoke()      masks rlang::invoke()
    ## x dplyr::lag()         masks stats::lag()
    ## x purrr::list_along()  masks rlang::list_along()
    ## x purrr::modify()      masks rlang::modify()
    ## x purrr::splice()      masks rlang::splice()

``` r
library(knitr)
source("00_GIA_functions.R")
```

``` r
jan <- read_csv("Data/Vietnam/ceo-vn70403-sample-data-2019-05-08.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   FLAGGED = col_logical(),
    ##   USER_ID = col_character(),
    ##   COLLECTION_TIME = col_time(format = ""),
    ##   IMAGERY_TITLE = col_character(),
    ##   PL_COUNTRY = col_character(),
    ##   `LAND COVER` = col_character(),
    ##   `UNDERSTORY PRESENT?` = col_character(),
    ##   `UNDERSTORY COVER` = col_logical(),
    ##   `LAND USE` = col_character(),
    ##   `LAND USE YEAR 2000` = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
colnames(jan)
```

    ##  [1] "PLOT_ID"             "SAMPLE_ID"           "LON"                
    ##  [4] "LAT"                 "FLAGGED"             "ANALYSES"           
    ##  [7] "USER_ID"             "COLLECTION_TIME"     "ANALYSIS_DURATION"  
    ## [10] "IMAGERY_TITLE"       "PL_PLOTID"           "PL_PACKETID"        
    ## [13] "PL_ORIGID"           "PL_COUNTRY"          "PL_STRATUM"         
    ## [16] "LAND COVER"          "UNDERSTORY PRESENT?" "UNDERSTORY COVER"   
    ## [19] "LAND USE"            "LAND USE YEAR 2000"

``` r
jan$`LAND USE`[jan$FLAGGED == FALSE & jan$`LAND COVER` == 'Rubber'& 
                 jan$`LAND USE` == 'Other'] <- "Plantation"
write_csv(jan, "Data/Vietnam/ceo-vn70403-sample-data-2019-05-08.csv")
```

``` r
stratum3 <- read_csv("Data/Stratum3/VietnamStrata3.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   X = col_double(),
    ##   Y = col_double(),
    ##   ID = col_double(),
    ##   Dynamics = col_character(),
    ##   Notes = col_character()
    ## )

``` r
colnames(stratum3)
```

    ## [1] "X"        "Y"        "ID"       "Dynamics" "Notes"

``` r
stratum3 <- stratum3[,-5]
unique(stratum3$Dynamics)
```

    ## [1] "Tree_cover_gain"             "Tree_cover_gain_partial"    
    ## [3] "Tree_cover_loss"             "Tree_cover_loss_partial"    
    ## [5] "Tree_cover_rotation"         "Tree_cover_rotation_partial"
    ## [7] "Nonforest"

``` r
sum(stratum3$Dynamics == "Tree_cover_loss" | stratum3$Dynamics == "Tree_cover_loss_partial")
```

    ## [1] 26

``` r
plots <- sample_n(stratum3, 250)
sum(plots$Dynamics == "Tree_cover_loss" | plots$Dynamics == "Tree_cover_loss_partial")
```

    ## [1] 15

``` r
colnames(plots) <- c("LON", "LAT", "PL_ORIGID", "Dynamics")
plots <- add_column(plots,  PLOT_ID = 1:250, SAMPLE_ID = na_dbl, FLAGGED = FALSE, 
           ANALYSES = 0, USER_ID = 'potapove@user.com',
           COLLECTION_TIME = 0, ANALYSIS_DURATION = 0, IMAGERY_TITLE = na_chr, 
           PL_PLOTID = na_dbl, PL_PACKETID = na_dbl, PL_COUNTRY = 'Vietnam', 
           PL_STRATUM = 3, `LAND COVER` = "Other", 
           `UNDERSTORY PRESENT?` = 'No', `UNDERSTORY COVER` = na_chr, 
           `LAND USE` = 'Other', `LAND USE YEAR 2000`= 'Other')
plots <- select(plots, colnames(jan), everything())
points <- map_dfc(plots, rep, 24)
points <- arrange(points, PLOT_ID)
points$SAMPLE_ID <- 1:24
```

``` r
dataPath <- c("data/Vietnam")
files <- dir(dataPath)

rawData <- files %>% 
  map_dfr(~ read_csv(file.path(dataPath, .), col_types = "ddddldc_dcdddcdccccc"))

# find plots in stratum 3 that are already PIed
removeS3 <- points$PL_ORIGID %in% rawData$PL_ORIGID &
      points$Dynamics == "Tree_cover_loss" | points$Dynamics == "Tree_cover_loss_partial"

# find stratum 3 plots in CEO data
dupes <-   rawData$PL_ORIGID %in% points$PL_ORIGID &
  (rawData$PL_STRATUM == 3 | rawData$PL_STRATUM == 3)
  

# Remove points already in CEO data from 'fake' stratum 3 
points <- points[!removeS3,]
points <- points[,-21]

# Remove points NOT in 'fake' stratum 3 from CEO
removeCEOS3 <- rawData$PL_STRATUM == 3 & dupes == FALSE
cleanData <- rawData[!removeCEOS3,]

#Write out Stratum3 points
write_csv(points, "Data/Stratum3/VietnamStratum3_points.csv")

#build clean data table
points <- select(points, colnames(cleanData))
fullData <- bind_rows(cleanData, points)

# Clean up the data
fullData <- clean_data(fullData, c(15:19))
write_csv(fullData, "Data/Compiled/Vietnam_points.csv")
```
