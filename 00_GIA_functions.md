Functions for analysis of CEO Data using NPIP Estimators
================
MS Patterson, <tertiarymatt@gmail.com>
June 14, 2019

### Required packages

``` r
library(tidyverse)
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

    ## -- Attaching packages ----------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.1       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts -------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
```

Data Cleanup and Prep Functions
-------------------------------

Functions to clean up input data to make it easier to work with.

``` r
clean_data <- function(.data, dtcls) {
  # .data input is a raw CEO table of point data
  # dtcls input should be a vector of the columns with the CEO questions.
  
  require(stringr)
  # clean up column names
  colnames(.data) <- colnames(.data) %>% 
    gsub("\\?", "", .) %>% 
    gsub(" ", "_", .)
  
  # clean up values of data columns
  for (i in 1:length(dtcls)) {
    clmn <- colnames(.data[,dtcls[i]])
    .data[, dtcls[i]] <- pull(.data[, dtcls[i]]) %>% 
      str_replace(" ", "_") %>% 
      str_replace("-", "_") %>% 
      str_replace("\\/", "_") %>% 
      str_replace("Other$", paste("Other", clmn, sep = "_"))
  }
  
  # remove flagged plots
  if (length(which(.data$FLAGGED == TRUE)) > 0) {
    .data <- .data[-which(.data$FLAGGED == TRUE),]
  }
  
  return(.data)
}
```

Support Functions
-----------------

In order to use the esimators, the point data must be converted into a binary 'indicator' **y** for each cover class.

``` r
# Function for determing y_sub_c for a given commodity. 
presence <- function(pnts, lblfld, cmmdty){
  # pnts is the tbl of point data, with labels stored as characters
  # lblfld is the field in pnts which contains the point label.
  # cmmdty is the commodity in question, as a string. 
  require(dplyr)
  cmmdty <- enquo(cmmdty)
  pnts <- mutate(pnts, !!cmmdty := .data[[lblfld]] == !!cmmdty)
  return(pnts)
}

# Function for building "question tables" from raw input.
build_question <- function(pnts, mtdt, qstn){
  # pnts is the tbl of point data, with labels stored as characters
  # mtdt is a list of strings with the names of the metadata fields to include
  # qstn is the name of the question field
  require(dplyr)
  meta <-  select(pnts, !!mtdt)
  quest <- select(pnts, !!qstn)
  return(bind_cols(meta, quest))
}

# Function for building analysis table from question table and list of commodities
build_yc <- function(pnts, lblfld, cmmdtylst){
  # pnts is the question tbl point data, with label stored as characters
  # lblfld is the field in pnts which contains the point label.
  # cmmdtylst is a list of the commodities in question, as a string. 
  require(dplyr)
  for (i in seq_along(cmmdtylst)) {
    cmmdty <- cmmdtylst[i]
    cmmdty <- enquo(cmmdty)
    pnts <- mutate(pnts, !!cmmdty := .data[[lblfld]] == !!cmmdty)
  }
  return(pnts)
}


# Function for creating new values and error by multiplying two tables with 
# values and errors. Used for getting carbon value uncertainties. 

calcError <- function(table1, table2) {
  # tables should be structured as a column of values, and a column of errors
  new_values <- table1[,1] * table2[,1]
  new_errors <- new_values * sqrt((table1[,2]/table1[,1])^2 + (table2[,2]/table2[,1])^2)
  new_table <- cbind(new_values, new_errors)
  return(new_table)
}

# Function for making easier to work with tables from large o_in_c tables

makePretty <- function(ocTable){
  require(dplyr)
  require(stringr)

  # split the commodity_in_cover name into pieces to use
  contents <- str_split(rownames(ocTable), "-", 3, simplify = T)
  
  #build a tibble of the values
  .data <- tibble(condition = contents[,3], cover = contents[,1], 
                 area_ha = ocTable[,1], se_area = ocTable[,2])
  conditions <- unique(.data$condition)
  
  # make a table for display, loop spits out a list of tables subset from 
  # larger input table, by condition
  prettyTable <- list()
  for (c in seq_along(conditions)) {
    prettyTable[[c]] <- filter(.data, condition == conditions[c])
    prettyTable[[c]] <- prettyTable[[c]][,-1]
    names(prettyTable)[c] <- conditions[c]
  }
  
  return(prettyTable)
}
```

Functions for deriving photo-based estimates proportion
-------------------------------------------------------

All work based on the equations presented in \_\_Patterson, P. L. (2012). Photo-based estimators for the Nevada photo-based inventory (Research Paper No. RMRS-RP-92). Fort Collins, CO. <https://doi.org/10.2737/RMRS-RP-92__>

Equations numbers listed with functions indicate the equation in the above paper.

### Proportion of cover.

In the following equations:
**p** = proportion of cover
**P** = overall proportion of cover **c** = a particular cover type
**y\_sub\_c** = indicator variable (0,1) for presence of cover type *c*
**h** = stratum of interest
**i** = plot in stratum *h*
**j** = point in plot *i*
**m** = number of points *j* per plot *i*
**n** = number of plots per stratum *h*
**R** = area of a region or stratum *h*
**W** = proportional area of a stratum *h*

#### Equation 16

Proportion of coverage of a cover *c* in plot *i*, in stratum *h*

p\_sub\_ch\[i\] = sum(y\_sub\_chi\[j\])/m\_sub\_h\[i\]
(this is simply a fancy restatement of "determine the mean")

``` r
plot_means <- function(.data, grplst, cmmdtylst){
  require(dplyr)
  #do summary at the plot level, using grplst to group data appropriately
  output <-  .data %>%
    group_by(!!!syms(grplst)) %>% 
    summarise_at(cmmdtylst, mean)
  
  return(output)
}
```

#### Equation 16

Average proportion of coverage of a cover *c* in stratum *h*

p\_bar\_sub\_c\[h\] = sum(p\_sub\_ch\[i\])/n\[h\]
(this is also simply a fancy restatement of "determine the mean")

``` r
stratum_means <- function(.data, grplst, cmmdtylst){
  require(dplyr)
  # do summary at the stratum level, using grplst to group data appropriately
  # requires two levels of summarization with point data
  output <-  .data %>%
    group_by(!!!syms(grplst)) %>% 
    summarise_at(cmmdtylst, mean) %>% 
    summarise_at(vars(cmmdtylst), mean)
  return(output)
}

stratum_SE <- function(.data, grplst, cmmdtylst){
  require(dplyr)
  # generate the standard error for the strata 
  output <-  .data %>%
    group_by(!!!syms(grplst)) %>% 
    summarise_at(cmmdtylst, mean) %>% 
    summarise_at(vars(cmmdtylst), list(SE = ~sd(.)/sqrt(length(.))))
  return(output)
}
```

#### Equation 18

Overall proportion of cover for a given commodity
P\_hat\_sub\_c = sum(W\[h\] \* P\_bar\_sub\_c\[h\])
(this is simply an area weighted mean of stratum averages)

``` r
overall_prop <- function(stratumSummary, areas){
  require(dplyr)
  # takes stratum summarization to build table, throwing first two colums 
  # which contain country names and so on. 
  
  .data <- stratumSummary[,-c(1,2)]
  clmns <- ncol(.data)
  weights <- areas/(sum(areas))
  
  output <- vector(mode = "numeric", length = clmns)
  names(output) <- colnames(stratumSummary[,-c(1:2)])
  
  # builds the output, by calculating P_hat_sub_c = sum(W[h] * P_bar_sub_c[h])
  for (i in 1:clmns) {
    total <- sum(weights * .data[,i])
    names(total) <- colnames(.data[i])
    output[i] <- total
  }
  return(output)
}
```

### Variance of proportion of overall cover.

#### Equation 20

Overall Variance for proportion of cover of a given commodity.
V\_hat(P\_hat\_sub\_c)= sum(W\[h\]^2/(n[h](n%5Bh%5D-1)) \* (sum(p\_sub\_ch\[i\]^2) - (sum(p\_sub\_ch\[i\])^2/n\[h\]))

``` r
#quatities to calcualte
# W[h]^2/(n[h](n[h]-1)
# sum(p_sub_ch[i]^2) = sum of squared individual plot coverage proportion
# (sum(p_sub_ch[i]))^2/n[h] = square of sum of individual plot divided by n
```

``` r
overall_SE <- function(plotSummary, strata, areas, ns = NULL) {
  require(dplyr)
  
  # takes stratum summarization to build table, throwing first three columns 
  # which contain country names and so on. 
  cmmdty <- colnames(plotSummary[,-c(1:3)])
  strataVar <- matrix(nrow = length(strata), ncol = ncol(plotSummary) - 3)
  colnames(strataVar) <- cmmdty
  
  #caclulate area weights
  weights <- areas/(sum(areas))
  
  #calculate the individual variances using nested loop
  for (h in strata) {
    stratum <- subset(plotSummary, PL_STRATUM == h)[,-c(1:3)]
    
    # if there are no sample sizes input, figure out sample size from data
    if (is.null(ns[h])) {
      nh <- nrow(stratum)
      } else {nh <- ns[h]}
    
    # loop for actual variance calc for each type of cover/condition
    deviations <- c()
    for (i in seq_along(cmmdty)) {
      plotSqSum <- sum(stratum[,i]^2)
      plotSqMean <- (sum(stratum[,i])^2) / nh
      deviations[i] <-  plotSqSum - plotSqMean
      areaComp <- weights[h]^2 / (nh * (nh - 1))
      variances <- areaComp * deviations
    }
    strataVar[h,] <- variances
  }
  
  #convert variances to standard errors
  output <- sqrt(colSums(strataVar))
  
  return(output)
}
```

Analysis tools
--------------

### y\_c Analysis tools

A function to take the raw table, produce question tables for each question, and analyze them to produce the appropriate summaries.

``` r
do_yc_analysis <- function(table, mtdt, strata, areas, ns, qstns, grplst){
  require(dplyr)
  
  # table = a cleaned table of CEO point data.
  
  # mtdt = a vector of the names of the metadata fields to be included
  # strata = a vector of the strata names
  # areas = a vector of areas of the strata
  # ns = a vector of the strata sample sizes
  # qstns = a vector of the names of the CEO questions
  # grplst = a vector of the names of the grouping variables
  
  #Step 1 - Build questions table
  qTables <- list()
  for (q in 1:length(qstns)) {
    qTables[[q]] <- build_question(table, mtdt = mtdt, qstn = qstns[q])
  }
  
  #Step 2 - Build analysis tables for each question
  qcol <- ncol(qTables[[1]]) # column with question label
  aTables <- list()
  answers <- list()
  for (a in 1:length(qTables)) {
    answers[[a]] <- sort(unique(pull(qTables[[a]][,qcol]))) # response classes
    aTables[[a]] <- build_yc(qTables[[a]], lblfld = qstns[a], 
                                  cmmdtylst = answers[[a]])
  }
  
  #Step 3 - Build plot tables
  pTables <- list()
  for (p in 1:length(aTables)) {
   pTables[[p]] <-  plot_means(aTables[[p]], grplst = grplst, 
                              cmmdtylst = answers[[p]])
  }
  
  #Step 4 - Build stratum tables, first means then SE
  sTables <- list()
  for (s in 1:length(aTables)) {
    sTables[[s]] <- stratum_means(aTables[[s]], grplst = grplst, 
                                 cmmdtylst = answers[[s]])
  }
  
  seTables <- list()
  for (s in 1:length(aTables)) {
    seTables[[s]] <- stratum_SE(aTables[[s]], grplst = grplst, 
                                 cmmdtylst = answers[[s]])
  }
  
  #Step 5 - Analyze plot tables to produce question overall P_hat_c and V_hat_C
  p_hat_sub_c <-  list()
  v_hat_sub_c <- list()
  for (v in 1:length(pTables)) {
    p_hat_sub_c[[v]] <- overall_prop(sTables[[v]], areas = areas)
    v_hat_sub_c[[v]] <- overall_SE(pTables[[v]], strata, areas, ns)
  }
  
  #Step 6 - Gather results and output
  cover <- list()
  for (i in 1:length(p_hat_sub_c)) {
    cover[[i]] <- rbind(p_hat_sub_c[[i]], v_hat_sub_c[[i]])
    rownames(cover[[i]]) <-  c("PercentCover", "SE")
    names(cover)[i] <- qstns[i] #add names to list elements
  }
  
  output <- list("Cover" = cover, "StrataMeans" = sTables, 
                 "StrataSE" = seTables, "PlotSummaries" = pTables)
  
  return(output)
}
```

### Cover-in-Condition Analysis

Functions for calculating y\_o|c, an "object occuring in cover" conditional proportion and variance.

``` r
# Function for building "cover in condition tables" from raw input.
build_yoc <- function(table, mtdt, grplst, qstns, cvrfld, cndtnfld, conditions,
                       covers){
  # This function builds a table of binary indicator variables, of the form
  # cover_in_condition; for instance this could be a land cover in a land use. 
  
  # table = a cleaned table of CEO point data.
  # mtdt = a vector of the names of the metadata fields to be included
  # qstns = a vector of the names of the CEO questions
  # grplst = a vector of the names of the grouping variables
  # cvrfld = the name of field of the question concerned with the covers
  # cndtnfld = the name of field of the question concerned with the conditions
  # conditions = the list of conditions to use
  # covers = the list of covers to use
  
  require(dplyr)
  
  #Step 1 - Build question tables
  qTables <- list()
  for (q in 1:length(qstns)) {
    qTables[[q]] <- build_question(table, mtdt = mtdt, qstn = qstns[q])
  }
  names(qTables) <- qstns
  
  #Step 2 - Build analysis tables for each question
  ccTable <- left_join(qTables[[cvrfld]], qTables[[cndtnfld]])
  
  #add a test here later to make sure the conditions and covers all exist.
  
  for (o in seq_along(covers)) {
    cover <- covers[o]
    cover <- enquo(cover)
    for (c in seq_along(conditions)) {
      cond <- conditions[c]
      cond <- enquo(cond)
      answer <- paste(covers[o], "in", conditions[c], sep = "-")
      answer <- enquo(answer)
      ccTable <- mutate(ccTable, !!answer := .data[[cvrfld]] == !!cover &
                       .data[[cndtnfld]] == !!cond )
    }
  }
  return(ccTable)
}

# Function for calculating p_hat_y_oc
# This is equation 21 in Patterson (2012)

# Need to calculate p_hat_c and p_hat_oc to determine p_hat_o_in_c
cond_prop <- function(yoctable, yctable, areas, grplst, conditions){
  # this function finds the conditional probability of one thing occurring 
  # in another; the percent of use X occupied by cover Y. 
  
  # yoctable = a table of binary indicators for yoc, created using build_yoc 
  # yctable = a table of bindary indicators for yc, created using build_yc
  # The question or field 'c' in yoc and yc must match! 
  
  # conditions = a vector of conditions, 'c', held in common by yoc and yc
  
  # areas = areas of stratum 
  # grplst = vector of grouping variables
  
  
  require(dplyr)
  #calculate p_hat_c
  condSummary <- stratum_means(yctable, grplst, conditions)
  p_hat_c <- overall_prop(condSummary, areas = areas)
  
  #calculate p_hat_o|c
  y_ocs <- colnames(yoctable)[-c(1:9)]
  yocSummary <- stratum_means(yoctable, grplst, y_ocs)
  p_hat_oc <- overall_prop(yocSummary, areas = areas)
  
  #calculate p_hat_o_in_c
  #Only works if uses are in the same order for building both tables!
  p_hat_o_in_c <- p_hat_oc/p_hat_c

  return(p_hat_o_in_c)
}


# Function for calculating v_hat_y_oc
# This is equation 23 and 25 in Patterson (2012)

cond_SE <- function(yoctable, yctable, strata, grplst, conditions, covers,
                    areas, ns = NULL) {
  
  # this function finds the standard error of the conditional probability of 
  # one thing occurring in another; the percent of use X occupied by cover Y. 
  
  # yoctable = a table of binary indicators for yoc, created using build_yoc 
  # yctable = a table of bindary indicators for yc, created using build_yc
  # The question or field 'c' in yoc and yc must match! 
  
  # conditions = a vector of conditions, 'c', held in common by yoc and yc
  # covers = a vector of the covers 'o', occurring in 'c'. 
  
  # strata = vector of the names of the strata
  # grplst = vector of grouping variables
  # areas = a vector of areas of the strata 
  # ns = a vector of sample sizes for the strata
  
    require(dplyr)
  #generate plot level summaries for input tables
  p_cSum <- plot_means(yctable, grplst, conditions)
  
  y_ocs <- colnames(yoctable)[-c(1:9)]
  p_ocSum <- plot_means(yoctable, grplst, y_ocs)
  
  cmmdty <- colnames(p_ocSum[,-c(1:3)])
  strataVar <- matrix(nrow = length(strata), ncol = ncol(p_ocSum) - 3)
  colnames(strataVar) <- cmmdty
  
  weights <- areas/(sum(areas))
  cov_poc_pc <- matrix(nrow = length(strata), ncol = ncol(p_ocSum) - 3)
  
  for (h in strata) {
    stratum_c <- subset(p_cSum, PL_STRATUM == h)[,-c(1:3)]
    stratum_oc <- subset(p_ocSum, PL_STRATUM == h)[,-c(1:3)]
    
    if (is.null(ns[h])) {
      nh <- nrow(stratum_c)
    } else {nh <- ns[h]}
    
    #equation 25
    deviations <- c()
    for (c in seq_along(conditions)) {
      for (i in seq_along(covers)) {
        u <- i + (c - 1) * length(covers)
        #sum(p_o_in_c * p_c)
        plotSqSum <- sum(stratum_oc[, u] * stratum_c[,c])
        #(sum(p_o_in_ci) * sum(p_ci))/nh
        plotSqMean <- (sum(stratum_oc[, u]) * sum(stratum_c[,c])) / nh 
        
        deviations <-  plotSqSum - plotSqMean
        areaComp <- weights[h]^2 / (nh * (nh - 1))
        
        cov_poc_pc[h, u] <- areaComp * deviations
      }
    }
    #equation 23
    # fill in row for stratum
    #strataVar[h,] <- variances
  }
  eq25 <- colSums(cov_poc_pc)
  names(eq25) <- cmmdty
  
  #calculate p_hat_c
  condSummary <- stratum_means(yctable, grplst, conditions)
  p_hat_c <- overall_prop(condSummary, areas = areas)

  #calculate p_hat_o_in_c
  p_hat_o_in_c <- cond_prop(yoctable, yctable, areas, grplst, conditions)

  # calculate v_p_hat_c
  var_p_hat_c <- overall_SE(p_cSum, strata, areas, ns)^2

  # calculate v_p_hat_oc
  var_p_hat_oc <- overall_SE(p_ocSum, strata, areas, ns)^2
  
  strataVar <- (var_p_hat_oc + p_hat_o_in_c^2 * var_p_hat_c - 2 * p_hat_o_in_c * eq25) / p_hat_c^2
  
  # convert variances into standard errors
  output <- sqrt(strataVar)
  return(output)
}


#wrapper function for doing analysis
do_yoc_analysis <- function(.data, mtdt, qstns, grplst, strata, ns, areas, 
                             cover, condition) {
  # this function performs the analysis of the conditional probability of 
  # one thing occurring in another; the percent of use X occupied by cover Y. 
  
  # .data is a dataframe of the cleaned CEO data, which is used to construct
  # the yoc and yc tables.  
  
  # condition = the question/field containing the conditions, 'c', in yoc and yc
  # cover = the question/field containing the covers 'o', occurring in 'c'. 
  
  # strata = vector of the names of the strata
  # grplst = vector of grouping variables
  # areas = a vector of areas of the strata 
  # ns = a vector of sample sizes for the strata
  
  require(dplyr)
  #generate needs variables
  condTable <- build_question(.data, mtdt = metaNames, qstn = condition)
  coverTable <- build_question(.data, mtdt = metaNames, qstn = cover)
  
  conditions <- sort(unique(pull(condTable[,ncol(condTable)])))
  covers <- sort(unique(pull(coverTable[,ncol(coverTable)])))
  
  #build y_c table
  y_cTable <- build_yc(condTable, lblfld = condition, cmmdtylst = conditions)
  
  #build y_oc table
  y_ocTable <- build_yoc(table = .data, 
                          mtdt = metaNames, qstns = questions, grplst = groupList, 
                          cvrfld = cover, cndtnfld = condition, 
                          covers = covers, conditions = conditions)
  
  #analyze tables
  p_hat_o_in_c <- cond_prop(y_ocTable, y_cTable, areas = stratumAreas, grplst = groupList, 
                            conditions = conditions)
  
  se_p_hat_o_in_c <- cond_SE(yoctable = y_ocTable, yctable = y_cTable, strata = strata, 
                             grplst = groupList, conditions = conditions, covers = covers, 
                             areas = stratumAreas, ns = sampSize)
  
  cover <- list()
  for (i in 1:length(p_hat_o_in_c)) {
    print(p_hat_o_in_c[i])
    label <- str_split(names(p_hat_o_in_c[[i]]), "-", 3, simplify = T)
    
    cover[[i]] <- tibble(label[,1], label[,3], 
                         p_hat_o_in_c[[i]], se_p_hat_o_in_c[[i]])
    colnames(cover[[i]]) <-  c("cover", "condition", "PercentCover", "SE")
    rownames(cover[[i]]) <- c()
    names(cover)[i] <- label[1,1] #add names to list elements
  }
  
  #output <- cbind(p_hat_o_in_c, se_p_hat_o_in_c)
  
  return(output)
}
```

### Total Cover Analysis for multipart covers

``` r
# Function for building "cover in multiple condition tables" from raw input.

build_yocc <- function(table, mtdt, qstns, cvrfld, cndtnfld1, 
                        cndtnfld2, covers = NULL, conditions1 = NULL, 
                        conditions2 = NULL){
  # This function builds a table of binary indicator variables, of the form
  # cover_in_condition2_in_condition1; for instance this could be a land cover
  # in a land use, in a historical land use. Functionally, this table is 
  # identical to a y_c table, because it addresses the total percent occurrence
  # of something, rather than a conditional occurrence. 
  
  # table = a cleaned table of CEO point data.
  # mtdt = a vector of the names of the metadata fields to be included
  # qstns = a vector of the names of the CEO questions
  # grplst = a vector of the names of the grouping variables
  # cvrfld = the name of field of the question concerned with the covers
  # cndtnfld1 = the name of field of the question concerned with condition 1
  # cndtnfld2 = the name of field of the question concerned with condition 2
  
  # covers = the list of covers to use from cvrfld
  # conditions1 = the list of conditions to use from cndtnfld 1
  # conditions2 = the list of conditions to use from cndtnfld 2
  
  # the output is a list of tables. 
  
  require(dplyr)
  #Step 1 - Build question tables
  qTables <- list()
  for (q in 1:length(qstns)) {
    qTables[[q]] <- build_question(table, mtdt = mtdt, qstn = qstns[q])
  }
  names(qTables) <- qstns
  
  # check to see if list of covers & conditions has been input
  # if not, read the values in from the data and use them all. 
  if (is.null(covers)){ 
    covers <- sort(unique(pull(qTables[[cvrfld]][,ncol(qTables[[cvrfld]])])))}
  
  if(is.null(conditions1)) {
    conditions1 <- sort(unique(pull(qTables[[cndtnfld1]][,ncol(qTables[[cndtnfld1]])])))
  }
  
  if(is.null(conditions2)){
    conditions2 <- sort(unique(pull(qTables[[cndtnfld2]][,ncol(qTables[[cndtnfld2]])])))
  }
  
  #Step 2 - Join question tables for each question
  occTable <- qTables[[cvrfld]] %>% 
    left_join(qTables[[cndtnfld1]]) %>% 
    left_join(qTables[[cndtnfld2]])
  
  #Step 3 - Build answer tables for each "question"
  #add a test here later to make sure the conditions and covers all exist.
  output <- list()
  for (o in seq_along(covers)) {
    cover <- covers[o]
    cover <- enquo(cover)
    
    output[[o]] <- occTable

    for (c1 in seq_along(conditions1)){
      cond1 <- conditions1[c1]
      cond1 <- enquo(cond1)
    
      for (c2 in seq_along(conditions2)) {
        cond2 <- conditions2[c2]
        cond2 <- enquo(cond2)
        
        answer <- paste(covers[o], "in", conditions1[c1], "in", conditions2[c2], 
                        sep = "-")
        answer <- enquo(answer)
      
        output[[o]] <- mutate(output[[o]], !!answer := .data[[cvrfld]] == !!cover &
                             .data[[cndtnfld1]] == !!cond1 &
                             .data[[cndtnfld2]] == !!cond2)
      }
    }
  }
  return(output)
}


do_yocc_analysis <- function(yocc_tables, mtdt, strata, areas, ns, qstns, grplst){
  require(dplyr)

  # yocc_tables = a list of tables produced by build_yocc
  
  # mtdt = a vector of the names of the metadata fields to be included
  # strata = a vector of the strata names
  # areas = a vector of areas of the strata
  # ns = a vector of the strata sample sizes
  # qstns = a vector of the names of the CEO questions
  # grplst = a vector of the names of the grouping variables
  
  # This function works similarly to the do_yc_analysis function. 
  
  #Step 1 - Build plot tables
  offset <- length(mtdt) + length(qstns) + 1

  pTables <- list()
  for (p in 1:length(yocc_tables)) {
    yocc <- yocc_tables[[p]]
    answers <- colnames(yocc)[offset:ncol(yocc)]
    
    pTables[[p]] <-  plot_means(yocc, grplst = grplst, 
                                cmmdtylst = answers)
  }
  
  #Step 2 - Build stratum tables
  sTables <- list()
  for (s in 1:length(yocc_tables)) {
    yocc <- yocc_tables[[s]]
    answers <- colnames(yocc)[offset:ncol(yocc)]
    
    sTables[[s]] <- stratum_means(yocc, grplst = grplst, 
                                  cmmdtylst = answers)
  }
  
  seTables <- list()
  for (s in 1:length(yocc_tables)) {
    yocc <- yocc_tables[[s]]
    answers <- colnames(yocc)[offset:ncol(yocc)]
    
    seTables[[s]] <- stratum_SE(yocc, grplst = grplst, 
                                cmmdtylst = answers)
  }
  
  #Step 3 - Analyze plot tables to produce question overall P_hat_c and V_hat_C
  p_hat_sub_c <-  list()
  v_hat_sub_c <- list()
  for (v in 1:length(pTables)) {
    p_hat_sub_c[[v]] <- overall_prop(sTables[[v]], areas = areas)
    v_hat_sub_c[[v]] <- overall_SE(pTables[[v]], strata, areas, ns)
  }
  
  #Step 4 - Gather results and output
  cover <- list()
  for (i in 1:length(p_hat_sub_c)) {
    label <- str_split(names(p_hat_sub_c[[i]]), "-", 5, simplify = T)
    cover[[i]] <- tibble(label[,1], label[,3], label[,5], 
                         p_hat_sub_c[[i]], v_hat_sub_c[[i]])
    colnames(cover[[i]]) <-  c("cover", "condition1" ,"condition2",
                               "PercentCover", "SE")
    rownames(cover[[i]]) <- c()
    names(cover)[i] <- label[1,1] #add names to list elements
  }
  
  output <- list("Cover" = cover, "StrataMeans" = sTables, 
                 "StrataSE" = seTables, "PlotSummaries" = pTables)
  
  return(output)
}
```
