
<!-- README.md is generated from README.Rmd. Please edit that file -->

# transrivalry

<!-- badges: start -->
<!-- badges: end -->

This package is used to analyze the transition data collected in the
project of “Phenomenology of Transitions in Binocular Rivalry”. These
functions organizes the data for the further statistical analysis.
Moreover, one can visualize the data and the descriptive statistics.

## Installation

You can install the released version of transrivalry from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("transrivalry")
```

## Example

This is a basic example for preprocessing:

``` r
library(transrivalry)
datafold <- paste(getwd(), '/tests', sep='')
csv_path <- paste(datafold, '/questionnaire_categories.csv',sep='')
subject_list <- c('s001','s003','s005')
basename <- 'assessments_'
data <- combine_all_subjects(csv_path,datafold,subject_list,basename,1)
```
