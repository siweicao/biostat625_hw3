
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/siweicao/biostat625_hw3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/siweicao/biostat625_hw3/actions/workflows/R-CMD-check.yaml)
  [![Codecov test coverage](https://codecov.io/gh/siweicao/biostat625_hw3/branch/main/graph/badge.svg)](https://app.codecov.io/gh/siweicao/biostat625_hw3?branch=main)
  <!-- badges: end -->


# linreg

## Overview
linreg is aimed to fit simple and multiple linear regression models. 

## Installation
To install the linreg package, you can use:

```r
#install.packages("devtools")
devtools::install_github("siweicao/biostat625_hw3")

```

## Usage (Example using airquality dataset)
The `airquality` dataset contains daily air quality measurements in New York, May to September 1973. The dataset consists of 6 variables and a total of 153 observations. We can use linreg to determine whether there are linear associations between different variables. 

```{r}
library(linreg)
data(airquality)
linreg_airquality=linreg(Temp~Wind+Month+Ozone, data = airquality)
linreg_airquality

airquality_summary=linreg_sum(Temp~Wind+Month+Ozone, data = airquality)
airquality_summary

```





