---
title: "vignette"
author: "Boxi Zhang, Priya Pullolickal"
date: "2017/9/22"
output: rmarkdown::html_document
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---



### 1. Load library and dataset

```r
library(boxlinre)
library(ggplot2)

data("faithful")
```


### 2. Create formula and fitting the model

```r
library(boxlinre)
linreg(eruptions~waiting, faithful)
```

```
## Call:
## linreg(formula =  eruptions ~ waiting ,data =  faithful )
## 
## Coefficients:
## (Intercept)     waiting 
## -1.87401599  0.07562795
```
