---
title: "package dplyrAssist"
author: "Keon-Woong Moon"
date: "2017-09-05"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{dplyrAssist}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



```
The 'dplyrAssist' is an RStudio addin for teaching and learning data manipulation using the 'dplyr' package. 
You can learn each steps of data manipulation by clicking your mouse without coding. 
You can get resultant data(as a 'tibble') and the code for data manipulation. 
```

## Install package

You can install `dplyrAssist` package from github.


```r
#install.packages("devtools")
devtools::install_github("cardiomoon/dplyrAssist")
```


## Usage: As an RStudio Add-in

This addin can be used to interactively manipulate a `data.frame` or a `tibble` using `dplyr`.
The intended way to use this is as follows:

1. Highlight a symbol naming a `data.frame` or a `tibble` in your R session, e.g. `iris`(1).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="70%" style="display: block; margin: auto;" />

2. Execute this addin(arrow), to interactively manipulate it.

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/2.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="70%" style="display: block; margin: auto;" />

3. When you're done(small arrow), the code for data manipulation will be emitted at the cursor position(green rectangle).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/3.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="70%" style="display: block; margin: auto;" />

## Usage: As a regular function

You can use the `dplyrAssist()` function as a regular function, e.g. in a command line.

You can find full vignette here. http://rpubs.com/cardiomoon/303716

