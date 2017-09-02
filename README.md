---
title: "package dplyrAssist"
author: "Keon-Woong Moon"
date: "2017-09-03"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



```
The 'dplyrAssist' is an RStudio addin for teaching and learning data manipulation using the 'dplyr' package. You can learn each steps of data manipulation by clicking your mouse without coding. You can get resultant data(as a 'tibble') and the code for data manipulation. 
```

## Install package

You can install `dplyrAssist` package from github.


```r
#install.packages("devtools")
devtools::install_github("cardiomoon/dplyrAssist")
```

## The First Example: Reshaping Data


```r
require(tidyverse)
require(dplyrAssist)
```

You can run dplyrAssist() function without data. 


```r
dplyrAssist()
```

Or you can run as an RStudio addin.

<img src="https://github.com/cardiomoon/dplyrAssist/blob/master/man/figures/0.png?raw=true" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="70%" style="display: block; margin: auto;" />

Bt default, tidyr::table1 data is displayed. Press `Show data structure` radio button(1) and you can see the data(2). 

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/6.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="70%" style="display: block; margin: auto;" />

You can reshape the data easily. Select "Reshaping Data"(1) and select "gather" function(2). You can see the plot explaining "gather"(3). Select `case` and `population`(4). Enter the key column name(5) and value column name(6). You can see the R code(7). Press `Add R code` button(8).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/7.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="70%" style="display: block; margin: auto;" />

You can see the Data Wrangling R codes(1) and the result(2). 

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/8.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" width="70%" style="display: block; margin: auto;" />

The reverse prcoess of `gather` is `spread`. Now select `spread` function(2). You can see the plot explaining "spread"(3). Select `key` and `value` columns(4). You can see the R code(5). Press `Add R code` button(6).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/9.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" width="70%" style="display: block; margin: auto;" />

You can see the Data Wrangling R codes(1) and the result(2). 

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/10.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" width="70%" style="display: block; margin: auto;" />


## The 2nd example: Summarise Data 

You can run dplyrAssist function with data name.


```r
result <- dplyrAssist(iris)
```

A shiny app appeared. Select `Group Data`(1) and select `group_by` function(2) and click the `Species` column(3).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/1.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" width="70%" style="display: block; margin: auto;" />

You can see the R code(1). You can edit this code. Press `Add R code`(2) button. 

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/2.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" width="70%" style="display: block; margin: auto;" />

You can the Data Wrangling R Code(1) and the result of R code(2).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/3.png" title="plot of chunk unnamed-chunk-13" alt="plot of chunk unnamed-chunk-13" width="70%" style="display: block; margin: auto;" />

You can add R code(s) as much as you want. Select `Summarise Data`(1) and `summarise_all` function(2). Insert `mean` to complete the R code(3). Press `Add R code` button(4).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/4.png" title="plot of chunk unnamed-chunk-14" alt="plot of chunk unnamed-chunk-14" width="70%" style="display: block; margin: auto;" />

You can see the Data Wrangling R codes(1) and the result(2). If you want to save the resultant data, Press `Save & exit` button(3).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/5.png" title="plot of chunk unnamed-chunk-15" alt="plot of chunk unnamed-chunk-15" width="70%" style="display: block; margin: auto;" />

In R console, you can see the result with the following code.



```r
result
```

```
# A tibble: 3 x 5
     Species Sepal.Length Sepal.Width Petal.Length Petal.Width
      <fctr>        <dbl>       <dbl>        <dbl>       <dbl>
1     setosa        5.006       3.428        1.462       0.246
2 versicolor        5.936       2.770        4.260       1.326
3  virginica        6.588       2.974        5.552       2.026
```

```r
cat(attr(result,"code"))
```

```
iris %>%
 group_by(Species) %>%
 summarise_all(mean)
```

This is identical with the following codes.

```r
result<-iris %>% 
     group_by(Species) %>%
     summarise_all(mean)
attr(result,"code") <- "iris %>%\n group_by(Species) %>%\n summarise_all(mean)"
```



## The 3rd example : Combine the data sets

You can join datas easily with dplyr language. 


```r
result<-dplyrAssist(band_members,band_instruments)
```
When you turn on the `Show the 2nd Data` switch(1), you can see the name of second data(2). You can edit the name of data.  Press `Show data structure` radio button(3) and you can see the 2nd data(4). 

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/11.png" title="plot of chunk unnamed-chunk-20" alt="plot of chunk unnamed-chunk-20" width="70%" style="display: block; margin: auto;" />

Select `Combine Data Sets`(1) and select `left_join` function(2). You can see the R code(3) (Of course, you can edit it!) and you can add the R code by presssing the `Add R code button`(4).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/12.png" title="plot of chunk unnamed-chunk-21" alt="plot of chunk unnamed-chunk-21" width="70%" style="display: block; margin: auto;" />

You can see the R code(1) for data wrangling and the result(2).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/13.png" title="plot of chunk unnamed-chunk-22" alt="plot of chunk unnamed-chunk-22" width="70%" style="display: block; margin: auto;" />

## The 4th Example

In this example, you need the `flights` and `airpors` data from the package `nycflights13`.

```r
require(nycflights13)

result <- dplyrAssist()
```

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/14.png" title="plot of chunk unnamed-chunk-24" alt="plot of chunk unnamed-chunk-24" width="70%" style="display: block; margin: auto;" />

Enter Data name as `flights`(1). You can see the data by pressing the `data structure` button(2). After turn on the switch(3), enter the second data nae as `airports`(4). You can see the 2nd data also by pressing the `data structure` button(5).

Because the origin and destination airport name is recorded in `origin` and  `dest` column in flights data. If you want to join the flights and airports data by `dest` column in `flights` data and `faa` column in `airports` data, select `Combine data sets`(1) and `left_join` function(2). Select `dest` column(3) and select `faa` in the y.b selectbox(4). The R code for join is ready for you(5). You can add the R code by pressing the Add Rcode button(6).

<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/15.png" title="plot of chunk unnamed-chunk-25" alt="plot of chunk unnamed-chunk-25" width="70%" style="display: block; margin: auto;" />

You can see the R code for data combining(1) and the result(2). 
<img src="https://raw.githubusercontent.com/cardiomoon/dplyrAssist/master/man/figures/16.png" title="plot of chunk unnamed-chunk-26" alt="plot of chunk unnamed-chunk-26" width="70%" style="display: block; margin: auto;" />


