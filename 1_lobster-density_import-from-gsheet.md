BIOL4408 Marine Ecology: lobster density 1.
================
TimLanglois
16/02/2021

``` r
#### Pacakges we will need

library(googlesheets4) #to read gsheet
library(tidyr) #to tidy data
library(dplyr) #to transform data
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr) #to write data
library(here) #to make robust links to files
```

    ## here() starts at /Users/00068010/github/UWA BIOL4408/Analysis-lobster-density

``` r
# How to install packages

#install.packages("googlesheets4")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("here")
#install.packages("ggplot2")
#install.packages("plyr")
```

## 1. Import lobster density data from a google sheet

We have to install two packages that are not pre-installed in ecocloud

``` r
# install.packages("googlesheets4")

# install.packages("here")
```

Next we load some librarys

``` r
library(googlesheets4) #to read gsheet
library(tidyr) #to tidy data
library(dplyr) #to transform data
library(readr) #to write data
library(here) #to make robust links to files
```

It is useful to set a study name

``` r
study<-"lobster.density"
```

Read in the data from the google sheet and check it

``` r
#We need the URL for the google sheet
url <- "https://docs.google.com/spreadsheets/d/1Wqn7m2jopx11n5fdl9MHZAujRVkjBusdmIHq_gMhf0A"

# Then we can read it in
options(httr_oob_default=TRUE) # to allow access to googlesheets

dat<-read_sheet(url, sheet = "lobster.density")%>%
  as_tibble()%>%
  select(-c('longitude','latitude','time','way.point','gps','group'))%>%
  glimpse()
```

    ## Using an auto-discovered, cached token.
    ## To suppress this message, modify your code or options to clearly consent to the use of a cached token.
    ## See gargle's "Non-interactive auth" vignette for more details:
    ## https://gargle.r-lib.org/articles/non-interactive-auth.html

    ## The googlesheets4 package is using a cached token for tim.langlois@marineecology.io.

    ## Reading from "BIOL4408.lobster.density"

    ## Range "'lobster.density'"

    ## Rows: 3,604
    ## Columns: 40
    ## $ year              <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 201…
    ## $ date              <dttm> 2014-01-26 16:00:00, 2014-01-26 16:00:00, 2014-01-…
    ## $ sanctuary         <chr> "Armstrong Bay", "Armstrong Bay", "Armstrong Bay", …
    ## $ status            <chr> "No-take", "No-take", "No-take", "No-take", "No-tak…
    ## $ site              <chr> "Little Armstrong", "Little Armstrong", "Little Arm…
    ## $ replicate         <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
    ## $ sampling.location <chr> "none", "none", "none", "none", "none", "none", "no…
    ## $ depth             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ complexity        <dbl> 0, 2, 4, 1, 4, 2, 2, 2, 2, 1, 3, 1, 1, 2, 1, 3, 1, …
    ## $ algal.cover       <dbl> 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, …
    ## $ unsized           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ legal.unsized     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ sublegal.unsized  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x20               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x25               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x30               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x35               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, …
    ## $ x40               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x45               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, …
    ## $ x50               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x55               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 1, 0, …
    ## $ x60               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x65               <dbl> 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, …
    ## $ x70               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x75               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x80               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x85               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, …
    ## $ x90               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x95               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x100              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x105              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x110              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x115              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x120              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x125              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x130              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x135              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x140              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x145              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x150              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

Check we have all ther years of data!

``` r
summary(dat$year)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2014    2015    2017    2017    2019    2021

Now we need to save a copy of the data.

There are several ways to setting the directory where we will read data
from or write data to.

We are going to use the here() function, which creates a shortcut to
your location.

``` r
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later----
data.dir<-paste(working.dir,"Data",sep="/")
plot.dir<-paste(working.dir,"Plots",sep="/")
primer.dir<-paste(working.dir,"PRIMER",sep="/")
```

As long as the names on the folders are consistent this function will
enable us to work across computers and operating systems.

If you are using an ecocloud server - we will have to add folder names
to the here() function - but this should work

\#here(“workspace”,“Template-lobster-density”,“Data”)

Let’s create a “Data” directory then use here() to make a shortcut to
that “Data” directory.

Now to write the data we have imported from the googlesheet. We will
append the study name.

``` r
setwd(data.dir)#this is our shortcut using here()
dir()
```

    ## [1] "dat.new2.csv"               "lobster.density.csv"       
    ## [3] "lobster.density.gsheet.csv"

``` r
write_csv(dat,paste(study,"gsheet","csv",sep = "."))
```

Go back to
[Analysis-lobster-density](https://github.com/UWA-BIOL4408-Marine-Ecology/Analysis-lobster-density/blob/master/README.md)

Go forward to
[2\_lobster-density\_check-data](https://github.com/UWA-BIOL4408-Marine-Ecology/Analysis-lobster-density/blob/master/2_lobster-density_check-data.md)
