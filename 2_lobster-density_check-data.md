BIOL4408 Marine Ecology: Lobster density 2.
================
TimLanglois
09/02/2021

## 2. Check the data and make corrections.

Load extra librarys

``` r
library(tidyr) #to tidy data
library(dplyr) #to transform data
library(readr) #to write data
library(here) #to make robust links to files

library(forcats) #to transform catagorical data
library(ggplot2) #to plot data
```

Set a study name

``` r
study<-"lobster.density"
```

``` r
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later----
data.dir<-paste(working.dir,"Data",sep="/")
plot.dir<-paste(working.dir,"Plots",sep="/")
primer.dir<-paste(working.dir,"PRIMER",sep="/")
```

Read in the data and glimpse the data. glimpse() shows the variable
names, formats, head of the data.

``` r
setwd(data.dir)#this is our shortcut using here()
dir()
```

    ## [1] "dat.new2.csv"               "lobster.density.csv"       
    ## [3] "lobster.density.gsheet.csv"

``` r
gsheet.dat<-read_csv("lobster.density.gsheet.csv")%>%
  dplyr::mutate(site.new=paste(sanctuary,status,site,sep="."))%>%
  glimpse()
```

    ## Rows: 3,604
    ## Columns: 41
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
    ## $ site.new          <chr> "Armstrong Bay.No-take.Little Armstrong", "Armstron…

This is wide format data.

Check west salmon

``` r
table(gsheet.dat$site.new,gsheet.dat$year)
```

    ##                                              
    ##                                               2014 2015 2016 2017 2018 2019
    ##   Armstrong Bay.Fished.City of York             30   30   30   40   28    0
    ##   Armstrong Bay.Fished.Geordie Bay              30   40   30   40   30    0
    ##   Armstrong Bay.Fished.Longreach                 0   10    0    0    0    0
    ##   Armstrong Bay.Fished.Parakeet Bay              0   30   30   40   40    0
    ##   Armstrong Bay.Fished.Ricey Beach              30   30   30   40   40    0
    ##   Armstrong Bay.Fished.Rocky Bay                 0   33    0    0    0    0
    ##   Armstrong Bay.Fished.Stark Bay                 0   20    0    0    0    0
    ##   Armstrong Bay.No-take.Armstrong Point         28    0    0    0    0    0
    ##   Armstrong Bay.No-take.Little Armstrong        58   30   30   40   31    0
    ##   Armstrong Bay.No-take.Parakeet Bay             0    0   30   40    0    0
    ##   Armstrong.Fished.City of York                  0    0    0    0    0    0
    ##   Armstrong.Fished.City of York Bay              0    0    0    0    0   30
    ##   Armstrong.Fished.Geordie Bay                   0    0    0    0    0    0
    ##   Armstrong.Fished.Geordie_Bay                   0    0    0    0    0   30
    ##   Armstrong.Fished.Parakeet Bay                  0    0    0    0    0    0
    ##   Armstrong.Fished.Parakeet_Bay                  0    0    0    0    0   30
    ##   Armstrong.Fished.Ricey Beach                   0    0    0    0    0    0
    ##   Armstrong.Fished.Ricey_Bay                     0    0    0    0    0   30
    ##   Armstrong.No-take.Little Armstrong Bay East    0    0    0    0    0    0
    ##   Armstrong.No-take.Little Armstrong Bay West    0    0    0    0    0    0
    ##   Armstrong.No-Take.Little_Armstrong_Bay         0    0    0    0    0   30
    ##   Green Is.Fished.Salmon Bay West                0    0    0    0    0    0
    ##   Green Is.Fished.Stickland East                 0    0    0    0    0    0
    ##   Green Is.No-take.Green Island                  0    0    0    0    0    0
    ##   Green Is.No-take.Mary Cove                     0    0    0    0    0    0
    ##   Green Island.Fished.Strickland Bay            25   40   30   40   40    0
    ##   Green Island.Fished.West Salmon               27   23   30   40   40    0
    ##   Green Island.No-take.Green Island             30   20   30   40   30    0
    ##   Green Island.No-take.Mary Cove                29   30   30   40   30    0
    ##   Green_Island.Fished.East_Strickland            0    0    0    0    0   30
    ##   Green_Island.Fished.West_Salmon_Bay            0    0    0    0    0   30
    ##   Green_Island.No-take.Green_Island              0    0    0    0    0   30
    ##   Green_Island.No-Take.Mary_Cove                 0    0    0    0    0   30
    ##   NA.In.Green Island                             0    0    0    0    0    0
    ##   NA.In.Green Island IN                          0    0    0    0    0    0
    ##   NA.In.Little Armstrong                         0    0    0    0    0    0
    ##   NA.In.Mary Cove                                0    0    0    0    0    0
    ##   NA.In.Parker Point                             0    0    0    0    0    0
    ##   NA.In.Salmon East                              0    0    0    0    0    0
    ##   NA.Out.City of York                            0    0    0    0    0    0
    ##   NA.Out.Fairbridge                              0    0    0    0    0    0
    ##   NA.Out.Geordie  Bay                            0    0    0    0    0    0
    ##   NA.Out.Parker Point                            0    0    0    0    0    0
    ##   NA.Out.Salmon West                             0    0    0    0    0    0
    ##   NA.Out.Strickland East                         0    0    0    0    0    0
    ##   NA.Out.West Salmon                             0    0    0    0    0    0
    ##   Parker Point.Fished.Fairbridge                 0   29   30   40   30    0
    ##   Parker Point.Fished.Parker Point              40   30   30   40   30    0
    ##   Parker Point.No-take.eastsalmon                0    0    0    0   30    0
    ##   Parker Point.No-take.Little Salmon             0   30   30   40   39    0
    ##   Parker Point.No-take.Parker Point             65   30   30   40   40    0
    ##   Parker Point.No-take.Salmon Bay               29   30   30   40    0    0
    ##   Parker Pt.Fished.Fairbridge                    0    0    0    0    0    0
    ##   Parker Pt.Fished.Parker point                  0    0    0    0    0    0
    ##   Parker Pt.No-take.East Salmon                  0    0    0    0    0    0
    ##   Parker Pt.No-take.Parker point                 0    0    0    0    0    0
    ##   Parker_Pt.Fished.Fairbridge                    0    0    0    0    0   30
    ##   Parker_Pt.Fished.Poc_Reef                      0    0    0    0    0   30
    ##   Parker_Pt.No-take.East_Salmon                  0    0    0    0    0   30
    ##   Parker_Pt.No-take.Little_Salmon                0    0    0    0    0   31
    ##                                              
    ##                                               2020 2021
    ##   Armstrong Bay.Fished.City of York              0    0
    ##   Armstrong Bay.Fished.Geordie Bay               0    0
    ##   Armstrong Bay.Fished.Longreach                 0    0
    ##   Armstrong Bay.Fished.Parakeet Bay              0    0
    ##   Armstrong Bay.Fished.Ricey Beach               0    0
    ##   Armstrong Bay.Fished.Rocky Bay                 0    0
    ##   Armstrong Bay.Fished.Stark Bay                 0    0
    ##   Armstrong Bay.No-take.Armstrong Point          0    0
    ##   Armstrong Bay.No-take.Little Armstrong         0    0
    ##   Armstrong Bay.No-take.Parakeet Bay             0    0
    ##   Armstrong.Fished.City of York                 30    0
    ##   Armstrong.Fished.City of York Bay              0    0
    ##   Armstrong.Fished.Geordie Bay                  30    0
    ##   Armstrong.Fished.Geordie_Bay                   0    0
    ##   Armstrong.Fished.Parakeet Bay                 30    0
    ##   Armstrong.Fished.Parakeet_Bay                  0    0
    ##   Armstrong.Fished.Ricey Beach                  30    0
    ##   Armstrong.Fished.Ricey_Bay                     0    0
    ##   Armstrong.No-take.Little Armstrong Bay East   30    0
    ##   Armstrong.No-take.Little Armstrong Bay West   30    0
    ##   Armstrong.No-Take.Little_Armstrong_Bay         0    0
    ##   Green Is.Fished.Salmon Bay West               30    0
    ##   Green Is.Fished.Stickland East                20    0
    ##   Green Is.No-take.Green Island                 30    0
    ##   Green Is.No-take.Mary Cove                    30    0
    ##   Green Island.Fished.Strickland Bay             0    0
    ##   Green Island.Fished.West Salmon                0    0
    ##   Green Island.No-take.Green Island              0    0
    ##   Green Island.No-take.Mary Cove                 0    0
    ##   Green_Island.Fished.East_Strickland            0    0
    ##   Green_Island.Fished.West_Salmon_Bay            0    0
    ##   Green_Island.No-take.Green_Island              0    0
    ##   Green_Island.No-Take.Mary_Cove                 0    0
    ##   NA.In.Green Island                             0   10
    ##   NA.In.Green Island IN                          0   11
    ##   NA.In.Little Armstrong                         0   61
    ##   NA.In.Mary Cove                                0   43
    ##   NA.In.Parker Point                             0   30
    ##   NA.In.Salmon East                              0   41
    ##   NA.Out.City of York                            0   25
    ##   NA.Out.Fairbridge                              0   35
    ##   NA.Out.Geordie  Bay                            0   33
    ##   NA.Out.Parker Point                            0   39
    ##   NA.Out.Salmon West                             0   10
    ##   NA.Out.Strickland East                         0   31
    ##   NA.Out.West Salmon                             0   10
    ##   Parker Point.Fished.Fairbridge                 0    0
    ##   Parker Point.Fished.Parker Point               0    0
    ##   Parker Point.No-take.eastsalmon                0    0
    ##   Parker Point.No-take.Little Salmon             0    0
    ##   Parker Point.No-take.Parker Point              0    0
    ##   Parker Point.No-take.Salmon Bay                0    0
    ##   Parker Pt.Fished.Fairbridge                   20    0
    ##   Parker Pt.Fished.Parker point                 30    0
    ##   Parker Pt.No-take.East Salmon                 30    0
    ##   Parker Pt.No-take.Parker point                30    0
    ##   Parker_Pt.Fished.Fairbridge                    0    0
    ##   Parker_Pt.Fished.Poc_Reef                      0    0
    ##   Parker_Pt.No-take.East_Salmon                  0    0
    ##   Parker_Pt.No-take.Little_Salmon                0    0

Check for unique levels of status.

``` r
unique(gsheet.dat$status)
```

    ## [1] "No-take" "Fished"  "No-Take" "Out"     "In"

there is a typo we need to correct in No-Take

Check for unique levels of sanctuary.

``` r
unique(gsheet.dat$sanctuary)
```

    ## [1] "Armstrong Bay" "Parker Point"  "Green Island"  "Parker_Pt"    
    ## [5] "Armstrong"     "Green_Island"  "Parker Pt"     "Green Is"     
    ## [9] NA

We need to make these consistent.

Check for unique levels of site.

``` r
unique(gsheet.dat$site)
```

    ##  [1] "Little Armstrong"          "City of York"             
    ##  [3] "Ricey Beach"               "Parker Point"             
    ##  [5] "Armstrong Point"           "Salmon Bay"               
    ##  [7] "Green Island"              "Mary Cove"                
    ##  [9] "Geordie Bay"               "West Salmon"              
    ## [11] "Strickland Bay"            "Parakeet Bay"             
    ## [13] "Stark Bay"                 "Rocky Bay"                
    ## [15] "Longreach"                 "Fairbridge"               
    ## [17] "Little Salmon"             "eastsalmon"               
    ## [19] "East_Salmon"               "Geordie_Bay"              
    ## [21] "East_Strickland"           "Green_Island"             
    ## [23] "Ricey_Bay"                 "Little_Salmon"            
    ## [25] "City of York Bay"          "Little_Armstrong_Bay"     
    ## [27] "Poc_Reef"                  "Mary_Cove"                
    ## [29] "West_Salmon_Bay"           "Parakeet_Bay"             
    ## [31] "East Salmon"               "Parker point"             
    ## [33] "Stickland East"            "Salmon Bay West"          
    ## [35] "Little Armstrong Bay East" "Little Armstrong Bay West"
    ## [37] "Geordie  Bay"              "Green Island IN"          
    ## [39] "Salmon West"               "Strickland East"          
    ## [41] "Salmon East"

“eastsalmon” should be “Salmon Bay”

Check for unique levels of year.

``` r
unique(gsheet.dat$year)
```

    ## [1] 2014 2015 2016 2017 2018 2019 2020 2021

We think the data from 2017 has errors in it. We should remove it.

Check measures of habitat complexity. As this is a semi-continous
variable we will use summary() to see the range.

``` r
unique(gsheet.dat$complexity)
```

    ## [1]  0  2  4  1  3 NA

``` r
summary(gsheet.dat$complexity)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   0.000   2.000   2.000   2.402   3.000   4.000       1

The maximum value is supposed to be 4. Looks ok.

Check measures of habitat cover.

``` r
unique(gsheet.dat$algal.cover)
```

    ## [1]  3  2  1  0  4  7 NA

``` r
summary(gsheet.dat$algal.cover)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   0.000   1.000   2.000   2.008   3.000   7.000     779

The maximum value is supposed to be 4. This does not look consistent.
Suggest you don’t use this.

Check measures of depth.

``` r
unique(gsheet.dat$depth)
```

    ##  [1]  0.00  2.00  1.50  2.50  3.00  1.00  1.70  1.80  0.50  3.50  2.30  3.20
    ## [13]  4.00  5.00  4.60  6.50  4.50  5.50  3.70  2.90  2.10  2.60  2.70  4.20
    ## [25]  2.20  0.70  4.30  1.20  2.80  0.75    NA  2.40  1.30  1.40  1.90  0.90
    ## [37]  3.60  3.80  3.90  4.90  4.40  5.30  1.55  0.30  0.60  1.60  0.80  1.10
    ## [49]  1.65  4.80  3.40  0.40  1.75  6.00  7.50  8.50  9.50 10.50 11.50

``` r
summary(gsheet.dat$depth)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   0.000   0.000   0.000   1.059   2.000  11.500     869

``` r
unique(filter(gsheet.dat,year=="2021")$depth)
```

    ##  [1]  2.50  1.50  2.00  1.00  3.00  1.75  3.50  4.00  5.00  4.50  6.00  1.80
    ## [13]  0.50  1.20  1.60  5.50  6.50  7.50  8.50  9.50 10.50 11.50

This looks reasonable.

Check what sites are missing between years.

``` r
table(gsheet.dat$site,gsheet.dat$year)
```

    ##                            
    ##                             2014 2015 2016 2017 2018 2019 2020 2021
    ##   Armstrong Point             28    0    0    0    0    0    0    0
    ##   City of York                30   30   30   40   28    0   30   25
    ##   City of York Bay             0    0    0    0    0   30    0    0
    ##   East Salmon                  0    0    0    0    0    0   30    0
    ##   East_Salmon                  0    0    0    0    0   30    0    0
    ##   East_Strickland              0    0    0    0    0   30    0    0
    ##   eastsalmon                   0    0    0    0   30    0    0    0
    ##   Fairbridge                   0   29   30   40   30   30   20   35
    ##   Geordie  Bay                 0    0    0    0    0    0    0   33
    ##   Geordie Bay                 30   40   30   40   30    0   30    0
    ##   Geordie_Bay                  0    0    0    0    0   30    0    0
    ##   Green Island                30   20   30   40   30    0   30   10
    ##   Green Island IN              0    0    0    0    0    0    0   11
    ##   Green_Island                 0    0    0    0    0   30    0    0
    ##   Little Armstrong            58   30   30   40   31    0    0   61
    ##   Little Armstrong Bay East    0    0    0    0    0    0   30    0
    ##   Little Armstrong Bay West    0    0    0    0    0    0   30    0
    ##   Little Salmon                0   30   30   40   39    0    0    0
    ##   Little_Armstrong_Bay         0    0    0    0    0   30    0    0
    ##   Little_Salmon                0    0    0    0    0   31    0    0
    ##   Longreach                    0   10    0    0    0    0    0    0
    ##   Mary Cove                   29   30   30   40   30    0   30   43
    ##   Mary_Cove                    0    0    0    0    0   30    0    0
    ##   Parakeet Bay                 0   30   60   80   40    0   30    0
    ##   Parakeet_Bay                 0    0    0    0    0   30    0    0
    ##   Parker point                 0    0    0    0    0    0   60    0
    ##   Parker Point               105   60   60   80   70    0    0   69
    ##   Poc_Reef                     0    0    0    0    0   30    0    0
    ##   Ricey Beach                 30   30   30   40   40    0   30    0
    ##   Ricey_Bay                    0    0    0    0    0   30    0    0
    ##   Rocky Bay                    0   33    0    0    0    0    0    0
    ##   Salmon Bay                  29   30   30   40    0    0    0    0
    ##   Salmon Bay West              0    0    0    0    0    0   30    0
    ##   Salmon East                  0    0    0    0    0    0    0   41
    ##   Salmon West                  0    0    0    0    0    0    0   10
    ##   Stark Bay                    0   20    0    0    0    0    0    0
    ##   Stickland East               0    0    0    0    0    0   20    0
    ##   Strickland Bay              25   40   30   40   40    0    0    0
    ##   Strickland East              0    0    0    0    0    0    0   31
    ##   West Salmon                 27   23   30   40   40    0    0   10
    ##   West_Salmon_Bay              0    0    0    0    0   30    0    0

“Armstrong Point”,“Longreach”,“Rocky Bay”,“Stark Bay” - are only sampled
once and we should remove them.

Check how we have used site names and that they are unique between
levels of status?

``` r
table(gsheet.dat$site,gsheet.dat$status)
```

    ##                            
    ##                             Fished  In No-take No-Take Out
    ##   Armstrong Point                0   0      28       0   0
    ##   City of York                 188   0       0       0  25
    ##   City of York Bay              30   0       0       0   0
    ##   East Salmon                    0   0      30       0   0
    ##   East_Salmon                    0   0      30       0   0
    ##   East_Strickland               30   0       0       0   0
    ##   eastsalmon                     0   0      30       0   0
    ##   Fairbridge                   179   0       0       0  35
    ##   Geordie  Bay                   0   0       0       0  33
    ##   Geordie Bay                  200   0       0       0   0
    ##   Geordie_Bay                   30   0       0       0   0
    ##   Green Island                   0  10     180       0   0
    ##   Green Island IN                0  11       0       0   0
    ##   Green_Island                   0   0      30       0   0
    ##   Little Armstrong               0  61     189       0   0
    ##   Little Armstrong Bay East      0   0      30       0   0
    ##   Little Armstrong Bay West      0   0      30       0   0
    ##   Little Salmon                  0   0     139       0   0
    ##   Little_Armstrong_Bay           0   0       0      30   0
    ##   Little_Salmon                  0   0      31       0   0
    ##   Longreach                     10   0       0       0   0
    ##   Mary Cove                      0  43     189       0   0
    ##   Mary_Cove                      0   0       0      30   0
    ##   Parakeet Bay                 170   0      70       0   0
    ##   Parakeet_Bay                  30   0       0       0   0
    ##   Parker point                  30   0      30       0   0
    ##   Parker Point                 170  30     205       0  39
    ##   Poc_Reef                      30   0       0       0   0
    ##   Ricey Beach                  200   0       0       0   0
    ##   Ricey_Bay                     30   0       0       0   0
    ##   Rocky Bay                     33   0       0       0   0
    ##   Salmon Bay                     0   0     129       0   0
    ##   Salmon Bay West               30   0       0       0   0
    ##   Salmon East                    0  41       0       0   0
    ##   Salmon West                    0   0       0       0  10
    ##   Stark Bay                     20   0       0       0   0
    ##   Stickland East                20   0       0       0   0
    ##   Strickland Bay               175   0       0       0   0
    ##   Strickland East                0   0       0       0  31
    ##   West Salmon                  160   0       0       0  10
    ##   West_Salmon_Bay               30   0       0       0   0

## Corrections to the data

We have some corrections to make. It is always good to have our source
data as ‘correct’ as possible. However, in this case we will make these
corrections in our R import script so that we can keep a record of the
changes/corrections we are making to the data.

We can use this list of corrections to go back and check and correct the
raw data if we want.

East salmon

Armstrong - combine sties

Make corrections and re-format.

``` r
correct.dat<-gsheet.dat %>%
  dplyr::mutate(status = fct_recode(status,
                                    "No-take" = "No-Take",
                                    "No-take" = "In",
                                    "Fished" = "Out"))%>%

  
#recode the site names
  
  
    dplyr::mutate(site = fct_recode(site,
                          "Little Salmon" = "East Salmon Bay",
                           "Little Salmon" = "eastsalmon",
                           "City of York" = "City of York Bay",
                           "Ricey Beach" = "Ricey_Bay",
                           "Little Salmon" = "East_Salmon",
                           # "Salmon Bay" = "East Salmon",  #Maybe ? add in?
                           "Little Salmon" = "Little_Salmon",
                           "Parker Point" = "Poc_Reef",
                           "Green Island" = "Green_Island",
                           "Green Island" = "Green Island IN",
                           "Parakeet Bay" = "Parakeet_Bay",
                           "West Salmon" = "West_Salmon_Bay",
                           "West Salmon" = "Salmon Bay West",
                          "Little Salmon" = "Salmon East",
                          "West Salmon" = "Salmon West",
                            "Geordie Bay" = "Geordie  Bay",
                          "Strickland Bay"  =   "Strickland East",
                           "Little Armstrong" = "Little_Armstrong_Bay",
                           "Little Armstrong" = "Little Armstrong Bay East",
                            "Little Armstrong" = "Little Armstrong Bay West",
                           "Geordie Bay" = "Geordie_Bay",
                           "Mary Cove" = "Mary_Cove",
                           "Strickland Bay" = "East_Strickland",
                          
                            "Little Salmon" = "East Salmon",
                          "Parker Point" = "Parker point",
                          "West Salmon" = "Salmon Bay West",
                          "Little Salmon" = "Salmon Bay",
                           "Strickland Bay" = "Stickland East"

                          ))%>%
    
  
  # # # # addin missing sanctuary names
  # # # 
  mutate(sanctuary = if_else(site == "Little Armstrong",   "Armstrong Bay", sanctuary)) %>%
  mutate(sanctuary = if_else(site == "Parker Point",   "Parker Point", sanctuary)) %>%
  mutate(sanctuary = if_else(site == "Little Salmon",   "Parker Point", sanctuary)) %>%
  mutate(sanctuary = if_else(site == "West Salmon",   "Green Island", sanctuary)) %>%
  mutate(sanctuary = if_else(site == "Fairbridge",   "Parker Point", sanctuary)) %>%
  mutate(sanctuary = if_else(site == "Geordie Bay",   "Armstrong Bay", sanctuary)) %>%
  mutate(sanctuary = if_else(site == "Strickland Bay",   "Green Island", sanctuary)) %>%
   mutate(sanctuary = if_else(site == "Green Island",   "Green Island", sanctuary)) %>%

mutate(sanctuary = if_else(site == "City of York",   "Armstrong Bay", sanctuary)) %>%
  mutate(sanctuary = if_else(site == "Mary Cove",   "Green Island", sanctuary)) %>%
  
  
  dplyr::mutate(sanctuary = fct_recode(sanctuary,
                                    "Parker Point" = "Parker_Pt",
                                    "Green Island" = "Green_Island",
                                    "Armstrong Bay" = "Armstrong",
                                    "Green Island" = "Green Is",
                                    "Parker Point" = "Parker Pt"))%>%
  
  
  
  
  #filter out sites only done once
    filter(!site%in%c(
    "Armstrong Point",
    "Longreach",
    "Rocky Bay",
    "Stark Bay",
    "Parakeet Bay"))%>%
  
  #filter out suspicous year  
    filter(!year==2017)%>%
  
  #remove the levels for the filtered facotrs
    droplevels()%>%
  
  # Make a new unique Site name - to account for Parker Point being used for inside and outside the NTZ.
  dplyr::mutate(site.new=paste(sanctuary,status,site,sep="."))%>%
    mutate_at(vars("x20":"x150"),  replace_na, '0')%>%
  glimpse()
```

    ## Rows: 2,723
    ## Columns: 41
    ## $ year              <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 201…
    ## $ date              <dttm> 2014-01-26 16:00:00, 2014-01-26 16:00:00, 2014-01-…
    ## $ sanctuary         <fct> Armstrong Bay, Armstrong Bay, Armstrong Bay, Armstr…
    ## $ status            <fct> No-take, No-take, No-take, No-take, No-take, No-tak…
    ## $ site              <fct> Little Armstrong, Little Armstrong, Little Armstron…
    ## $ replicate         <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
    ## $ sampling.location <chr> "none", "none", "none", "none", "none", "none", "no…
    ## $ depth             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ complexity        <dbl> 0, 2, 4, 1, 4, 2, 2, 2, 2, 1, 3, 1, 1, 2, 1, 3, 1, …
    ## $ algal.cover       <dbl> 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, …
    ## $ unsized           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ legal.unsized     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ sublegal.unsized  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x20               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x25               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x30               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x35               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x40               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x45               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x50               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x55               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x60               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x65               <chr> "0", "0", "0", "0", "0", "2", "0", "0", "0", "0", "…
    ## $ x70               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x75               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x80               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x85               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x90               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x95               <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x100              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x105              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x110              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x115              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x120              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x125              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x130              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x135              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x140              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x145              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ x150              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "…
    ## $ site.new          <chr> "Armstrong Bay.No-take.Little Armstrong", "Armstron…

Now we can check our corrections

``` r
table(correct.dat$site.new,correct.dat$year)
```

    ##                                         
    ##                                          2014 2015 2016 2018 2019 2020 2021
    ##   Armstrong Bay.Fished.City of York        30   30   30   28   30   30   25
    ##   Armstrong Bay.Fished.Geordie Bay         30   40   30   30   30   30   33
    ##   Armstrong Bay.Fished.Ricey Beach         30   30   30   40   30   30    0
    ##   Armstrong Bay.No-take.Little Armstrong   58   30   30   31   30   60   61
    ##   Green Island.Fished.Strickland Bay       25   40   30   40   30   20   31
    ##   Green Island.Fished.West Salmon          27   23   30   40   30   30   20
    ##   Green Island.No-take.Green Island        30   20   30   30   30   30   21
    ##   Green Island.No-take.Mary Cove           29   30   30   30   30   30   43
    ##   Parker Point.Fished.Fairbridge            0   29   30   30   30   20   35
    ##   Parker Point.Fished.Parker Point         40   30   30   30   30   30   39
    ##   Parker Point.No-take.Little Salmon       29   60   60   69   61   30   41
    ##   Parker Point.No-take.Parker Point        65   30   30   40    0   30   30

Make corrections for 2021 data.

Let’s check this with Jane. See Green Island.No-take.West Salmon - this
has always been fished?

Now make new varialbes for sum of legal and sub.legal.

``` r
dat<-correct.dat%>%
  select(-c("sampling.location","date"))%>%
  replace(is.na(.), 0)%>%
  
  mutate_at(vars("x20":"x150"), funs(as.numeric(as.character(.))))%>%

  
  mutate_at(vars(starts_with("x")), funs(ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("unsi")), funs(ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("legal")), funs(ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("sub")), funs(ifelse(is.na(.),0,.)))%>%
  
  # make the legal sum
  dplyr::mutate(legal=(legal.unsized+x75+x80+x85+x90+x95+x100+x105+x110+x115+x120+x125+x130+x135+x140+x145+x150))%>%
  
  # make the sub.legal sum
  dplyr::mutate(sub.legal=(sublegal.unsized+x20+x25+x30+x35+x40+x45+x50+x55+x60+x65+x70))%>%
  
  # make an all sum
  dplyr::mutate(all=(legal+sub.legal+unsized))%>%
  
  # make a unique sample number
  dplyr::mutate(sample.no=1:nrow(.))%>%
  
  # select the variables of interest
  select(c(sample.no,year,sanctuary,status,site.new,complexity,depth,legal,sub.legal,all))%>%
  
  glimpse()
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))

    ## Rows: 2,723
    ## Columns: 10
    ## $ sample.no  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
    ## $ year       <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014…
    ## $ sanctuary  <fct> Armstrong Bay, Armstrong Bay, Armstrong Bay, Armstrong Bay…
    ## $ status     <fct> No-take, No-take, No-take, No-take, No-take, No-take, No-t…
    ## $ site.new   <chr> "Armstrong Bay.No-take.Little Armstrong", "Armstrong Bay.N…
    ## $ complexity <dbl> 0, 2, 4, 1, 4, 2, 2, 2, 2, 1, 3, 1, 1, 2, 1, 3, 1, 2, 2, 0…
    ## $ depth      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ legal      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0…
    ## $ sub.legal  <dbl> 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 17, 0, 2, 1, 0, 1, 0, 1, 1, …
    ## $ all        <dbl> 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 17, 0, 3, 1, 0, 1, 0, 1, 1, …

Make the data long format.

``` r
dat<-dat%>%
  gather(key="size.class",value="count",legal, sub.legal,all)%>%
  glimpse()
```

    ## Rows: 8,169
    ## Columns: 9
    ## $ sample.no  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
    ## $ year       <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014…
    ## $ sanctuary  <fct> Armstrong Bay, Armstrong Bay, Armstrong Bay, Armstrong Bay…
    ## $ status     <fct> No-take, No-take, No-take, No-take, No-take, No-take, No-t…
    ## $ site.new   <chr> "Armstrong Bay.No-take.Little Armstrong", "Armstrong Bay.N…
    ## $ complexity <dbl> 0, 2, 4, 1, 4, 2, 2, 2, 2, 1, 3, 1, 1, 2, 1, 3, 1, 2, 2, 0…
    ## $ depth      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ size.class <chr> "legal", "legal", "legal", "legal", "legal", "legal", "leg…
    ## $ count      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0…

Write the data

``` r
setwd(data.dir) #set the directory
dir() #look in the directory
```

    ## [1] "dat.new2.csv"               "lobster.density.csv"       
    ## [3] "lobster.density.gsheet.csv"

``` r
# Write dat using study name
write_csv(dat,paste(study,"csv",sep = "."))
```

Go back to
[Analysis-lobster-density](https://github.com/UWA-BIOL4408-Marine-Ecology/Analysis-lobster-density/blob/master/README.md)

Go forward to
[3\_lobster-density\_basic-plots-to-check-data](https://github.com/UWA-BIOL4408-Marine-Ecology/Analysis-lobster-density/blob/master/3_lobster-density_basic-plots-to-check-data.md)
