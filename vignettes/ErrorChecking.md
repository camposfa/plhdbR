[Back to Respository](https://github.com/camposfa/plhdbR)

Functions for working with the PLHDB data tables
================================================

Prepare workspace
-----------------

``` r
  library(plhdbR)
  load_plhdb_packages()
```

Reading data
------------

The functions `read_bio_table` and `read_fert_table` read csv files of biography and fertility data, respectively, created by the download buttons for these tables on the [PLHDB website](https://plhdb.org/). These functions strip away blank lines and header lines, parse any date/time columns, and return a well-ordered `dplyr::tbl_df`, an extension of R's `data.frame`. To pull all the data from a given table, use search criteria like `'Study.ID != 10'`. Note that the data are **not** extensively error-checked at this stage. If you try to feed these functions a normal csv file, bad things might happen.

### Biography data

``` r
  # Assuming your file is called "biography_2015_03_17.csv"
  lh <- read_bio_table("biography_2015_03_17.csv")
  summary(lh)
#>      Study.Id      Animal.Id     Animal.Name  
#>  rppn-fma: 564   KOM    :   4   Ahab   :   2  
#>  amboseli:1324   RUS    :   4   APOLLO :   2  
#>  kakamega: 599   AFR    :   3   APPLE  :   2  
#>  gombe   : 305   BUS    :   3   AQUA   :   2  
#>  karisoke: 321   FLO    :   3   Bali   :   2  
#>  beza    : 993   GOD    :   3   (Other):3402  
#>  ssr     : 299   (Other):4385   NA's   : 993  
#>    Birth.Date                  Min.Birth.Date               
#>  Min.   :1919-07-02 00:00:00   Min.   :1914-07-03 00:00:00  
#>  1st Qu.:1988-07-15 00:00:00   1st Qu.:1988-01-02 00:00:00  
#>  Median :1999-10-13 00:00:00   Median :1999-07-30 00:00:00  
#>  Mean   :1996-08-26 14:01:26   Mean   :1996-02-19 17:40:28  
#>  3rd Qu.:2007-07-15 00:00:00   3rd Qu.:2007-07-09 00:00:00  
#>  Max.   :2014-09-17 00:00:00   Max.   :2014-09-18 00:00:00  
#>                                                             
#>  Max.Birth.Date                Birth.Date.Distribution  Birth.Group  
#>  Min.   :1924-06-30 00:00:00   N: 743                  Matão  : 206  
#>  1st Qu.:1989-08-05 00:00:00   U:3662                  KK     : 204  
#>  Median :2000-01-02 00:00:00                           1.1    : 193  
#>  Mean   :1997-05-21 14:30:32                           2.2    : 174  
#>  3rd Qu.:2007-07-17 00:00:00                           1      : 170  
#>  Max.   :2997-11-26 00:00:00                           (Other):2912  
#>                                                        NA's   : 546  
#>  Birth.Group.Certainty First.Born     Mom.Id     Sex     
#>  C   :4031             N:2713     81     :  16   F:1868  
#>  U   : 185             U:1086     137    :  15   M:1988  
#>  NA's: 189             Y: 606     9160   :  14   U: 549  
#>                                   86     :  13           
#>                                   BS     :  13           
#>                                   (Other):3426           
#>                                   NA's   : 908           
#>    Entry.Date                  Entry.Type  Depart.Date                 
#>  Min.   :1963-01-15 00:00:00   B:3066     Min.   :1963-04-29 00:00:00  
#>  1st Qu.:1991-12-28 00:00:00   C: 456     1st Qu.:1998-07-30 00:00:00  
#>  Median :2001-10-02 00:00:00   I: 315     Median :2009-01-22 00:00:00  
#>  Mean   :1998-11-23 22:55:16   O: 568     Mean   :2004-12-19 09:05:55  
#>  3rd Qu.:2007-11-26 00:00:00              3rd Qu.:2013-06-28 00:00:00  
#>  Max.   :2014-09-29 00:00:00              Max.   :2014-09-30 00:00:00  
#>                                                                        
#>  Depart.Type Depart.Date.Error
#>  D:2137      Min.   :0.00000  
#>  E:  98      1st Qu.:0.00000  
#>  O:1549      Median :0.00000  
#>  P: 621      Mean   :0.06836  
#>              3rd Qu.:0.01000  
#>              Max.   :3.00000  
#> 
```

### Fertility data

``` r
  # Assuming your file is called "fertility_2015_03_17.csv"
  fert <- read_fert_table("fertility_2015_03_17.csv")
  summary(fert)
#>      Study.Id     Animal.Id      Start.Date                  Start.Type
#>  rppn-fma:151   BLAN   :   8   Min.   :1963-05-15 00:00:00   B:1078    
#>  amboseli:618   DOS-   :   8   1st Qu.:1990-05-19 00:00:00   C: 183    
#>  kakamega:229   KATH   :   8   Median :1998-09-29 00:00:00   I: 109    
#>  gombe   :162   SERI   :   8   Mean   :1997-05-16 16:59:06   O: 457    
#>  karisoke:202   LIMP   :   7   3rd Qu.:2006-07-13 00:00:00             
#>  beza    :252   NYLA   :   7   Max.   :2014-08-27 00:00:00             
#>  ssr     :213   (Other):1781                                           
#>    Stop.Date                   Stop.Type
#>  Min.   :1965-02-14 00:00:00   D:838    
#>  1st Qu.:1997-05-09 12:00:00   E: 78    
#>  Median :2008-03-08 00:00:00   O:832    
#>  Mean   :2004-02-11 19:17:02   P: 79    
#>  3rd Qu.:2013-06-27 00:00:00            
#>  Max.   :2014-09-30 00:00:00            
#> 
```

Error-checking data
-------------------

The functions `find_bio_errors` and `find_fert_errors` scan the loaded biography and fertility data, respectively, looking for errors of various kinds. Each function takes as an argument the relevant `dplyr::tbl_df` generated by the `read_..._table` functions listed above. `find_bio_errors` will find dates that are unrealistic as well as duplicate entries for the same (Study.Id, Animal.Id) combination. These are returned as named list elements `$error_dates` and `$error_duplicates`. `find_fert_errors` only scans the date/time fields for errors, since there are multiple fertility entries for some individuals. This is returned in a list with named element `$error_dates`.

``` r
  # Check the biography data for errors
  bio_errors <- find_bio_errors(lh)
  bio_errors$error_dates
#> Source: local data frame [1 x 8]
#> 
#>   Study.Id Animal.Id Mom.Id Birth.Date Min.Birth.Date Max.Birth.Date
#> 1 karisoke       AKI    MAH 2007-11-26     2007-11-26     2997-11-26
#> Variables not shown: Entry.Date (time), Depart.Date (time)
  bio_errors$error_duplicates
#> NULL
  
  # There is currently an error for animal "AKI" in the Karisoke data
  # Look at AKI's data
  lh %>% filter(Animal.Id == "AKI") %>% glimpse()
#> Observations: 1
#> Variables:
#> $ Study.Id                (fctr) karisoke
#> $ Animal.Id               (fctr) AKI
#> $ Animal.Name             (fctr) Agakiza
#> $ Birth.Date              (time) 2007-11-26
#> $ Min.Birth.Date          (time) 2007-11-26
#> $ Max.Birth.Date          (time) 2997-11-26
#> $ Birth.Date.Distribution (fctr) N
#> $ Birth.Group             (fctr) Pablo
#> $ Birth.Group.Certainty   (fctr) C
#> $ First.Born              (fctr) N
#> $ Mom.Id                  (fctr) MAH
#> $ Sex                     (fctr) F
#> $ Entry.Date              (time) 2007-11-26
#> $ Entry.Type              (fctr) B
#> $ Depart.Date             (time) 2008-12-08
#> $ Depart.Type             (fctr) D
#> $ Depart.Date.Error       (dbl) 0

  # Fix error
  lh[lh$Animal.Id == "AKI", ]$Max.Birth.Date <- ymd("2007-11-26")

  # Check again
  find_bio_errors(lh)$error_dates
#> No errors found!
#> NULL

  # Check the fertility data for errors
  fert_errors <- find_fert_errors(fert)
#> No errors found!
  fert_errors$error_dates
#> NULL
  
  # Currently no date/time errors in the fertility data
```

The function `find_mom_errors` checks to see if all the animals listed in Mom.Id in the biography table for a given study have a corresponding record in Animal.Id.

``` r
  find_mom_errors(lh)
#> $`rppn-fma`
#> [1] "AD-M"  "ANA2"  "FRA-N" "LR-N"  "NEO"   "PRI2" 
#> 
#> $amboseli
#>  [1] "EST" "ETA" "FLU" "JAN" "KUP" "LIS" "LOI" "NUB" "PIN" "RIN" "RUK"
#> [12] "SKI" "TWI"
#> 
#> $kakamega
#> [1] "Ange"
#> 
#> $gombe
#> [1] "PATINA"
#> 
#> $karisoke
#> [1] "AMR" "GUY" "IGT" "KUG" "UMH" "UMY"
#> 
#> $beza
#> character(0)
#> 
#> $ssr
#>  [1] "ABU"   "Baloo" "BB--"  "CH--"  "CT"    "Ed"    "Ed--"  "Helen"
#>  [9] "MsWs"  "Salsa" "SGTI"  "SIMB"  "ZaZU" 
#> 
#> attr(,"split_type")
#> [1] "data.frame"
#> attr(,"split_labels")
#>   Study.Id
#> 1 rppn-fma
#> 2 amboseli
#> 3 kakamega
#> 4    gombe
#> 5 karisoke
#> 6     beza
#> 7      ssr
```

We can see from this that there are 40 Mom.ID records that don't match any entry in Animal.Id and therefore need to be corrected.
