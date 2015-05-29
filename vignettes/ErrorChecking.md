[Back to Respository](https://github.com/camposfa/plhdbR)

Functions for working with the PLHDB data tables
================================================

Prepare workspace
-----------------

``` r
  Sys.setenv(TZ = 'UTC')
  
  library(plhdbR)
  load_plhdb_packages()
```

Reading data
------------

The functions `read_bio_table` and `read_fert_table` read csv files of biography and fertility data, respectively, created by the download buttons for these tables on the [PLHDB website](https://plhdb.org/). These functions strip away blank lines and header lines, parse any date/time columns, and return a well-ordered `dplyr::tbl_df`, an extension of R's `data.frame`. To pull all the data from a given table, use search criteria like `'Study.ID != 10'`. Note that the data are **not** extensively error-checked at this stage. If you try to feed these functions a normal csv file, bad things might happen.

### Biography data

``` r
  
  # Assuming your file is called "biography_2015_05_20.csv"
  lh <- read_bio_table("../data/biography_2015_05_20.csv")
  summary(lh)
#>      Study.Id      Animal.Id      Animal.Name  
#>  rppn-fma: 562   KOM    :   4   Susuruka:   4  
#>  amboseli:1324   RUS    :   4   Ahab    :   2  
#>  kakamega: 599   SUS    :   4   APOLLO  :   2  
#>  gombe   : 313   AFR    :   3   APPLE   :   2  
#>  karisoke: 343   AMA    :   3   AQUA    :   2  
#>  beza    : 993   BUS    :   3   (Other) :3436  
#>  ssr     : 307   (Other):4420   NA's    : 993  
#>    Birth.Date                  Min.Birth.Date               
#>  Min.   :1919-07-02 00:00:00   Min.   :1914-07-03 00:00:00  
#>  1st Qu.:1988-08-08 00:00:00   1st Qu.:1988-02-01 00:00:00  
#>  Median :1999-12-13 00:00:00   Median :1999-08-16 00:00:00  
#>  Mean   :1996-10-17 04:21:01   Mean   :1996-03-17 05:50:43  
#>  3rd Qu.:2007-07-21 00:00:00   3rd Qu.:2007-07-19 00:00:00  
#>  Max.   :2015-03-20 00:00:00   Max.   :2015-03-15 00:00:00  
#>                                NA's   :19                   
#>  Max.Birth.Date                Birth.Date.Distribution  Birth.Group  
#>  Min.   :1924-06-30 00:00:00   N: 778                  KK     : 209  
#>  1st Qu.:1989-08-20 06:00:00   U:3663                  Matão  : 206  
#>  Median :2000-01-25 12:00:00                           1.1    : 193  
#>  Mean   :1997-03-24 15:10:30                           2.2    : 174  
#>  3rd Qu.:2007-07-20 00:00:00                           1      : 170  
#>  Max.   :2015-03-25 00:00:00                           (Other):2941  
#>  NA's   :19                                            NA's   : 548  
#>  Birth.Group.Certainty First.Born     Mom.Id     Sex     
#>  C   :4066             N:2749     81     :  16   F:1881  
#>  U   : 187             U:1094     137    :  15   M:2011  
#>  NA's: 188             Y: 598     9160   :  14   U: 549  
#>                                   TZ     :  14           
#>                                   86     :  13           
#>                                   (Other):3461           
#>                                   NA's   : 908           
#>    Entry.Date                  Entry.Type  Depart.Date                 
#>  Min.   :1963-01-15 00:00:00   B:3092     Min.   :1963-04-29 00:00:00  
#>  1st Qu.:1992-01-18 00:00:00   C: 457     1st Qu.:1998-08-28 00:00:00  
#>  Median :2002-01-23 00:00:00   I: 319     Median :2009-03-14 00:00:00  
#>  Mean   :1999-01-10 20:01:59   O: 573     Mean   :2005-02-16 09:29:42  
#>  3rd Qu.:2008-02-20 00:00:00              3rd Qu.:2013-07-15 00:00:00  
#>  Max.   :2015-12-23 00:00:00              Max.   :2015-04-30 00:00:00  
#>                                                                        
#>  Depart.Type Depart.Date.Error
#>  D:2166      Min.   :0.00000  
#>  E: 107      1st Qu.:0.00000  
#>  O:1544      Median :0.00000  
#>  P: 624      Mean   :0.06784  
#>              3rd Qu.:0.01000  
#>              Max.   :3.00000  
#> 
```

### Fertility data

``` r
  
  # Assuming your file is called "fertility_2015_05_20.csv"
  fert <- read_fert_table("../data/fertility_2015_05_20.csv")
  summary(fert)
#>      Study.Id     Animal.Id      Start.Date                  Start.Type
#>  rppn-fma:151   BLAN   :   8   Min.   :1963-05-15 00:00:00   B:1086    
#>  amboseli:618   DOS-   :   8   1st Qu.:1990-07-15 00:00:00   C: 192    
#>  kakamega:229   KATH   :   8   Median :1999-02-02 00:00:00   I: 114    
#>  gombe   :177   SERI   :   8   Mean   :1997-07-30 08:29:33   O: 459    
#>  karisoke:210   LIMP   :   7   3rd Qu.:2006-11-01 12:00:00             
#>  beza    :252   NYLA   :   7   Max.   :2015-03-20 00:00:00             
#>  ssr     :214   (Other):1805                                           
#>    Stop.Date                   Stop.Type
#>  Min.   :1965-02-14 00:00:00   D:847    
#>  1st Qu.:1997-06-17 00:00:00   E: 88    
#>  Median :2008-08-03 00:00:00   O:838    
#>  Mean   :2004-05-08 13:23:37   P: 78    
#>  3rd Qu.:2013-06-28 00:00:00            
#>  Max.   :2015-04-30 00:00:00            
#> 
```

Error-checking data
-------------------

### Problems with dates and duplicate animals

The functions `find_bio_errors` and `find_fert_errors` scan the loaded biography and fertility data, respectively, looking for errors of various kinds. Each function takes as an argument the relevant `dplyr::tbl_df` generated by the `read_..._table` functions listed above. `find_bio_errors` will find dates that are unrealistic as well as duplicate entries for the same (Study.Id, Animal.Id) combination. These are returned as named list elements `$error_dates` and `$error_duplicates`. `find_fert_errors` only scans the date/time fields for errors, since there are multiple fertility entries for some individuals. This is returned in a list with named element `$error_dates`.

``` r
  
  # Check the biography data for errors
  bio_errors <- find_bio_errors(lh)
  
  bio_errors$error_dates %>% data.frame()
#>    Study.Id Animal.Id Mom.Id Birth.Date Min.Birth.Date Max.Birth.Date
#> 1  karisoke       IZI    IZU 2005-09-02           <NA>           <NA>
#> 2  karisoke    FATINF    FAT 2014-09-18           <NA>           <NA>
#> 3  karisoke       BUK    KUB 2014-03-15           <NA>           <NA>
#> 4  karisoke       ISK    POP 2005-08-25           <NA>           <NA>
#> 5  karisoke       MAS    IKZ 2014-04-24           <NA>           <NA>
#> 6  karisoke    UKUINF    UKU 2014-12-25           <NA>           <NA>
#> 7  karisoke       UBO    NYB 2013-09-17           <NA>           <NA>
#> 8  karisoke    TMSINF    TMS 2014-10-22           <NA>           <NA>
#> 9  karisoke    TEKINF    TEK 2014-08-01     2014-07-01     2014-09-01
#> 10 karisoke    KRNINF    KRN 2014-11-17           <NA>           <NA>
#> 11 karisoke       NDE    NAH 2013-10-21           <NA>           <NA>
#> 12 karisoke       ISN    KUB 2013-11-01           <NA>           <NA>
#> 13 karisoke    GUTINF    GUT 2015-02-24           <NA>           <NA>
#> 14 karisoke    BSHINF    BSH 2014-03-31           <NA>           <NA>
#> 15 karisoke       BIR    TAR 2014-05-23           <NA>           <NA>
#> 16 karisoke    MAHINF    MAH 2015-03-10           <NA>           <NA>
#> 17 karisoke    KRDINF    KRD 2015-01-02           <NA>           <NA>
#> 18 karisoke    TAMINF    TAM 2014-04-14           <NA>           <NA>
#>    Entry.Date Depart.Date
#> 1  2013-08-10  2015-04-30
#> 2  2014-09-18  2015-04-30
#> 3  2014-03-15  2014-09-04
#> 4  2013-08-10  2015-04-30
#> 5  2014-04-24  2015-04-30
#> 6  2014-12-25  2015-02-23
#> 7  2013-09-17  2015-04-30
#> 8  2014-10-22  2014-10-30
#> 9  2015-12-23  2015-04-30
#> 10 2014-11-17  2015-01-30
#> 11 2013-10-21  2015-04-30
#> 12 2013-11-01  2015-04-30
#> 13 2015-02-24  2015-04-30
#> 14 2014-03-31  2014-04-22
#> 15 2014-05-24  2014-10-01
#> 16 2015-03-10  2015-04-30
#> 17 2015-01-02  2015-03-21
#> 18 2014-04-14  2014-05-26
  
  bio_errors$error_duplicates
#> Source: local data frame [1 x 3]
#> Groups: Study.Id
#> 
#>   Study.Id Animal.Id n_records
#> 1 karisoke       SUS         4

  # Check the fertility data for errors
  fert_errors <- find_fert_errors(fert)
#> No errors found!
  
  fert_errors$error_dates
#> NULL
```

### Problems with the Mom.Id field in the biography table

The function `find_mom_id_errors` checks to see if all the animals listed in Mom.Id in the biography table for a given study have a corresponding record in Animal.Id. It is important to note that *not all of these are errors!* In some studies, the mother can be known but excluded from the biography table for a variety of reasons. It would be a good idea to double-check the cases listed below.

``` r
  
  find_mom_id_errors(lh)
#> $`rppn-fma`
#> [1] "ANA2"
#> 
#> $amboseli
#>  [1] "EST" "ETA" "FLU" "JAN" "KUP" "LIS" "LOI" "NUB" "PIN" "RIN" "RUK"
#> [12] "SKI" "TWI"
#> 
#> $kakamega
#> character(0)
#> 
#> $gombe
#> [1] "PATINA"
#> 
#> $karisoke
#> [1] "AMR" "GUY" "IGT" "IZU" "KUG" "UMH" "UMY"
#> 
#> $beza
#> character(0)
#> 
#> $ssr
#> [1] "BB--" "CH--"
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

### Problems with animals that are supposed to be first-born offspring

The function `find_first_born_errors` checks for two kinds of errors regarding first-born offspring.

``` r
  
  first_born_errors <- find_first_born_errors(lh)
  
```

First, there should be a known Mom.Id (not blank or NA) for any animal that has "N" or "Y" in the First.Born column because if the animal's first-born status is known ("N", or "Y"), then the researchers must know the mother. Cases that violate this rule are returned in the named list element `$unknown_mother_first_born`.

``` r
  
  first_born_errors$unknown_mother_first_born
#> Source: local data frame [3 x 6]
#> 
#>   Study.Id Mom.Id Animal.Id Animal.Name Birth.Date First.Born
#> 1     beza     NA       131          NA 1990-07-15          N
#> 2     beza     NA      9012          NA 1985-07-15          Y
#> 3     beza     NA      9023          NA 1979-07-15          N
```

Second, any given female should have no more than one first-born offspring. Multiple offspring that are identified as first-born but attributed to the same female are returned in the named list element `$multiple_first_born`.

``` r
  
  first_born_errors$multiple_first_born
#> NULL
```
