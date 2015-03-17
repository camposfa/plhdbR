plhdbR
======

------------------------------------------------------------------------

`plhdbR` is a collection of tools to work with data from the [Primate Life History Database](https://plhdb.org/) (PLHDB).

*There are no life history or fertility data included in this package.*

To use the tools, you must have access to the PLHDB. The site is currently accessible for the working group members only.

> Karen B. Strier, Jeanne Altmann, Diane K. Brockman, Anne M. Bronikowski, Marina Cords, Linda M. Fedigan, Hilmar Lapp, Xianhua Liu, William F. Morris, Anne E. Pusey, Tara S. Stoinski and Susan C. Alberts (2010). The Primate Life History Database: a unique shared ecological data resource. *Methods in Ecology and Evolution*, 1(2), 199–211. doi: [10.1111/j.2041-210X.2010.00023.x](http://doi.org/10.1111/j.2041-210X.2010.00023.x)

`plhdbR` aims to facillitate the analysis of data in the PLHDB. It will have three main sets of utilities:

-   Functions for reading and error-checking the life history and fertility data

-   Functions for calculating vital rates

-   Functions for loading and analyzing climate data

The available functions will be documented below as they become available.

------------------------------------------------------------------------

Preparation
-----------

To use this package, you first need to install `devtools` with:

``` r
    install.packages("devtools")
```

Then, you can install the latest development version from github:

``` r
  library(devtools)
  devtools::install_github("camposfa/plhdbR")
```

This package makes heavy use of the data manipulation utilities provided by packages [stringr](http://cran.r-project.org/package=stringr), [lubridate](http://cran.r-project.org/package=lubridate), [tidyr](http://cran.r-project.org/package=tidyr), and [dplyr](http://cran.r-project.org/package=dplyr). If not already installed, plhdbR will install these packages automatically. To load them in one fell swoop, use the convenience function:

``` r
  load_plhdb_packages()
```

------------------------------------------------------------------------

Utility functions for life history and fertility data
-----------------------------------------------------

The functions `load_lh_data` and `load_fert_data` read biography and fertility csv files, respectively, created by the download button at [PLHDB](https://plhdb.org/). These functions strip away the header lines, parse any date/time columns, and return a well-ordered `dplyr::tbl_df`. To pull all the data, use search criteria like 'Study.ID != 10'. Note that the data are **not** extensively error-checked at this stage.

### Biography data

``` r
  # Assuming your file is called "biography_2015_03_17.csv"
  lh <- load_lh_data("biography_2015_03_17.csv")
  summary(lh)
#>      Study.Id      Animal.Id     Animal.Name  
#>  rppn-fma: 564   KOM    :   4          : 993  
#>  amboseli:1324   RUS    :   4   Ahab   :   2  
#>  kakamega: 599   AFR    :   3   APOLLO :   2  
#>  gombe   : 305   BUS    :   3   APPLE  :   2  
#>  karisoke: 321   FLO    :   3   AQUA   :   2  
#>  beza    : 993   GOD    :   3   Bali   :   2  
#>  ssr     : 299   (Other):4385   (Other):3402  
#>    Birth.Date                  Min.Birth.Date               
#>  Min.   :1919-07-02 00:00:00   Min.   :1914-07-03 00:00:00  
#>  1st Qu.:1988-07-15 00:00:00   1st Qu.:1988-01-02 00:00:00  
#>  Median :1999-10-13 00:00:00   Median :1999-07-30 00:00:00  
#>  Mean   :1996-08-26 14:01:26   Mean   :1996-02-19 17:40:28  
#>  3rd Qu.:2007-07-15 00:00:00   3rd Qu.:2007-07-09 00:00:00  
#>  Max.   :2014-09-17 00:00:00   Max.   :2014-09-18 00:00:00  
#>                                                             
#>  Max.Birth.Date                Birth.Date.Distribution  Birth.Group  
#>  Min.   :1924-06-30 00:00:00   N: 743                         : 546  
#>  1st Qu.:1989-08-05 00:00:00   U:3662                  Matão  : 206  
#>  Median :2000-01-02 00:00:00                           KK     : 204  
#>  Mean   :1997-05-21 14:30:32                           1.1    : 193  
#>  3rd Qu.:2007-07-17 00:00:00                           2.2    : 174  
#>  Max.   :2997-11-26 00:00:00                           1      : 170  
#>                                                        (Other):2912  
#>  Birth.Group.Certainty First.Born     Mom.Id     Sex     
#>   : 189                N:2713            : 908   F:1868  
#>  C:4031                U:1086     81     :  16   M:1988  
#>  U: 185                Y: 606     137    :  15   U: 549  
#>                                   9160   :  14           
#>                                   86     :  13           
#>                                   (Other):3433           
#>                                   NA's   :   6           
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
  fert <- load_fert_data("fertility_2015_03_17.csv")
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

------------------------------------------------------------------------

Utility functions for climate data
----------------------------------

`plhdbR` includes functionality for pulling the most recent data for several large-scale climate oscillation indices from various online data repositories. Supported climate indices include:

-   Dipole Mode index ("dmi")
-   Multivariate ENSO Index ("mei")
-   Oceanic Niño Index ("oni")
-   Southern Oscillation Index ("soi")
-   Pacific Decadal Oscillation ("pdo")
-   Atlantic Multidecadal Oscillation ("amo")
-   North Atlantic Oscillation ("nao")

The function `load_climate_index` returns a named list of monthly climate index data. Each element of the list is an object of class `dplyr::tbl_df`, an extension of R's data.frame

``` r
  indices <- load_climate_index(c("nao", "pdo"))
#> Reading PDO data from http://jisao.washington.edu/pdo/PDO.latest
#> Reading NAO data from http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table
  
  indices
#> $pdo
#> Source: local data frame [1,382 x 3]
#> 
#>       date_of value index
#> 1  1900-01-16  0.04   pdo
#> 2  1900-02-16  1.32   pdo
#> 3  1900-03-16  0.49   pdo
#> 4  1900-04-16  0.35   pdo
#> 5  1900-05-16  0.77   pdo
#> 6  1900-06-16  0.65   pdo
#> 7  1900-07-16  0.95   pdo
#> 8  1900-08-16  0.14   pdo
#> 9  1900-09-16 -0.24   pdo
#> 10 1900-10-16  0.23   pdo
#> ..        ...   ...   ...
#> 
#> $nao
#> Source: local data frame [782 x 3]
#> 
#>       date_of value index
#> 1  1950-01-16  0.92   nao
#> 2  1950-02-16  0.40   nao
#> 3  1950-03-16 -0.36   nao
#> 4  1950-04-16  0.73   nao
#> 5  1950-05-16 -0.59   nao
#> 6  1950-06-16 -0.06   nao
#> 7  1950-07-16 -1.26   nao
#> 8  1950-08-16 -0.05   nao
#> 9  1950-09-16  0.25   nao
#> 10 1950-10-16  0.85   nao
#> ..        ...   ...   ...
```
