[Back to Respository](https://github.com/camposfa/plhdbR)

Functions for working with the PLHDB data tables
================================================

Prepare workspace
-----------------

``` r
  library(plhdbR)
  load_plhdb_packages()
```

Utility functions for climate data
----------------------------------

`plhdbR` includes functions for pulling the most recent data for several large-scale climate oscillation indices from various online data repositories. Supported climate indices include:

-   Dipole Mode index ("dmi")
-   Multivariate ENSO Index ("mei")
-   Oceanic Niño Index ("oni")
-   Southern Oscillation Index ("soi")
-   Pacific Decadal Oscillation ("pdo")
-   Atlantic Multidecadal Oscillation ("amo")
-   North Atlantic Oscillation ("nao")

The function `load_climate_index` returns a named list of monthly climate index data. Each element of the list is an object of class `dplyr::tbl_df`.

``` r
  indices <- load_climate_index(c("nao", "mei"))
#> Reading MEI data from http://www.esrl.noaa.gov/psd/enso/mei/table.html
#> Reading NAO data from http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table
  
  indices
#> $mei
#> Source: local data frame [783 x 3]
#> 
#>       date_of  value index
#> 1  1950-01-01 -1.027   mei
#> 2  1950-02-01 -1.149   mei
#> 3  1950-03-01 -1.298   mei
#> 4  1950-04-01 -1.061   mei
#> 5  1950-05-01 -1.416   mei
#> 6  1950-06-01 -1.372   mei
#> 7  1950-07-01 -1.332   mei
#> 8  1950-08-01 -1.061   mei
#> 9  1950-09-01 -0.584   mei
#> 10 1950-10-01 -0.402   mei
#> ..        ...    ...   ...
#> 
#> $nao
#> Source: local data frame [783 x 3]
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

Squash all the list elements together to a single `tbl_df` with `dplyr::bind_rows`:

``` r
  indices_df <- dplyr::bind_rows(indices)
  summary(indices_df)
#>     date_of                        value              index          
#>  Min.   :1950-01-01 00:00:00   Min.   :-3.180000   Length:1566       
#>  1st Qu.:1966-04-19 18:00:00   1st Qu.:-0.676750   Class :character  
#>  Median :1982-08-08 12:00:00   Median : 0.010000   Mode  :character  
#>  Mean   :1982-08-08 17:43:54   Mean   : 0.006591                     
#>  3rd Qu.:1998-11-27 06:00:00   3rd Qu.: 0.683750                     
#>  Max.   :2015-03-16 00:00:00   Max.   : 3.040000
```
