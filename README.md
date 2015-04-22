plhdbR
======

`plhdbR` is a collection of tools to work with data from the [Primate Life History Database](https://plhdb.org/) (PLHDB).

> Karen B. Strier, Jeanne Altmann, Diane K. Brockman, Anne M. Bronikowski, Marina Cords, Linda M. Fedigan, Hilmar Lapp, Xianhua Liu, William F. Morris, Anne E. Pusey, Tara S. Stoinski and Susan C. Alberts (2010). The Primate Life History Database: a unique shared ecological data resource. *Methods in Ecology and Evolution*, 1(2), 199–211. doi: [10.1111/j.2041-210X.2010.00023.x](http://doi.org/10.1111/j.2041-210X.2010.00023.x)

*There are no life history or fertility data included in this package.*

To use the tools, you must have access to the PLHDB. The site is currently accessible for the working group members only.

`plhdbR` aims to facillitate the analysis of data in the PLHDB. It will contain three main sets of tools:

-   Functions that make it easy to read the life history and fertility data into R for further analysis

-   Functions for calculating vital rates

-   Functions for retrieving and analyzing up-to-date climate data

Additional functionality may be added in the future. The available functions will be documented below as they become available.

Preparation
-----------

To use this package, you first need to install `devtools` with:

``` r
    install.packages("devtools")
```

Then, you can install the latest development version of `plhdbR` from github:

``` r
  library(devtools)
  devtools::install_github("camposfa/plhdbR")
```

After you have installed the package once, you can simply load it in the future using:

``` r
  library(plhdbR)
```

This package makes heavy use of the data manipulation packages [stringr](http://cran.r-project.org/package=stringr), [lubridate](http://cran.r-project.org/package=lubridate), [tidyr](http://cran.r-project.org/package=tidyr), and [dplyr](http://cran.r-project.org/package=dplyr). If not already installed, `plhdbR` will install these packages automatically. It also provides a convenient function to load them all into your R session, if you want to use them for other tasks:

``` r
  load_plhdb_packages()
```

Vignettes
---------

[Reading and error-checking the PLHDB data tables](vignettes/ErrorChecking.md)

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
