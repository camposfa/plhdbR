plhdbR
======

`plhdbR` is a collection of tools to work with data from the [Primate Life History Database](https://plhdb.org/) (PLHDB).

*There are no life history data included in this package.*

To use the tools, you must have access to the PLHDB. The site is currently accessible for the working group members only.

> Karen B. Strier, Jeanne Altmann, Diane K. Brockman, Anne M. Bronikowski, Marina Cords, Linda M. Fedigan, Hilmar Lapp, Xianhua Liu, William F. Morris, Anne E. Pusey, Tara S. Stoinski and Susan C. Alberts (2010). The Primate Life History Database: a unique shared ecological data resource. *Methods in Ecology and Evolution*, 1(2), 199–211. doi: [10.1111/j.2041-210X.2010.00023.x](http://doi.org/10.1111/j.2041-210X.2010.00023.x)

`plhdbR` aims to facillitate the analysis of data in the PLHDB. It will have three main sets of utilities:

-   Functions for reading and error-checking the life history and fertility data

-   Functions for calculating vital rates

-   Functions for loading and analyzing climate data

The available functions will be documented below as they become available.

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

Utility functions for climate data
----------------------------------

`plhdbR` includes functionality for pulling the most recent data for several large-scale climate oscillation indices from various online data repositories. Supported climate indices include:

-   Dipole Mode indes ("dmi")
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
