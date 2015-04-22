[Back to Respository](https://github.com/camposfa/plhdbR)

Functions for calculating vital rates
=====================================

Prepare workspace and [read biography and fertility data](ErrorChecking.md)
---------------------------------------------------------------------------

``` r
  library(plhdbR)
  load_plhdb_packages()

  lh <- read_bio_table("biography_2015_03_17.csv")
  fert <- read_fert_table("fertility_2015_03_17.csv")
```

It's a good idea to [error-check](ErrorChecking.md) the data extensively before running the fuctions below.

Median age at first reproduction
--------------------------------

The function `median_age_first_rep` uses the biography data to calculate the median age at first reproduction for each study species.

``` r
  median_age_first_rep(lh)
#> Source: local data frame [7 x 3]
#> 
#>   Study.Id median_age_days median_age_years
#> 1 rppn-fma     3210.5 days         8.795890
#> 2 amboseli     2181.0 days         5.975342
#> 3 kakamega     2665.0 days         7.301370
#> 4    gombe     5544.5 days        15.190411
#> 5 karisoke     3601.0 days         9.865753
#> 6     beza     2191.0 days         6.002740
#> 7      ssr     2363.5 days         6.475342
```

Age-specific fertility
----------------------

The function `age_specific_fertility` uses the biography and fertility tables to calculate age-specific fertility separately for each study species using discrete age-classes. The procedure follows the instructions provided by Bill Morris in the "Methods for calculating vital rates" Word file available on the PLHDB Wiki. *Warning*: this function takes ~1 minute to run.

``` r
  age_specific_fertility(lh, fert)
#> Source: local data frame [266 x 4]
#> Groups: Study.Id
#> 
#>    Study.Id Discrete.Age.Class         f  n
#> 1  rppn-fma                  0 0.0000000 83
#> 2  rppn-fma                  1 0.0000000 90
#> 3  rppn-fma                  2 0.0000000 75
#> 4  rppn-fma                  3 0.0000000 62
#> 5  rppn-fma                  4 0.0000000 61
#> 6  rppn-fma                  5 0.0000000 83
#> 7  rppn-fma                  6 0.0000000 68
#> 8  rppn-fma                  7 0.2122659 42
#> 9  rppn-fma                  8 0.3593069 38
#> 10 rppn-fma                  9 0.2350103 35
#> ..      ...                ...       ... ..
```

Stage-specific fertility between censuses
-----------------------------------------

**Work in progress**
