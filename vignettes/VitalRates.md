[Back to Respository](https://github.com/camposfa/plhdbR)

Functions for calculating vital rates
=====================================

Prepare workspace and read biography and fertility data
-------------------------------------------------------

``` r
  Sys.setenv(TZ = 'UTC')

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
#> Source: local data frame [7 x 4]
#> 
#>   Study.Id median_age_days median_age_years n_first_births
#> 1 rppn-fma     3210.5 days         8.795890             58
#> 2 amboseli     2181.0 days         5.975342            193
#> 3 kakamega     2665.0 days         7.301370            117
#> 4    gombe     5544.5 days        15.190411             54
#> 5 karisoke     3601.0 days         9.865753             55
#> 6     beza     2191.0 days         6.002740             80
#> 7      ssr     2363.5 days         6.475342             32
```

Age-specific fertility
----------------------

The function `age_specific_fertility` uses the biography and fertility tables to calculate age-specific fertility separately for each study species using discrete age-classes. The procedure follows the instructions provided by Bill Morris in the "Methods for calculating vital rates" Word file available on the PLHDB Wiki.

*Warning: this function takes ~1 minute to run.*

``` r
  asf <- age_specific_fertility(lh, fert)
  asf
#> Source: local data frame [266 x 5]
#> Groups: Study.Id
#> 
#>    Study.Id Discrete.Age.Class n_animals female_years         f
#> 1  rppn-fma                  0        83     79.81644 0.0000000
#> 2  rppn-fma                  1        90     77.35890 0.0000000
#> 3  rppn-fma                  2        75     67.61644 0.0000000
#> 4  rppn-fma                  3        62     59.33973 0.0000000
#> 5  rppn-fma                  4        61     57.40548 0.0000000
#> 6  rppn-fma                  5        83     59.61096 0.0000000
#> 7  rppn-fma                  6        68     46.36986 0.0000000
#> 8  rppn-fma                  7        42     37.69041 0.2122556
#> 9  rppn-fma                  8        38     36.10411 0.3593110
#> 10 rppn-fma                  9        35     34.04110 0.2350101
#> ..      ...                ...       ...          ...       ...
```

Stage-specific fertility between censuses
-----------------------------------------

The function `stage_specific_fertility` uses the biography and fertility tables to calculate stage-specific fertility separately for each study species. The function uses pseudo-census dates on January 1 of each year of the study. The life-history stages include (following Morris et al. 2011):

-   Newborns: individuals born between the pseudo-census dates in each interval
-   Juveniles: individuals alive but younger than the median age at first reproduction at the first pseudo-census date in each interval
-   Adults: individuals that are older than the median age at first reproduction at the first pseudo-census date in each interval

The optional logical argument `annual` determines whether fertilities are calculated for each year separately (the default is TRUE).

*Warning: this function takes ~5 minutes to run.*

``` r
  ssf <- stage_specific_fertility(lh, fert, annual = TRUE)
  ssf
#> Source: local data frame [712 x 6]
#> Groups: Study.Id, year_of
#> 
#>    Study.Id year_of age_class n_animals female_years         f
#> 1  rppn-fma    1983     adult         7    3.6438356 0.0000000
#> 2  rppn-fma    1983  juvenile         6    2.6876712 0.1936799
#> 3  rppn-fma    1983   newborn         1    0.4191781 0.0000000
#> 4  rppn-fma    1984     adult         7    3.7780822 0.4285714
#> 5  rppn-fma    1984  juvenile         8    4.1643836 0.0000000
#> 6  rppn-fma    1984   newborn         1    0.0739726 0.0000000
#> 7  rppn-fma    1986     adult         8    4.0986301 0.3750000
#> 8  rppn-fma    1986  juvenile        11    5.5945205 0.0000000
#> 9  rppn-fma    1986   newborn         2    0.8191781 0.0000000
#> 10 rppn-fma    1987     adult         8    8.0000000 0.1250000
#> ..      ...     ...       ...       ...          ...       ...
```

Age-specific and stage-specific survival
----------------------------------------

**Work in progress**
