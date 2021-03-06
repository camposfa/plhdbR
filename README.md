plhdbR
======

![PLHDB Research Sites](plhdb.jpg)

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

[Calculating vital rates](vignettes/VitalRates.md)

[Obtaining and analyzing climate data](vignettes/Climate.md)
