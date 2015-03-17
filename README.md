plhdR
=====

plhdR is a collection of tools to work with data from the Primate Life History Database.

*There are no life history data included in this package.* To use the tools, you must have access to the [Primate Life History Database](https://plhdb.org/). This site is currently only accessible for the working group members.

> Karen B. Strier, Jeanne Altmann, Diane K. Brockman, Anne M. Bronikowski, Marina Cords, Linda M. Fedigan, Hilmar Lapp, Xianhua Liu, William F. Morris, Anne E. Pusey, Tara S. Stoinski and Susan C. Alberts (2010). The Primate Life History Database: a unique shared ecological data resource. *Methods in Ecology and Evolution*, 1(2), 199–211. doi: [10.1111/j.2041-210X.2010.00023.x](http://doi.org/10.1111/j.2041-210X.2010.00023.x)

plhdR is focused on facillitating the analysis of life history data. It will have three main sets of utilities:

-   Functions for reading and error-checking the life history and fertility data

-   Functions for calculating vital rates

-   Functions for loading and analyzing climate data

You can install the latest development version from github with:

``` r
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("camposfa/plhdR")
```
