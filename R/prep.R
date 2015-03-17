#' #' Loads various data munging packages.
#'
#' @export
#' @examples
#' load_plhdb_packages()
load_plhdb_packages <- function (...)
{
  packages <- c('stringr', 'plyr', 'lubridate', 'tidyr', 'dplyr')
  for (package in packages) {
    if (!isTRUE(require(package, character.only = TRUE))) {
      install.packages(package)
      require (package, character.only = TRUE, ...)
    } else
      require(package, character.only = TRUE, ...)
  }
}