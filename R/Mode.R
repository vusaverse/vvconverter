## ++++++++++++++++++++++++++++++++++++++++++++++++++ ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Mode.R ####
## ++++++++++++++++++++++++++++++++++++++++++++++++++ ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Student Analytics VU University Amsterdam
## Copyright 2022 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Distribution outside the VU: Yes
##
## Purpose: Calculate the mode of a vector
##
## Dependencies: Dependency
##
## Datasets: Datasets
##
## Comments:
## 1) None.
## 2) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++ ++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
##1) ___.
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++ ++++++++++++++++++++++++++++++++++++++++++++++++++
## History:
## 26-07-2022: TK: Create file
## ++++++++++++++++++++++++++++++++++++++++++++++++++ ++++++++++++++++++++++++++++++++++++++++++++++++++


#' Mode (most common value)
#'
#' Determine the most common value in a vector. If two values have the same frequency,
#' the first occurring value is used.
#'
#' @param x a vector
#' @param na.rm If TRUE: Remove nas before the calculation is done
#'
#' @return the most common value in the vector x
#' @export
mode <- function(x, na.rm = F) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
