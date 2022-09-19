#' Sum 0 1
#'
#' This function is the same as sum(), with one exception: If the outcome value is higher
#' than 1, it will always return 1.
#' @param x a vector with numeric values
#' @return 0 or 1. Depending on whether the sum is greater than 0 or not.
#' @family vector calculations
#' @export
sum_0_1 <- function(x) {
  ## Add all X's together
  y <- sum(x, na.rm = T)

  ## if y is NA, it means there were no values in the data. In that case
  ## the value becomes 0
  if (is.na(y)) {
    y <- 0
    ## If y is greater than 0, it means that on one of the fields at least once
    ## a 1 has been entered. Then the value becomes 1.
  } else if (y > 0) {
    y <- 1
  }
  ## return the 0 or 1
  return(y)
}
