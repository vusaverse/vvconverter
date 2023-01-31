#' Mode (most common value)
#'
#' Determine the most common value in a vector. If two values have the same frequency,
#' the first occurring value is used.
#'
#' @param x a vector
#' @param na.rm If TRUE: Remove nas before the calculation is done
#' @examples
#' mode(c(0,3,5,7,5,3,2))
#'
#'
#' @return the most common value in the vector x
#' @export
mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
