#' Interval round
#'
#' Function to round numeric values in a vector to values from
#' an interval sequence.
#'
#' @param x The numeric vector to adjust
#' @param interval The interval sequence
#' @examples
#' interval_round(c(5,4,2,6), interval = seq(1:4))
#'
#' @family vector calculations
#' @return The vector corrected for the given interval
#' @export
interval_round <- function(x, interval) {
  x <- interval[ifelse(x < min(interval), 1, findInterval(x, interval))]
  return(x)
}
