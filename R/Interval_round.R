#' Interval round
#'
#' Function to round numeric values in a vector to values from
#' an interval sequence.
#'
#' @param x The numeric vector to adjust
#' @param interval The interval sequence
#' @family vector calculations
#' @export
interval_round <- function(x, interval){
  x <- interval[ifelse(x < min(interval), 1, findInterval(x, interval))]
  return(x)
}
