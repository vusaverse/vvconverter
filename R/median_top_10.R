#' Median top 10 percentage
#'
#' Calculate the median of the top ten percentage of the values.
#'
#' @param x A numerical vector
#' @param na.rm Default TRUE: Remove NAs, before calculations.
#' @examples
#' median_top_10(mtcars$cyl)
#'
#'
#' @return A numerical value
#' @export
median_top_10 <- function(x, na.rm = FALSE) {
  ## Select only the top ten percentage students.
  Top_10 <- x[x >= stats::quantile(x, 0.9, na.rm = na.rm)]
  ## Calculate the median of this group.
  y <- stats::median(Top_10, na.rm = na.rm)
  ## Return the median
  return(y)
}
