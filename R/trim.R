#' Trim
#'
#' Trim both leading and trailing whitespaces from string.
#'
#' @param x A text string.
#' @return Cleaned string.
#' @export
trim  <- function (x) gsub("^\\s+|\\s+$", "", x)

#' LTrim
#'
#' Trim leading whitespace from sting.
#' @param x A text string.
#' @return Cleaned string.
#' @export
ltrim <- function (x) gsub("^\\s+", "", x)

#' RTrim
#'
#' Trim trailing whitespaces from string.
#' @param x A text string.
#' @return Cleaned string.
#' @export
rtrim <- function (x) gsub("\\s+$", "", x)
