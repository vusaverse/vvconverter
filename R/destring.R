#' Convert character vector to numeric, ignoring irrelevant characters.
#'
#' @param x A vector to be operated on
#' @param keep Characters to keep in, in bracket regular expression form.
#' Typically includes 0-9 as well as the decimal separator (. in the US and , in
#' Europe).
#' @examples
#' destring("24k")
#' destring("5,5")
#'
#' @return vector of type numeric
#' @family vector berekeningen
#' @export destring
destring <- function(x, keep="0-9.-") {
    return(as.numeric(gsub(paste("[^", keep, "]+", sep = ""), "", x)) )
}
