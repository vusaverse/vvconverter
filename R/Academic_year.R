#' Academic year
#'
#' In this function, a date is translated to the academic year in which it
#' falls. This is based on a start of the academic year on the
#' 1st of September.
#'
#' @param x A date, or vector with multiple dates. POSIXct is also accepted.
#' @param start_1_oct Does the academic year start on the 1st of October? default FALSE:
#' based on September 1st
#' @return The academic year in which the specified date falls
#' @examples
#' academic_year(lubridate::today())
#'
#' @family vector calculations
#' @export
academic_year <- function(x, start_1_oct = FALSE){

  ## Convert POSIXct and POSIXt to date class
  if (any(class(x) %in% c("POSIXct", "POSIXt"))) {
    x <- lubridate::as_date(x)
  }

  ## if the class is not a date, then this function cannot be executed
  stopifnot(class(x) == "Date")

  ## determine the calendar year of x
  calendar_year <- lubridate::year(x)

  ## determine for the calendar year in which year the academic year starts and ends
  ## The NA values that cannot be converted to dates should remain NA. Hence
  ## the 'suppressWarnings()
  if (start_1_oct) {
    acad_year_start <- suppressWarnings(lubridate::dmy(paste("01-10",calendar_year, sep = "-")))
    acad_year_end <- suppressWarnings(lubridate::dmy(paste("01-10",calendar_year + 1, sep = "-")))
  } else {
    acad_year_start <- suppressWarnings(lubridate::dmy(paste("01-09",calendar_year, sep = "-")))
    acad_year_end <- suppressWarnings(lubridate::dmy(paste("01-09",calendar_year + 1, sep = "-")))
  }

  ## If the specified date falls on or after the start of the academic year,
  ## returned the calendar year
  ## If the specified date falls before the start of the academic year,
  ## it to the previous calendar year: so calendar_year - 1
  x <- dplyr::case_when(x >= acad_year_start ~ calendar_year,
                        x < acad_year_end ~ calendar_year - 1)
  return(x)

}
