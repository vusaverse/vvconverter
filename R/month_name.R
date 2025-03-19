#' Month Name
#'
#' Transform month from numeric to equivalent in specified language.
#'
#' @param month_numeric Numeric in range 1 - 12.
#' @param lang The language of the month names. Default is "nl" (Dutch).
#' @return Character string representation of month in specified language.
#' @export
#' @family vector calculations
month_name <- function(month_numeric, lang = "nl") {
  ## Check if the value is numeric
  checkmate::expect_numeric(month_numeric, lower = 1, upper = 12)
  ## Create a vector with the months
  months <- c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  )
  ## Translate the month names to the specified language
  months <- tryCatch({
    polyglotr::google_translate(months, target_language = lang)
  }, error = function(e) {
    message("Translation service is currently unavailable or has changed. Please try again later.")
    return(NULL) # Return NULL if translation fails
  })
  if (is.null(months)) {
    return(NULL)
  }
  ## Return the correct month
  months[month_numeric]
}
