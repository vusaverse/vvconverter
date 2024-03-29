#' Transform Logical to Yes/No and Vice Versa
#'
#' This function transforms a logical vector to a vector of yes/no strings or vice versa.
#'
#' @param x A logical or character vector.
#' @param lang The language of the yes/no strings. Default is "nl" (Dutch).
#' @return A vector of yes/no strings or a logical vector.
#' @export
transform_logical_yes_no <- function(x, lang = "nl") {
  if (is.logical(x)) {
    y <- as.character(x)
    y[x] <- "Yes"
    y[!x] <- "No"
    y <- polyglotr::google_translate(y, target_language = lang)
  } else if (is.character(x) | is.factor(x)) {
    y <- tolower(trimws(x))
    y <- polyglotr::google_translate(y, source_language = lang, target_language = "en")
    y <- y == "yes" | y == "y" | y == "Yes"
  } else {
    stop("Invalid input. Please provide a logical or character/factor vector.")
  }

  return(y)
}
