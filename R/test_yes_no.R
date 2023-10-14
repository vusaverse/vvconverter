#' Translate Yes/No Responses
#'
#' This function translates yes/no responses from a given language to English.
#'
#' @param responses A vector of responses.
#' @param source_language The language of the responses. Default is "nl" (Dutch).
#' @return A vector of translated responses.
#' @export
translate_yes_no <- function(responses, source_language = "nl") {
  if (is.character(responses) | is.factor(responses)) {
    return(polyglotr::google_translate(responses, source_language = source_language, target_language = "en"))
  } else {
    return(FALSE)
  }
}

#' Test Yes/No Responses
#'
#' This function tests if a vector of responses are yes or no.
#'
#' @param responses A vector of responses.
#' @return A logical vector indicating if each response is yes or no.
#' @export
test_yes_no <- function(responses) {
  values <- c("yes", "Yes", "No", "no", "y", "n", NA)
  return(all(tolower(trimws(responses)) %in% values))
}
