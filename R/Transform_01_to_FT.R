#' Transform 01 to FT
#'
#' If the vector is a 0/1 vector, it is converted to a logical one
#' TRUE/FALSE vector. This transformation is performed only if
#' the vector contains only values 0, 1, or NA. If this is not the case
#' returns the original variable.
#' This transformation can be done on numeric, string, and factor vectors.
#'
#' @param x the vector to be tested and transformed.
#' @return The transformed vector if a transformation is possible.
#' If no transformation is possible, the original vector
#' returned.
#' @family vector calculations
#' @family booleans
#' @examples
#' vector <- c(0, 1, 0, 1, 1, 1, 0)
#' transform_01_to_ft(vector)
#' @export
transform_01_to_ft <- function(x) {
  ## Check if this vector is a 0/1 variable converted to T/F
  ## can become
  if (test_01(x)) {
    if (is.numeric(x)) {
      ## Transform to a logical vector and return it
      ## If the vector is numeric, 1 is encoded as TRUE
      return(x == 1)
    } else if (is.character(x) | is.factor(x)) {
      ## Transform to a logical vector and return it
      ## If the vector is string or factor, "1" is coded as TRUE
      return(x == "1")
    }
  } else {
    ## If the variable is not a 0/1 variable, the original vector
    ## returned: x
    return(x)
  }
}


#' Test 01
#'
#' This function tests whether the vector is actually a boolean, but is encoded
#' as a 0/1 variable. The function checks for numeric vectors
#' whether the only occurring values are 0, 1, or NA. At character and factor
#' vectors checks whether the only occurring values are "0", "1", or NA
#' to be.
#' If there is a 0/1 variable, TRUE is returned, in all others
#' cases FALSE.
#' @param x The vector to test
#' @return A TRUE/FALSE value on the test
#' @family tests
#' @family booleans
#' @examples
#' vector <- c(0, 1, 0, 1, 1, 1, 0)
#' test_01(vector)
#' @export
test_01 <- function(x) {
  ## First determine the class of x. Depending on that, another test
  ## executed.
  if (is.numeric(x)) {
    ## The values to test if the vector is numeric
    values <- c(0, 1, NA)
  } else if (is.character(x) | is.factor(x)) {
    ## The values to test if the vector is a string or factor
    values <- c("0", "1", NA)
  } else {
    ## If the vector is not a numeric, string, or factor, it returns FALSE
    return(FALSE)
  }
  ## Test for each occurring value whether it is in the values variable
  ## Check if everything is TRUE, and return the result
  return(all(x %in% values))
}
