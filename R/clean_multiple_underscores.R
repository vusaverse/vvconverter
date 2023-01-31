#' clean multiple underscores
#'
#' Replaces multiple underscores into a single underscore in a vector or string.
#' @param x The vector or string to be cleaned.
#' @return cleaned vector or string.
#' @examples
#' clean_multiple_underscores("hello___world")
#'
#' @family vector calculations
#' @export
clean_multiple_underscores <- function(x) {
    if (any(grepl("__", x))) {
        x <- gsub("__", "_", x)
    }

    if (any(grepl("__", x))) {
        ## apply recursion if there still are multiple underscores in sequence
        x <- clean_multiple_underscores(x)
    }

    return(x)
}
