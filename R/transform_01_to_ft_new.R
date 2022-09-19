#' T_F_maker
#'
#' Deprecated function to transform vector to boolean.
#' @param x Vector containg zeros and ones.
#' @family vector calculations
#' @family booleans
#' @export
t_f_maker <- function(x) {
    .Deprecated("transform_01_to_ft")

    x <- transform_01_to_ft(x)

    return(x)
}

