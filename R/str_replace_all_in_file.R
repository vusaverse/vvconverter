#' Replace all occurences of a pattern in a file
#'
#' @param file character, path of file to be modified
#' @param pattern character, pattern to be replaced
#' @param replacement character, replacement text
#' @param only_comments logical, should the replacement only be done in comments
#' @param collapse logical, should the lines be collapsed into a single line before replacement
#' @return NULL, the file is modified in place
#' @export
#'
str_replace_all_in_file <- function(file, pattern, replacement = "[...]", only_comments = TRUE, collapse = FALSE) {

    Lines <- readLines(file)

    if (!collapse) {
        if (only_comments) {
            Search_line <- stringr::str_detect(Lines, "^\\s*#")
        } else {
            Search_line <- T
        }

        Lines[Search_line] <- stringr::str_replace_all(Lines[Search_line], pattern = pattern, replacement = replacement)

        cat(Lines, file = file, sep = "\n")
    } else {
        Lines_collapsed <- paste(c(Lines, ""), collapse = "\n")

        Lines_collapsed <- stringr::str_replace_all(Lines_collapsed, pattern = pattern, replacement = replacement)

        cat(Lines_collapsed, file = file)
    }

}
