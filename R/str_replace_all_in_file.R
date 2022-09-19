#' Vervang alles in een bestand
#'
#' Vervang alle matches op een reguliere expressie in een bestand. Hiermee kunnen
#' wijzigingen in scripts op grote schaal doorgevoerd worden.
#'
#' @param file Het bestandspad van het script
#' @param pattern De reguliere expressie waarop gematcht wordt
#' @param replacement Een character string die in de plaats van de match komt
#' @param alleen_comments TRUE: Doorzoek alleen regels die beginnen met (eventueel
#' spaties en een "#". Dit werkt alleen als collapse == FALSE
#' @param collapse FALSE (standaard): Iedere regel wordt één voor één doorzocht.
#' Als TRUE wordt alle tekst in één keer doorzocht, hierbij wordt een nieuwe regel
#' aangegeven met "\\n". Zo is het mogelijk om reguliere expressies over meerdere
#' regels heen te detecteren. (alleen_comments werkt niet als collapse op TRUE staat)
#' @export
str_replace_all_in_file <- function(file, pattern, replacement = "[...]", alleen_comments = TRUE, collapse = FALSE) {

    Lines <- readLines(file)

    if (!collapse) {
        if (alleen_comments) {
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
