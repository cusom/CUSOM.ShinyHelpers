#' parses a delimited string looking for standard set of delimiters (excludes "-")
#'
#' @param string string with any number of delimiters ("," "." ";" ":")
#' @param position parsed element number to return
#' @return parsed element number from original string
#' @export
parseDelimitedString <- function(string, position = 1) {

  patterns <- "[,,.,;,:]+"

  replacement <- "|"

  cleanedString <- stringi::stri_replace_all_regex(string, patterns, replacement, vectorize_all = FALSE)

  return(str_split(cleanedString, "\\|", simplify = TRUE)[position])

}
