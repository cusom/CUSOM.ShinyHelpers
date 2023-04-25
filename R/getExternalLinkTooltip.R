#' Utility function to create tooltip for external links for a given analyte
#'
#' @param analyte_name string - name of analyte to inject into tooltip
#'
#' @return shiny div containing shiny action button objects
#' @export
getExternalLinkTooltip <- function(analyte_name) {
  if (length(analyte_name) == 1) {
    return(
      CUSOMShinyHelpers::createTutorialTooltip(
        Text = glue::glue(
          "Search external sites <br />for {analyte_name}"
        ),
        onClickFunction = "",
        TooltipText = glue::glue(
          "Click any link below to search external sites for {analyte_name}"
          ),
        additionalText = "",
        size = 4
      )
    )
  }
}
