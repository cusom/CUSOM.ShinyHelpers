
#' returns standard text with info-circle tooltip. If TooltipText is missing or NA, tooltip span element is omitted.
#'
#' @param Text Text to diplay before info circle tooltip
#' @param URL URL to open for onclick event
#' @param TooltipText text to display in tooltip on hover
#' @return HTML div with tooltip span element
#' @export

createTooltip <- function(Text,URL,TooltipText) {

  if( TooltipText != "" & !is.na(TooltipText)) {
    return(
      HTML(
        glue('<div>{Text}
              <span
                onclick="window.open(\'{URL}\');"
                data-toggle="tooltip"
                data-placement="auto right"
                title=""
                class="fas fa-info-circle gtooltip info-tooltip"
                data-original-title="{TooltipText}">
              </span>
            </div>'
        )
      )
    )
  }
  else {
    return(
      HTML(
        glue('<div>{Text}</div>'
        )
      )
    )
  }
}
