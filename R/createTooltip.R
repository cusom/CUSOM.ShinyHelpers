
#' returns standard text with info-circle tooltip. If TooltipText is missing or NA, tooltip span element is omitted.
#'
#' @param Text Text to diplay before info circle tooltip
#' @param URL URL to open for onclick event
#' @param TooltipText text to display in tooltip on hover
#' @param ShowTooltip boolean - whether to add info tooltip or just output Text - defaults to TRUE
#' @param ... dots - additional args
#' @return HTML div with tooltip span element
#' @importFrom shiny HTML
#' @export
createTooltip <- function(Text,URL,TooltipText,ShowTooltip=TRUE, ...) {

  if(!ShowTooltip) {
    return(
      shiny::HTML(
        glue::glue('<div>{Text}</div>')
      )
    )
  }
  else {

    if(URL != "" & !is.na(URL)) {
      return(
        shiny::HTML(
          glue::glue(
            '<div>{Text}
              <span
                data-html="true"
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
        shiny::HTML(
          glue::glue(
            '<div>{Text}
              <span
                data-html="true"
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
  }
}
