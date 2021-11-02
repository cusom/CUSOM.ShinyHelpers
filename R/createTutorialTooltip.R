#' returns standard turtorial text with info-circle tooltip. If TooltipText is missing or NA, tooltip element is omitted.
#'
#' @param Text Text to diplay before info circle tooltip
#' @param onClickFunction function and arguments to be bound to onclick event
#' @param TooltipText text to display in tooltip on hover
#' @param additionalText additional text to be shown below text / tooltip line
#' @param size size of H tag to use - defaults to 3 for <h3>
#' @return HTML div with tooltip span element
#' @export

createTutorialTooltip <- function(Text,onClickFunction,TooltipText,additionalText, size = "3") {

  opentag <- glue("<h{size}>")
  closetag <- glue("</h{size}>")

  if( TooltipText != "" & !is.na(TooltipText)) {
    return(
      HTML(
        glue(
          '{opentag}{Text}
              <span onclick=\"{onClickFunction}\"
                data-html="true"
                data-toggle="tooltip"
                data-placement="auto right"
                title=""
                class="fas fa-info-circle gtooltip info-tooltip"
                data-original-title="{TooltipText}">
              </span>
            {closetag}
            {additionalText}
           '
        )
      )
    )
  }
  else {
    return(
      HTML(
        glue('{opentag}{Text}{closetag}'
        )
      )
    )
  }
}
