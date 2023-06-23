#' Create a group of radio buttons with common input
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param fieldSetData A \code{tibble} of values used to build group of radio buttons.
#' @param label Display label for the control.
#' @param selected The values that should be initially selected,
#' (if not specified then defaults to the first value).
#' @param status Add a class to the radio,
#' you can use Bootstrap status like 'info', 'primary', 'danger', 'warning' or 'success'.
#' @param shape Shape of the radio between \code{square}, \code{curve} and \code{round}.
#' @param outline Color also the border of the radio (\code{TRUE} or \code{FALSE}).
#' @param fill Fill the radio with color (\code{TRUE} or \code{FALSE}).
#' @param thick Make the content inside radio smaller (\code{TRUE} or \code{FALSE}).
#' @param animation Add an animation when radio is checked, a value between
#' \code{smooth}, \code{jelly}, \code{tada}, \code{rotate}, \code{pulse}.
#' @param icon Optional, display an icon on the radio, must be an icon created with \code{icon}.
#' @param plain Remove the border when radio is checked (\code{TRUE} or \code{FALSE}).
#' @param bigger Scale the radio a bit bigger (\code{TRUE} or \code{FALSE}).
#' @param inline If \code{TRUE}, render the choices inline (i.e. horizontally).
#' @param width The width of the input, e.g. `400px`, or `100\%`.
#'
#' @return returns original dataframe if threshold is met for each group member. If threshold not met, NULL is returned
#' @export
prettyRadioButtonsFieldSet <- function(
    inputId,
    fieldSetData,
    label,
    selected = NULL,
    status = "primary",
    shape = c("round", "square", "curve"),
    outline = FALSE,
    fill = FALSE,
    thick = FALSE,
    animation = NULL,
    icon = NULL,
    plain = FALSE,
    bigger = FALSE,
    inline = FALSE,
    width = NULL
) {
  
  status <- match.arg(status, c("default", "primary", "success", "info", "danger", "warning"))

  shape <- match.arg(shape)

  selected <- shiny::restoreInput(id = inputId, default = selected)

  options <- htmltools::tags$div(
    sapply(
      as.list(unique(fieldSetData$FieldSet)),
      createFieldSet,
      fieldSetData = fieldSetData,
      inputId = inputId,
      label = label,
      selected = selected,
      status = status,
      shape = shape,
      outline = outline,
      fill = fill,
      thick = thick,
      animation = animation,
      icon = icon,
      plain = plain,
      bigger = bigger,
      inline = inline,
      width = width
    )
  )

  divClass <- "form-group shiny-input-radiogroup shiny-input-container"

  if (inline) {
    divClass <- paste(divClass, "shiny-input-container-inline")
  }

  radioTag <- htmltools::tags$div(
    id = inputId,
    style = if (!is.null(width)) paste0("width: ", htmltools::validateCssUnit(width), ";"),
    class = divClass,
    htmltools::tags$label(class = "control-label", `for` = inputId, class = if (is.null(label)) "shiny-label-null", label),
    options
  )

  shinyWidgets:::attachShinyWidgetsDep(radioTag, "pretty")
}


#' Create tag list of field sets
#'
#' @param fieldSetName string - name of grouped set of radio buttons
#' @param fieldSetData tibble - tibble of values to create radio button
#' @param ... dots to accomodate additiaon arguments passed when called from parent function
#' @importFrom shiny tagList
#' @importFrom dplyr filter
#' @importFrom dplyr select
createFieldSet <- function(fieldSetName, fieldSetData, ...) {

  radioChoices <- fieldSetData |>
    dplyr::filter(FieldSet == fieldSetName) |>
    dplyr::select(-FieldSet)

  return(
    shiny::tagList(
      htmltools::tags$div(
        htmltools::tags$fieldset(
          htmltools::tags$b(shiny::HTML(fieldSetName)),
          getRadioButton(radioChoices, ...)
        )
      )
    )
  )
}

#' get a pretty radio button with HTML tooltip choice names, values
#'
#' @param radioChoices tibble - containing required args to create HTML version of choice names list 
#' @param ... dots to accomodate additional arguments passed when called from parent function
#' @import  shinyWidgets
getRadioButton <- function(radioChoices, ...) {

  args <- list(...)

  return(
    shinyWidgets:::generatePretty(
      inputId = args$inputId,
      selected = args$selected,
      inline = args$inline,
      type = "radio",
      choiceNames = purrr::pmap(radioChoices, createTooltip),
      choiceValues = radioChoices$Values,
      status = args$status,
      shape = args$shape,
      outline = args$outline,
      fill = args$fill,
      thick = args$thick,
      animation = args$animation,
      icon = args$icon,
      plain = args$plain,
      bigger = args$bigger
    )
  )
}
