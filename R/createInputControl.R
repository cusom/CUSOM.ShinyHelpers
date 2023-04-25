#' Utility function to create standard Shiny input widgets
#'
#' @param controlType - string - name of widget to use
#' @param inputId - string - shiny input id for the UI component
#' @param label - string
#' @param choices - string
#' @param  selected - string
#' @param ... dots - additional arguments to pass to widgets functions
#' @return returns a shiny input widget based on inputs
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets awesomeCheckboxGroup
#' @importFrom shiny sliderInput
#' @importFrom shinyWidgets materialSwitch
#' @export
createInputControl <- function(controlType, inputId, label, choices, selected, ...) {

  if(controlType=="radioButtons") {

    shinyWidgets::prettyRadioButtons(
      inputId = inputId,
      label = label,
      choiceNames = unique(choices),
      choiceValues = unique(choices),
      selected = selected,
      ...
    )

  } else if (controlType=="pickerInput"){

    shinyWidgets::pickerInput(
      inputId = inputId,
      label = label,
      choices = choices,
      selected = selected,
      ...      
    )

  } else if (controlType=="checkboxGroupInput"){

    shinyWidgets::awesomeCheckboxGroup(
      inputId = inputId,
      label = label,
      choices = choices,
      selected = selected,
      ...
    )

  } else if (controlType=="sliderInput"){

    range <- range(choices,na.rm = FALSE)

    shiny::sliderInput(
      inputId = inputId,
      label = label,
      min = min(range),
      max = max(range),
      value =  range
    )
  }

  else if (controlType=="primarySwitch") {

    shinyWidgets::materialSwitch(
      inputId = inputId,
      label = label,
      ...
    )

  }

}
