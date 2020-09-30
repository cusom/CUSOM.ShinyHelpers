#' Utility function to create standard Shiny input widgets
#'
#' @param controlType - string - name of widget to use
#' @param inputId - string - shiny input id for the UI component
#' @param label - string
#' @param choices - string
#' @param  selected - string
#' @param ... dots - additional arguments to pass to widgets functions
#' @return returns a shiny input widget based on inputs
#' @export
createInputControl <- function(controlType, inputId, label, choices, selected, ...) {

  if(controlType=="radioButtons") {

    prettyRadioButtons(
      inputId = inputId,
      label = label,
      choiceNames = unique(choices),
      choiceValues = unique(choices),
      selected = selected,
      ...
      #icon = icon("check")
    )

  } else if (controlType=="pickerInput"){

    pickerInput(
      inputId = inputId,
      label = label,
      choices = choices,
      selected = selected,
      ...
      # options = list(
      #   `actions-box` = TRUE,
      #   `live-search` = TRUE,
      #   size = 10,
      #   `selected-text-format` = "count > 3",
      #   `max-options` =  choiceLimit
      # ),
      # multiple = multiple
    )

  } else if (controlType=="checkboxGroupInput"){

    #checkboxGroupInput(
    awesomeCheckboxGroup(
      inputId = inputId,
      label = label,
      choices = choices,
      selected = selected,
      ...
      #inline = TRUE
    )

  } else if (controlType=="sliderInput"){

    range <- range(choices,na.rm = FALSE)

    sliderInput(
      inputId = inputId,
      label = label,
      min = min(range),
      max = max(range),
      value =  range
    )
  }

  else if (controlType=="primarySwitch") {

    materialSwitch(
      inputId = inputId,
      label = label,
      ...
      # value = TRUE,
      # status = "primary"
    )

  }

}
