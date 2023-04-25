#' Utility function to create application link buttons
#'
#' @param linkData A dataframe containing the following required columns
#' label - string - label to be used for link button
#' imageURL - string - background image to use for link button
#' link - string - link for onclick event linked to button
#' IsCurrentApplication - boolean - indicates whether the
#' row is for the current application
#' @return shiny div containing shiny action button objects
#' @importFrom shiny actionButton
#' @export
createApplicationLinks <- function(linkData) {

  requiredColumns <- c("label", "imageURL", "link", "IsCurrentApplication")

  if(length(requiredColumns) > length(colnames(linkData))) {

    missingArguments <- paste0(
      intersect(requiredColumns, colnames(linkData)),
      collapse = ", "
      )

    msg <- paste0(
      "Missing the following required columns: ",
      missingArguments, " "
      )

    stop(msg)

  }

  linkItems <- vector("list", nrow(linkData))

  for(i in 1:length(linkItems)) {
    linkItems[[i]] <- list(
      inputId = linkData$label[[i]],
      label = linkData$label[[i]],
      imageURL = linkData$imageURL [[i]],
      onclick = linkData$link[[i]],
      IsCurrentApplication = linkData$IsCurrentApplication[[i]]
    )
  }

  return(
    lapply(linkItems, getActionButtonLink)
  )

}


getActionButtonLink <- function(x) {

  if (x$IsCurrentApplication[1] == 1) {
    return(
      shiny::actionButton(
        inputId = x$label,
        label =  x$label,
        class = "header-button-active",
        style = paste0("background-image: url('", x$imageURL, "');"),
      )
    )
  } else {
    return(
      shiny::actionButton(
        inputId = x$label,
        label =  x$label,
        class = "header-button",
        style = paste0("background-image: url('", x$imageURL, "');"),
        onclick = paste0("window.open('", x$onclick, "', '_blank')")
      )
    )
  }
}
