#' Utility function to create Shiny UI inputs within the sidebar panel for a sidebar name
#' Requires specfic module format where inputs are a nammed list (Inputs) within the module script.
#'
#' @param sidebarName - string - indicates which tab / sidebar to attribute inputs
#' @importFrom shiny conditionalPanel
#' @return returns a shiny conditional Panel with appropriate UI objects
#' @export
createSidebarInputs <- function(sidebarName) {

  UIModuleFunctionName <- paste0(sidebarName,'UI')

  UIObjects <- do.call(UIModuleFunctionName,list(sidebarName))

  return(
    shiny::conditionalPanel(
      condition = paste0("input.sidebar == '",sidebarName,"'"),
      UIObjects$Inputs
    )
  )

}
