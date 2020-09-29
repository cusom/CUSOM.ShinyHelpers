#'Utility function to create Tab Items
# Requires specfic module format where outputs are a nammed list (outputs) within the module script.
#' @param tabName string - indicates which tab / sidebar to attribute outputs
#' @return returns a shiny tabName object with appropriate UI ouputs

createTabOutput <- function(tabName) {

  UIModuleFunctionName <- paste0(tabName,'UI')

  UIObjects <- do.call(UIModuleFunctionName,list(tabName))$Outputs

  tab <- tabItem(
    tabName = tabName,
    UIObjects
  )

  return(tab)

}
