#' utility function to call modules by namespace and module name
#'
#' @param id shiny namespace
#' @param module  string - module name

#' @return shiny module
#' @export
moduleServer <- function(id, module) {
  callModule(module, id)
}
