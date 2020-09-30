#' utility function to import shiny modules to server
#'
#' @param NameSpace shiny module namespace
#' @return shiny server module with objects
#' @export
createServerObject <- function(NameSpace) {

  ServerModuleFunctionName <- paste0(NameSpace,'Server')

  do.call(ServerModuleFunctionName,list(NameSpace))

}
