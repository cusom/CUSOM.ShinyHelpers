#' Creates Shiny Dashboard Menu Item / Tab
#'
#' @param text string -
#' @param tabName string -
#' @param iconName string -
#' @return shiny dashboard menu item

#' @export
createMenuItem <- function(text,tabName,iconName) {
  menuItem(text=text,tabName=tabName,icon=icon(iconName),newtab = FALSE)
}




