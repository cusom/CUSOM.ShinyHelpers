#' Creates Shiny Dashboard Menu Item / Tab
#'
#' @param text string -
#' @param tabName string -
#' @param iconName string -
#' @return shiny dashboard menu item
#' @importFrom shinydashboard menuItem
#' @importFrom shiny icon
#' @export
createMenuItem <- function(text, tabName, iconName) {
  shinydashboard::menuItem(
    text = text,
    tabName = tabName,
    icon = shiny::icon(iconName, verify_fa = FALSE),
    newtab = FALSE)
}