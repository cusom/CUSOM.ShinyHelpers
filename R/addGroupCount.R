#' Adds HTML formatted count to group variable
#'
#' @param .data dataframe with group variable
#' @param .group grouping variable to add count
#' @return datframe with html formatted group count
#' @export
addGroupCount <- function (.data, .group) {

  .group <- enquo(.group)

  return(
    .data %>%
      add_count(!!.group) %>%
      mutate(!!.group := paste0('<b>',!!.group,"</b> \n","(n=",n,")"))
  )
}
