#' Adds HTML formatted count to group variable
#'
#' @param .data dataframe with group variable
#' @param .group grouping variable to add count
#' @param addLineBreak logical - whether to insert a line break between group label and count. Defaults to TRUE
#' @return datframe with html formatted group count
#' @export
addGroupCount <- function (.data, group, addLineBreak = TRUE) {

  group <- enquo(group)
  lineBreak <- ifelse(addLineBreak,"\n","")
  
  return(
    .data %>% 
      add_count(!!group) %>% 
      mutate(
        `:=`(!!group, paste0("<b>",!!group,"</b>",lineBreak," (n=",n,")"))
      )
    )
}
