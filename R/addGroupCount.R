#' Adds HTML formatted count to group variable
#'
#' @param .data dataframe with group variable
#' @param group grouping variable to add count
#' @param addLineBreak logical - whether to insert a line break between group label and count. Defaults to TRUE
#' @return datframe with html formatted group count
#' @importFrom dplyr add_count
#' @importFrom dplyr mutate
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang :=
#' @export
addGroupCount <- function(.data, group, addLineBreak = TRUE) {

  group <- rlang::enquo(group)
  lineBreak <- ifelse(addLineBreak,"\n","")

  return(
    .data |>
      dplyr::add_count(!!group)|>
      dplyr::mutate(
        `:=`(!!group, paste0("<b>",!!group,"</b>",lineBreak," (n=",n,")"))
      )
    )
}
