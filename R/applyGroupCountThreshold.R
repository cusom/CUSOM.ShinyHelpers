#' Utility function to check for minimum group membership in a dataframe
#'
#' @param .data dataframe containing group column to check
#' @param ... columns, string - any combinaton of grouping variables to check threshold against
#' @param threshold integer - minimum group membership threshold to be met in order to return data. default = 10
#
#' @return returns original dataframe if threshold is met for each group member. If threshold not met, NULL is returned
#' @importFrom rlang enquos
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @export
applyGroupCountThreshold <- function(.data, ...,  threshold = 10) {

  .groupVars <- rlang::enquos(...)

  t <- .data |>
    dplyr::group_by(!!!.groupVars) |>
    dplyr::summarize(n = n()) |>
    dplyr::mutate(f = dplyr::case_when(n < threshold ~ 1 , n >= threshold ~ 0 )) |>
    dplyr::summarise(fs = sum(f)) |>
    dplyr::select(fs) |>
    dplyr::summarise(fs = sum(fs)) |>
    dplyr::mutate(t = dplyr::case_when(sum(fs) == 0 ~ TRUE, sum(fs) > 0 ~ FALSE)) |>
    dplyr::pull(t)

  if(t){
    return(
      .data
    )
  }
  else {
    return(
      NULL
    )
  }
}
