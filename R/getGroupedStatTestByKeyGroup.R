#' function to run stat tests for multiple groups
#'
#' @param .data dataframe
#' @param groupVar string - column of group values
#' @param ... additional arguments to be passed to
#' getStatTestByKeyGroup function
#' @return dataframe containing resulting p.value for each group calculated
#' @importFrom rlang enquo
#' @import dplyr
#' @importFrom rlang :=
#' @export


getGroupedStatTestByKeyGroup <- function(
  .data,
  groupVar,
  ...
) {

  groupVar <- rlang::enquo(groupVar)

  groups <- .data |>
    dplyr::select(!!groupVar) |>
    dplyr::distinct() |>
    dplyr::pull()

  stats_data <- tibble::tibble()

  for (group in groups) {

    stats_data <- stats_data |>
      dplyr::bind_rows(
        .data |>
          dplyr::filter(!!groupVar == group) |>
          CUSOMShinyHelpers::getStatTestByKeyGroup(...) |>
          dplyr::mutate(`:=`(!!groupVar, group)) |>
          dplyr::ungroup() |>
          dplyr::select(!!groupVar, p.value)
      )

  }

  return(stats_data)

}
