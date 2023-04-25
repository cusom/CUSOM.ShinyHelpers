#' Calculate fold change for each key value between groups
#'
#' @param .data A dataframe
#' @param .key A string or number - key value for dataframe. Statistics will be computed between groups for each key value
#' @param .group A string - column indicating group membership - should be binary.
#' @param .response A number - numerical value to use with statitical test. Should be log2 transformed values.
#' @param baselineGroupLabel a string - indicating which statisical test to perform by name..
#' @param inf.rm logical indicating whether to remove INF fold changes (dividing by 0) - defaults to TRUE
#' @param ... dots to accomodate additiaon arguments passed when called from parent function
#' @importFrom rlang enquo
#' @importFrom rlang sym
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate
#' @return dataframe indicating  fold change (raw and log2) between baseline and comparison for each group by key value

#' @export
calculateFoldChangeByKeyGroup <- function(
  .data,
  .key,
  .group,
  .response,
  baselineGroupLabel,
  inf.rm = TRUE,
  ...
) {

  .key <- rlang::enquo(.key)
  .group <- rlang::enquo(.group)
  .response <- rlang::enquo(.response)

  groupLabels <- .data |> dplyr::ungroup() |> dplyr::select(!!.group) |> unique()
  comparisonGroupLabel <- groupLabels |> dplyr::filter(!!.group != baselineGroupLabel) |> dplyr::pull() |> as.character()
  baselineGroupLabel <- rlang::sym(baselineGroupLabel)
  comparisonGroupLabel <- rlang::sym(comparisonGroupLabel)

  foldChangeData <- .data |>
    dplyr::select(!!.key, !!.group, !!.response) |>
    tidyr::pivot_wider(names_from = !!.group, values_from = !!.response, values_fill = NA) |>
    dplyr::mutate(log2FoldChange = !!comparisonGroupLabel - !!baselineGroupLabel) |>
    dplyr::mutate(FoldChange = 2^log2FoldChange)

  if (inf.rm) {
      foldChangeData <- foldChangeData |> dplyr::filter(FoldChange != Inf)
  }

  return(foldChangeData)
}
