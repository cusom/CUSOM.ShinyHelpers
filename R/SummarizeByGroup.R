#' Calculate standard summaries across groups for a dataframe
#'
#' @param .data A dataframe
#' @param .summaryVar A number to summarize
#' @param ... group variables to summarize within dataframe - can be mutliples.
#' @param na.rm boolean - indicates whether NA values should be stripped 
#' from summary calculation. Defaults to TRUE
#' @return dataframe containing with summaries for each group
#         includes median, mean, and n
#' @importFrom rlang enquo
#' @import dplyr
#' @importFrom stats median
#' @export
summarizeByGroup <- function(
  .data, 
  .summaryVar,
  ...,
  na.rm = TRUE
) {

  .summaryVar <- rlang::enquo(.summaryVar)
  .group_vars <- rlang::enquos(...)

  .data |>
    dplyr::group_by(!!!.group_vars) |>
    dplyr::summarise(
      median = stats::median(!!.summaryVar, na.rm = na.rm),
      mean = mean(!!.summaryVar, na.rm = na.rm),
      n = dplyr::n()
    )
}
