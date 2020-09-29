#' Calculate standard summaries across groups for a dataframe
#'
#' @param .data A dataframe
#' @param .summaryVar A number to summarize
#' @param ... groups to summarize within dataframe - can be mutliples.
#' @return dataframe containing with summaries for each group
#         includes median, mean, and n
#' @export
SummarizeByGroup <- function(.data, .summaryVar, ...) {

  .summaryVar <- enquo(.summaryVar)
  .group_vars <- enquos(...)

  .data %>%
    group_by(!!!.group_vars) %>%
    summarise(
      median = median(!!.summaryVar),
      mean = mean(!!.summaryVar),
      n = n()
    )
}
