#' Calculate fold change for each key value between groups
#'
#' @param .data A dataframe
#' @param .key A string or number - key value for dataframe. Statistics will be computed between groups for each key value
#' @param .group A string - column indicating group membership - should be binary.
#' @param .value A number - numerical value to use with statitical test.
#' @param baselineGroupLabel a string - indicating which statisical test to perform by name..
#' @param inf.rm logical indicating whether to remove INF fold changes (dividing by 0) - defaults to TRUE

#' @return dataframe indicating  fold change (raw and log2) between baseline and comparison for each group by key value

#' @export
calculateFoldChangeByKeyGroup <- function(.data, .key, .group, .value, baselineGroupLabel, inf.rm = TRUE) {

  .key <- enquo(.key)
  .group <- enquo(.group)
  .value <- enquo(.value)

  groupLabels <- .data %>%  pull(!!.group) %>% unique()
  comparisonGroupLabel <- groupLabels[which(groupLabels != baselineGroupLabel)]

  baselineGroupLabel <- sym(baselineGroupLabel)
  comparisonGroupLabel <- sym(comparisonGroupLabel)

  foldChangeData <- .data %>%
    select(!!.key,!!.group,!!.value) %>%
    pivot_wider(names_from = !!.group, values_from=!!.value, values_fill=NA) %>%
    mutate(FoldChange = !!comparisonGroupLabel / !!baselineGroupLabel) %>%
    mutate(log2Foldchange = log2(FoldChange))

  if(inf.rm) {

    foldChangeData <- foldChangeData %>% filter(FoldChange != Inf)

  }

  return(foldChangeData)
}
