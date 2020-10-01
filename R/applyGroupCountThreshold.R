#' Utility function to check for minimum group membership in a dataframe
#'
#' @param .data dataframe containing group column to check
#' @param ... columns, string - any combinaton of grouping variables to check threshold against
#' @param threshold integer - minimum group membership threshold to be met in order to return data. default = 10
#
#' @return returns original dataframe if threshold is met for each group member. If threshold not met, NULL is returned
#' @export
applyGroupCountThreshold <- function(.data, ...,  threshold = 10) {

  .groupVars <- enquos(...)

  t <- .data %>%
    group_by(!!!.groupVars) %>%
    summarize(n = n()) %>%
    mutate(f = case_when(n < threshold ~ 1 , n >= threshold ~ 0 )) %>%
    summarise(fs = sum(f)) %>%
    select(fs) %>%
    summarise(fs = sum(fs)) %>%
    mutate(t = case_when(sum(fs) == 0 ~ TRUE, sum(fs) > 0 ~ FALSE)) %>%
    pull(t)

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
