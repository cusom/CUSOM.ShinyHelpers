#' Utility function to check for minimum group membership in a dataframe
#'
#' @param .data dataframe containing group column to check
#' @param .groupVar column, string - group labels to check
#' @param groupLabels vector - labels to check for membership - useful when the source dataframe is missing a required group ex(Sex- Male, Female)
#' @param threshold integer - minimum group membership threshold to be met in order to return data
#
#' @return returns original dataframe if threshold is met for each group member. If threshold not met, NULL is returned
#' @export
applyGroupCountThreshold <- function(.data, .groupVar, groupLabels, threshold = 10 ) {

  .groupVar <- enquo(.groupVar)

  cartesian <- expand.grid(.groupLabels)
  colnames(cartesian) <- c(quo_name(.groupVar))

  t <- .data %>%
    group_by(!!.groupVar) %>%
    summarize(n = n()) %>%
    mutate(f = case_when(n < threshold ~ 1 , n >= threshold ~ 0 )) %>%
    full_join(cartesian,by = c(quo_name(.groupVar))) %>%
    replace(is.na(.), 1) %>%
    summarise(fs = sum(f)) %>%
    mutate(t = case_when(fs == 0 ~ TRUE, fs > 0 ~ FALSE)) %>%
    pull()

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
