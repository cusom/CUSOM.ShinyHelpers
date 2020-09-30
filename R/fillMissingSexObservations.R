#' Utility function to fill in missing missing conbinations between sex and another group variable
#'
#' @param .data dataframe containing group column to check
#' @param .groupVar column, string - group labels to check
#' @param groupLabels vector - labels to check for membership - useful when the source dataframe is missing a required group ex(Group A vs Group B)
#
#' @return returns completed dataframe with 0 filled in for missing combinations.
#' @export

fillMissingSexObservations <- function(.data, .group, groupLabels) {

  .group <- enquo(.group)
  sexes <- c("Male","Female")

  cartesian <- expand.grid(.groupLabels,sexes)
  colnames(cartesian) <- c(quo_name(.group),"Sex")

  return(
    cartesian %>%
      full_join(.data,by = c(quo_name(.group),"Sex")) %>%
      replace(is.na(.), 0.00)
  )

}
