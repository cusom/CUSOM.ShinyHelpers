#' Utility function to fill in missing missing
#' conbinations between sex and another group variable
#'
#' @param .data dataframe containing group column to check
#' @param .group column, string - group labels to check
#' @param groupLabels vector - labels to check for membership
#' useful when the source dataframe is missing a
#' required group ex(Group A vs Group B)
#' @return returns completed dataframe with 0 filled in
#' for missing combinations.
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom dplyr full_join
#' @importFrom dplyr full_join
#' @export

fillMissingSexObservations <- function(.data, .group, groupLabels) {

  .group <- rlang::enquo(.group)
  sexes <- c("Male", "Female")

  cartesian <- expand.grid(groupLabels,sexes)
  colnames(cartesian) <- c(rlang::quo_name(.group), "Sex")

  return(
    cartesian |>
      dplyr::full_join(.data, by = c(rlang::quo_name(.group), "Sex")) |>
      replace(is.na(.), 0.00)
  )

}
