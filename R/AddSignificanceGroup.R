#' Function to add 3 group significance group label to fold change dataframe
#'
#' @param .data fold change dataframe 
#' @param foldChangeVar name of fold change column
#' @param significanceVariable name of significance variable column (p-value, q-value, etc)
#' @param adjustedInd logical - indicating whether or not a multiple hypothesis correction was applied. When TRUE, "q" will is used. When FALSE "p" is used. 
#' @param significanceThreshold numeric - value indicating threshold at which an observation in the significanceVariable coulumn is considered statistically significant. 
#' @param originalSignificanceThreshold numeric - value indicating un-transformed threshold at which an observation in the significanceVariable coulumn is considered statistically significant. 
#' @return returns labels and colors for 3 signficance groups (up, down, not significant)
#' @importFrom rlang enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom tidyr replace_na
#' @export
addSignificanceGroup <- function(
  .data, 
  foldChangeVar,
  significanceVariable,
  adjustedInd,
  significanceThreshold,
  originalSignificanceThreshold
) {

  foldChangeVar <- rlang::enquo(foldChangeVar)
  significanceVariable <- rlang::enquo(significanceVariable)

  significanceLetter <- ifelse(adjustedInd,"q","p")

  .data |>
    dplyr::mutate(
      significanceGroup = dplyr::case_when(
        (!!significanceVariable > significanceThreshold & !!foldChangeVar < -0) ~ glue::glue("down ({significanceLetter} < {originalSignificanceThreshold})"), 
        (!!significanceVariable > significanceThreshold & !!foldChangeVar >= 0) ~ glue::glue("up ({significanceLetter} < {originalSignificanceThreshold})"), 
        TRUE ~ "not significant"), 
      color = dplyr::case_when(
        grepl("down",significanceGroup) ~ "#3E99CD", 
        grepl("up",significanceGroup) ~ "#1D4D7C", 
        grepl("not",significanceGroup) ~ "#686868")
      ) |>
    tidyr::replace_na(list(significanceGroup = "not significant"))

}
