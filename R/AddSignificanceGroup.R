#' Function to add 3 group significance group label to fold change dataframe
#'
#' @param .data fold change dataframe (x = fold change + y = p value )
#' @param foldChangeVar name of fold change column
#' @param pValueVar name of p value column
#' @param threshold p value threshold defaults to 0.05
#' @return returns labels for signficance groups (3 - up, down, not significant)
#' @export
addSignificanceGroup <- function(.data, foldChangeVar, pValueVar, threshold) {

  foldChangeVar <- enquo(foldChangeVar)
  pValueVar <- enquo(pValueVar)

  .data %>% 
    mutate(
      significanceGroup = case_when(
          (!!pValueVar > threshold & !!foldChangeVar < -0) ~ "Significant (down)", 
          (!!pValueVar > threshold & !!foldChangeVar >= 0) ~ "Significant (up)", 
          TRUE ~ "Not Significant"
      ), 
      color = case_when(
          significanceGroup == "Significant (down)" ~ "#3E99CD", 
          significanceGroup == "Significant (up)" ~ "#1D4D7C",
          significanceGroup == "Not Significant" ~ "#686868"
      )
    ) %>% 
    replace_na(list(significanceGroup = "Not Significant"))

}
