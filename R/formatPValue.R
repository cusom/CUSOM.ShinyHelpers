#' HTML formats p value based on threshold
#'
#' @param pvalue - numeric - p value
#' @param threshold - numeric - threshold under which to display formatted p value - defaults to 0.05. For values above threshold, returns "No Significant Difference"
#' @return dataframe with html formatted p value.
#' @export
formatPValue <- function(pvalue,threshold = 0.05){
  if(pvalue<= threshold){
    pVal <- formatC(pvalue, format = "e", digits = 2)
    return(paste0('<p> *p = ',pVal,'</p>'))
  } else {
    return(paste0('<p>No significant difference</p>'))
  }
}
