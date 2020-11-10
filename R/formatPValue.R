#' formats p value based on threshold
#'
#' @param p.value - numeric - p value
#' @param p.value.adjustment.method string - p value adjustment method if any - defaults to "none"
#' @param p.value.threshold - numeric - threshold under which to display formatted p value - defaults to 0.05. For values above threshold, returns "No Significant Difference"
#' @return string - formatted p value.
#' @export
formatPValue <- function(p.value,p.value.adjustment.method="none",p.value.threshold = 0.05){

  if(!is.na(p.value)) {

    if(p.value <= p.value.threshold){

      p.value <- formatC(p.value, format = "e", digits = 2)

      p.value.suffix <- ifelse(p.value.adjustment.method=="none",""," (adj)")

      return(paste0("*p-value",p.value.suffix," = ",p.value,""))

    }

    else {

      return(paste0('No significant difference'))

    }

  }

  else {

    return(paste0("Unable to compute p-value using chosen methods"))

  }

}
