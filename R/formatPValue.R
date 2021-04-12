#' formats p value based on threshold
#'
#' @param p.value - numeric - p value
#' @param p.value.adjustment.method string - p value adjustment method if any - defaults to "none"
#' @return string - formatted p or q value depending on adjustment method.
#' @export
formatPValue <- function(p.value,p.value.adjustment.method="none"){

  if (!is.na(p.value)) {

    pValueAdjustInd <- p.value.adjustment.method != "none"
    p.value.threshold <- ifelse(pValueAdjustInd,0.1,0.05)

    if (p.value <= p.value.threshold) {

      p.value <- formatC(p.value, format = "e", digits = 2)
      p.value.prefix <- ifelse(pValueAdjustInd,"q","p")
      return(paste0(p.value.prefix, "-value = ", p.value))
    }

    else {

      return(paste0("No significant difference"))

    }

  }

  else {

    return(paste0("Unable to compute p-value using chosen methods"))

  }

}
