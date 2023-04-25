#' formats significance Variable value value based on
#' threshold and adjustment method
#' @param significanceVariable - numeric - significanceVariable
#' @param adjustmentMethod string - multiple hypothesis correction method if any
#' defaults to "none"
#' @param formatInsignificantValues logical - whether to format
#' and return values
#' that are "insignificant" based on threshold
#' @return string - formatted p or q value depending on adjustment method.
#' @export
formatPValue <- function(
  significanceVariable,
  adjustmentMethod = "none",
  formatInsignificantValues = FALSE
  ) {
  if (!is.na(significanceVariable)) {
    adjustedInd <- adjustmentMethod != "none"
    threshold <- ifelse(adjustedInd, 0.1, 0.05)
    if (significanceVariable <= threshold || formatInsignificantValues) {
      formattedValue <- formatC(significanceVariable, format = "e", digits = 2)
      prefix <- ifelse(adjustedInd, "q", "p")
      return(
        glue::glue("{prefix}-value = {formattedValue}")
      )
    } else {
        return("No significant difference")
    }
  } else {    
    return("Unable to compute using chosen methods")
  }
}
