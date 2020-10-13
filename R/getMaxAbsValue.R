#' gets maximum abs value from dataframe column
#'
#' @param .data dataframe
#' @param .var numeric column name to calcualte max abs value
#' @param inf.rm logical indicating whether to remove INF values
#' @param buffer numeric value to add buffer or padding to max abs value

#' @return maximum abs value for column within dataframe
#' @export
getMaxAbsValue <- function(.data, .var, inf.rm = TRUE, buffer=1.1) {

  dataframe  <- .data %>%
    ungroup() %>%
    select(!!.var)

  if(inf.rm) {

    dataframe  <- dataframe %>%
    filter(!!.var != Inf)

  }

  maxAbsValue <- dataframe %>%
    summarise(max = max(abs(!!.xvar)) * buffer) %>%
    pull()

  return(maxAbsValue)

}
