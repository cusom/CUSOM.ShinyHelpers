#' gets maximum abs value from dataframe column
#'
#' @param .data dataframe
#' @param .var numeric column name to calcualte max abs value
#' @param inf.rm logical indicating whether to remove INF values
#' @param buffer numeric value to add buffer or padding to max abs value
#' @return maximum abs value for column within dataframe
#' @importFrom rlang enquo
#' @import dplyr
#' @export
getMaxAbsValue <- function(
  .data,
  .var,
  inf.rm = TRUE,
  buffer = 1.1
) {

  .var <- rlang::enquo(.var)

  dataframe  <- .data |>
    dplyr::ungroup() |>
    dplyr::select(!!.var)

  if(inf.rm) {

    dataframe  <- dataframe |>
      dplyr::filter(!!.var != Inf)

  }

  maxAbsValue <- dataframe |>
    dplyr::summarise(
      max = max(abs(!!.var)) * buffer
    ) |>
    dplyr::pull()

  return(maxAbsValue)

}
