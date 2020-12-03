#' volcano plot Plotly function to get standard volcano line dividing significance cutoff
#'
#' @param cutoffThreshold - numeric - y coordinate for line
#' @param color - string - color to use for line - defaults to "black"
#' @param lineType - string - line style to use - defaults to "dash"
#' @return list of lists with line shape definition
#'
#'
#' @export
getDefaultVolcanoLine <- function(cutoffThreshold, color="black", lineType="dash") {

  return(
    list(
      list(
        type = "line",
        xref = "paper",
        yref = "y",
        axref = "paper",
        ayref = "y",
        y0 = cutoffThreshold,
        y1 = cutoffThreshold,
        x0 = 0,
        x1 = 1,
        line = list(
          color = color,
          dash = lineType
        )
      )
    )
  )

}
