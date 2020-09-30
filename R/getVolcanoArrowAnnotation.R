#' volcano plot Plotly function to add volcano plot arrow annotation to specfic point
#'
#' @param xCoordinate - numeric - indicates the x coordinate to use for the annotation
#' @param  yCoordinate - numeric -indicates the y coordinate to use for the annotation
#' @param  text - text to use for arrow annotation
#' @return plotly annotation object
#'
#'
#' @export
getVolcanoArrowAnnotation <- function(xCoordinate,yCoordinate,text) {

  return(
    list(
      list(
        x = xCoordinate,
        y = yCoordinate,
        text = text,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 1,
        startarrowhead = 1,
        arrowside = "end",
        arrowcolor = "#e74c3c",
        ax = 20,
        ay = -40,
        font = list(color = 'Black',
                    family = 'Arial',
                    size = 16),
        bgcolor = "#abb2b9",
        standoff = 4
      )
    )
  )
}
