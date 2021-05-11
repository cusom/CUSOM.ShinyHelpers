#' Wrapper function around grDevices::densCols
#' produces a vector containing colors which encode the local densities at each point in a scatterplot.
#'
#' @param x the x and y arguments provide the x and y coordinates of the points.
#' @param y the x and y arguments provide the x and y coordinates of the points.
#' @param transform whether or not to log2 transform the x,y values
#' @return vector containing colors which encode the local densities at each point in a scatterplot.

#' @export

getDensityColors <- function(x, y, transform = FALSE) {

  if(transform) {
    dataframe <- data.frame(log2(x), log2(y))
  }

  else{
    dataframe <- data.frame(x,y)
  }

  z <- grDevices::densCols(dataframe, colramp = grDevices::colorRampPalette(c("black", "white")))

  density <- grDevices::col2rgb(z)[1,] + 1L

  return(density)

}
