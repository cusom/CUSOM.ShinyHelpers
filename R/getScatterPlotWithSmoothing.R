#' GGplot function to generate standard volcano plot from fold change data
#'
#' @param .data dataframe containing pairwise data (x vs y)
#' @param xvar x variable column 
#' @param yvar y variable column 
#' @param colorVar column containing colors to apply for each point 
#' @param textVar column containing text values to show in tooltip
#' @param smoothingMethod - nammed argument to pass to ggplot geom_smooth see ?ggplot2::geom_smooth()
#' 
#' @return returns ggplot scatter plot showing x, y points with geom_smooth() layer added using nammed smoothing method
#' @export

getScatterPlotWithSmoothing <- function(.data, xVar, yVar, colorVar, textVar, smoothingMethod = "lm") {

  match.arg(smoothingMethod)

  xVar <- enquo(xVar)
  yVar <- enquo(yVar)
  colorVar <- enquo(colorVar)
  textVar <- enquo(textVar)
  
  p <- .data %>%
    ggplot(
      aes(
        x = !!xVar, 
        y = !!yVar, 
        color = !!colorVar
      )
    ) +
    geom_smooth(method = smoothingMethod) +
    geom_point(aes(text=!!textVar)) + 
    scale_color_viridis_c() +
    theme_bw() +
    theme(
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )

  return(p)

}