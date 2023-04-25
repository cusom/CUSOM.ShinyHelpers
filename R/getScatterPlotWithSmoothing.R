#' GGplot function to generate standard volcano plot from fold change data
#'
#' @param .data dataframe containing pairwise data (x vs y)
#' @param xVar x variable column
#' @param yVar y variable column
#' @param colorVar column containing colors to apply for each point
#' @param textVar column containing text values to show in tooltip
#' @param smoothingMethod - nammed argument to pass to ggplot
#' geom_smooth see ?ggplot2::geom_smooth()
#' @return returns ggplot scatter plot showing x, y points with
#' geom_smooth() layer added using nammed smoothing method
#' @importFrom rlang enquo
#' @export

getScatterPlotWithSmoothing <- function(
  .data, 
  xVar, 
  yVar, 
  colorVar, 
  textVar, 
  smoothingMethod = "lm"
) {

  match.arg(smoothingMethod)

  xVar <- rlang::enquo(xVar)
  yVar <- rlang::enquo(yVar)
  colorVar <- rlang::enquo(colorVar)
  textVar <- rlang::enquo(textVar)

  p <- .data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!xVar,
        y = !!yVar,
        color = !!colorVar
      )
    ) +
    ggplot2::geom_smooth(method = smoothingMethod) +
    ggplot2::geom_point(
      ggplot2::aes(text = !!textVar)) +
    ggplot2::scale_color_viridis_c() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "black"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )

  return(p)

}