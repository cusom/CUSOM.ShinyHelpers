#' Plotly function to generate standard volcano plot from fold change data
#'
#' @param .data dataframe containing fold change data
#' @param .xvar fold change variable
#' @param .yvar p value variable
#' @param .significanceGroup significance group column
#' @param .text column containing text values to show in tooltip
#' @param .key - key column in fold change data - will be used for capuring select events
#' @param plotName - name to attribute to plot (used for tracking clicks, events, etc)
#'
#' @return returns list of lists of plotly annotation objects
#'          -- up regulated arror at top of plot (with up regulated text)
#'          -- down regulated arrow at the top of plot (
#'          -- dp value threshold text with "p(a) > threshold" text and up arrow
#'          -- if a value is chosen, adds in arrow annotation.
#' @export
getVolcanoPlot <- function(.data, .xvar, .yvar, .significanceGroup, .text, .key, plotName) {

  .xvar <- enquo(.xvar)
  .yvar <- enquo(.yvar)
  .significanceGroup <- enquo(.significanceGroup)
  .text <- enquo(.text)
  .key <- enquo(.key)

  maxFoldChange <- max(abs(.data[quo_name(.xvar)])) *1.1
  maxPValue <- max(abs(.data[quo_name(.yvar)])) * 1.1

  if(maxPValue < 5) { maxPValue <- 5.0}

  f <- list(
    family = "Arial",
    color = "rgb(58, 62, 65)",
    size = 12
  )

  x <- list(
    title = list(

      font = list (
        family = "Arial",
        size = 18
      )
    ),
    font = list (
      family = "Arial",
      color = "rgb(58, 62, 65)",
      size = 18
    ),
    showgrid = FALSE,
    zeroline = TRUE,
    showline = FALSE,
    showticklabels = TRUE,
    range = c(-maxFoldChange,maxFoldChange)
  )

  y <- list(
    title = list (

      font = list (
        family = "Arial",
        size = 18
      )
    ),
    font = list (
      family = "Arial",
      color = "rgb(58, 62, 65)",
      size = 18
    ),
    showgrid = FALSE,
    zeroline = TRUE,
    showline = TRUE,
    showticklabels = TRUE,
    range = c(0,maxPValue)
  )

  margin <- list(autoexpand = TRUE,
                 l = 10,
                 r = 30,
                 t = 30)

  p <- .data %>%
    plot_ly(
      data = ,
      type ='scatter',
      x = .xvar ,
      y = .yvar,
      text = .text,
      hoverinfo = 'text',
      mode = "markers",
      color = .significanceGroup,
      colors = c('#686868','#3E99CD','#1D4D7C'),
      key = .key,
      marker = list(size = 8,width = 2)
    ) %>%
    layout(
      title = list(
        text = '',
        font = list(
          family="Arial",
          size=24,
          color="#004F80"
        )
      ),
      showlegend=FALSE,
      legend = list(x = 100, y = 0.1),
      xaxis = x,
      yaxis = y,
      margin = margin,
      font = list(
        family="Arial",
        size= 18,
        color= "rgb(58, 62, 65)"
      ),
      shapes = list(
        list(
          type = "line",
          y0 = -log10(pValueThreshold),
          y1 = -log10(pValueThreshold),
          x0 = -maxFoldChange,
          x1 = maxFoldChange,
          line = list(color = "black", dash="dash")
        )
      )
    )

  p$x$source <- paste0(plotName,"VolcanoPlot")

  p

}
