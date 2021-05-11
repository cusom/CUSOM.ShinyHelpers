#' Plotly function to generate standard volcano plot from fold change data
#'
#' @param .data dataframe containing fold change data
#' @param xvar fold change variable
#' @param yvar p value variable
#' @param significanceGroup significance group column
#' @param text column containing text values to show in tooltip
#' @param key - key column in fold change data - will be used for capuring select events
#' @param plotName - name to attribute to plot (used for tracking clicks, events, etc)
#' @param colors - optional colors argument
#' @param shape - optional shape - should be passed as a nammed column in source data. 
#'
#' @return returns Plotly scatter plot showing fold change vs p value colored by significance group
#' @export
getVolcanoPlot <- function(.data, foldChangeVar, pValueVar, significanceGroup, text, key, plotName, colors = c("#686868","#3E99CD", "#1D4D7C"), shape = "X") {

  foldChangeVar <- enquo(foldChangeVar)
  pValueVar <- enquo(pValueVar)
  significanceGroup <- enquo(significanceGroup)
  text <- enquo(text)
  key <- enquo(key)
  shape <- enquo(shape)

  maxFoldChange <- getMaxAbsValue(.data,!!foldChangeVar,inf.rm = TRUE, buffer=1.1)
  maxPValue <- getMaxAbsValue(.data,!!pValueVar,inf.rm = TRUE, buffer=1.1)

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
      type ='scatter',
      x = foldChangeVar ,
      y = pValueVar,
      text = text,
      hoverinfo = 'text',
      mode = "markers",
      color = significanceGroup,
      colors = colors, 
      symbol = shape,
      key = key,
      marker = list(
        size = 8,
        width = 2
      )
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
      )
    )

  p$x$source <- paste0(plotName,"VolcanoPlot")

  p

}

