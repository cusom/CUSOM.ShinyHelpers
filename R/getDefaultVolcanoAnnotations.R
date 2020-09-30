#' volcano plot Plotly function to get standard volcano annotations
#'
#' @param maxFoldChange - numeric - indicates the max value along the x axis for the plot (fold change) - helps anchor the annotations along the x-axis
#' @param pValueThreshold - numeric - p value cutoff threshold - helps anchor the dotted line for splits between significant groups
#' @param upRegulatedText - text to use for "up regulated" groups -- down regulated text will simply replace "up" with "down"
#' @return list of lists of plotly annotation objects
#'          -- up regulated arror at top of plot (with up regulated text)
#'          -- down regulated arrow at the top of plot (
#'          -- dotted line showing p value threshold with "p(a) > threshold" text and up arrow
#'
#'
#' @export

getDefaultVolcanoAnnotations <- function(maxFoldChange,pValueThreshold,upRegulatedText) {

  downRegulatedText <- stringr::str_replace(upRegulatedText,'Up','Down')
  n <- nchar(downRegulatedText)
  downAnchor <- round(n / 32.5,2)
  upAnchor <- 1 - downAnchor

  return(
    list(

      # top level up regulated arrow going from center to right
      list (
        x = maxFoldChange,
        y = 1,
        xref = "x",
        yref = "paper",
        axref = "x",
        ayref = "y",
        showarrow = T,
        arrowcolor = "#1D4D7C",
        ax = 0,
        ay = 0
      ),

      # up regulated text, anchor above arrow
      list (
        x = upAnchor,
        y = 1.04,
        text = upRegulatedText,
        showarrow = F,
        xref='paper',
        yref='paper',
        font = list (
          family = "Arial",
          size = 18
        )
      ),

      # top level down regualted arrow going from center to left
      list(
        x = -maxFoldChange,
        y = 1,
        xref = "x",
        yref = "paper",
        axref = "x",
        ayref = "y",
        showarrow = T,
        arrowcolor = "#3E99CD",
        ax = 0,
        ay = 0
      ),

      # down regulated text, anchor above down regulated arrow
      list (
        x = downAnchor,
        y = 1.04,
        text = downRegulatedText,
        showarrow = F,
        xref='paper',
        yref='paper',
        font = list (
          family = "Arial",
          size = 18
        )
      ) ,

      # p(a) < threshold text and dotted line
      list (
        x = 0.025,
        y = -log10(pValueThreshold) + 0.35,
        text = paste0("&#9650; <b>p(a)<",pValueThreshold,"</b>"),
        xref = "paper",
        yref = "y",
        axref = "x",
        ayref = "y",
        showarrow = FALSE,
        ax = 0,
        ay = 0,
        font = list (
          family = "Arial",
          color = "rgb(58, 62, 65)",
          size = 14
        )
      )
    )
  )
}
