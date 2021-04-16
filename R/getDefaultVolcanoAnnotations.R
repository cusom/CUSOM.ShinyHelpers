#' volcano plot Plotly function to get standard volcano annotations
#'
#' @param maxFoldChange - numeric - indicates the max value along the x axis for the plot (fold change) - helps anchor the annotations along the x-axis
#' @param pValueThreshold - numeric - p value cutoff threshold - helps anchor the dotted line for splits between significant groups
#' @param upRegulatedText - text to use for "up regulated" groups -- down regulated text will simply replace "up" with "down"
#' @param pValueAdjustedInd - logical - indicates if the p.value has been adjusted
#' @return list of lists of plotly annotation objects
#'          -- up regulated arror at top of plot (with up regulated text)
#'          -- down regulated arrow at the top of plot
#'          -- p value threshold with "p or q < threshold" text and up arrow
#'
#'
#' @export

getDefaultVolcanoAnnotations <- function(maxFoldChange,upRegulatedText,pValueThreshold,pValueThresholdLabel,pValueAdjustedInd = FALSE) {

  # text length is not 1:1 with "paper" units -- add 25% to length to properly center annotation
  upTextLength <- nchar(upRegulatedText) * 1.25
  upAnchor <- 0.75 + (upTextLength/100/2)

  downRegulatedText <- stringr::str_replace(upRegulatedText,"Up", "Down")
  downTextLength <- nchar(downRegulatedText) * 1.25
  downAnchor <-  0.25 - (downTextLength/100/2)

  pValueThresholdAnnotation <- ifelse(pValueAdjustedInd, "q","p")

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

      # p < threshold text
      list (
        x = 0.025,
        y = pValueThreshold * 1.1 ,
        text = paste0("&#9650; <b>",pValueThresholdAnnotation," < ",pValueThresholdLabel,"</b>"),
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
