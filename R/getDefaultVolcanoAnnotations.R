#' volcano plot Plotly function to get standard volcano annotations
#'
#' @param maxFoldChange - numeric - indicates the max value along the x axis for the plot (fold change) - helps anchor the annotations along the x-axis
#' @param upRegulatedText - text to use for "up regulated" groups -- down regulated text will simply replace "up" with "down"
#' @param significanceThreshold - numeric - significance threshold
#' @param adjustedInd - logical - indicates if the significance variable has been adjusted with a multiple hypothesis correction
#' @param includeThresholdLabel - logical - whether to include arrow with significance threshold
#' @return list of lists of plotly annotation objects
#'          -- up regulated arror at top of plot (with up regulated text)
#'          -- down regulated arrow at the top of plot
#'          -- p value threshold with "p or q < threshold" text and up arrow
#'
#'
#' @export

getDefaultVolcanoAnnotations <- function (maxFoldChange, upRegulatedText, significanceThreshold,  adjustedInd = FALSE, includeThresholdLabel = TRUE) {

  upTextLength <- nchar(upRegulatedText) * 1.25

  upAnchor <- 0.75 + (upTextLength/100/2)

  downRegulatedText <- case_when(
    grepl('Up',upRegulatedText) ~ stringr::str_replace(upRegulatedText,"Up","Down"),
    grepl('Increasing',upRegulatedText) ~ stringr::str_replace(upRegulatedText,"Increasing", "Decreasing"),
    grepl('Greater',upRegulatedText) ~ stringr::str_replace(upRegulatedText,"Greater","Decreased"),
    TRUE ~ upRegulatedText
  )

  downTextLength <- nchar(downRegulatedText) * 1.25

  downAnchor <- 0.25 - (downTextLength/100/2)

  significanceThresholdAnnotation <- ifelse(adjustedInd, "q", "p")

  return(
    list(
      list(
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
      list(
        x = upAnchor,
        y = 1.06,
        text = upRegulatedText,
        showarrow = F,
        xref = "paper",
        yref = "paper",
        font = list(
          family = "Arial",
          size = 12
          )
      ),
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
      list(
        x = downAnchor,
        y = 1.06,
        text = downRegulatedText,
        showarrow = F,
        xref = "paper",
        yref = "paper",
        font = list(
          family = "Arial",
          size = 12
        )
      ),
      list(
        x = 0.025,
        y = 0.015,
        text = ifelse(includeThresholdLabel,glue("&#9650; <b>{significanceThresholdAnnotation} < {significanceThreshold}</b>"),""),
        xref = "paper",
        yref = "paper",
        axref = "x",
        ayref = "y",
        showarrow = FALSE,
        ax = 0,
        ay = 0,
        font = list(
          family = "Arial",
          color = "rgb(58, 62, 65)",
          size = 14
        )
      )
    )
  )
}

