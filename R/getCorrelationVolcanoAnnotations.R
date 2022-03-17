#' gets all possible correlation volcano plot annotations in a single call. Combines list output from default and arrow annotations.
#' similar to getVolcanoAnnotations, geared towards specific annotations for correlation volcano plots.
#'
#' @param .data dataframe containing fold change data along with selecte point indicator "selected_"
#' @param foldChangeVar fold change variable
#' @param significanceVariable statistical significance variable
#' @param selected selected indicator column
#' @param arrowLabelTextVar name of column in dataframe to pull for arrow annotation text
#' @param ... - additiaonal named arguments to include in calls to other functions.
#' @return returns list of lists of annotation objects
#'          -- annotations - list of default volcano annotations
#'          -- shapes - list of shapes - defaults to dotted line separating significance groups
#'          -- arrow - list of arrow annotations to highlight points on volcano plot
#'          -- parameters - metatdata used to generate annotation lists
#' @export

getCorrelationVolcanoAnnotations <- function (.data, foldChangeVar, significanceVariable, selected, arrowLabelTextVar, ...) {

  foldChangeVar <- enquo(foldChangeVar)
  significanceVariable <- enquo(significanceVariable)
  selected <- enquo(selected)
  arrowLabelTextVar <- enquo(arrowLabelTextVar)
  maxFoldChange <- getMaxAbsValue(.data, !!foldChangeVar, inf.rm = TRUE,buffer = 1.1)

  includeArrow <- dim(.data %>% filter(!!selected == 1, !is.na(!!foldChangeVar),!is.na(!!significanceVariable)))[1] > 0

  adjustmentMethodVar <- colnames(.data)[which(grepl("adjust",colnames(.data)))][1]
  tranformationVar <- colnames(.data)[which(grepl("log",colnames(.data)))][1]
  adjustmentMethod <- .data %>% ungroup() %>% select(!!adjustmentMethodVar) %>% unique() %>% pull()
  adjustedInd <- adjustmentMethod != "none"
  significanceThreshold <- ifelse(adjustedInd, 0.1, 0.05)
  significanceThresholdTransformed <- ifelse(!is.na(tranformationVar),-log10(significanceThreshold), significanceThreshold)

  parameters <- list(
    "maxFoldChange" = maxFoldChange,
    "includeArrow" = includeArrow,
    "adjustmentMethod" = adjustmentMethod,
    "adjustedInd" = adjustedInd,
    "significanceThreshold" = significanceThreshold,
    "significanceThresholdTransformed" = significanceThresholdTransformed
  )

  annotations <- getCorrelationVolcanoDefaultAnnotations(
    maxFoldChange,
    significanceThreshold = significanceThreshold,
    adjustedInd = adjustedInd,
    ...
  )

  shapes <- getDefaultVolcanoLine(cutoffThreshold = significanceThresholdTransformed, color = "black", lineType = "dash")

  if (includeArrow) {

    xcoordinate <- .data %>% ungroup() %>% filter(!!selected == 1) %>% select(!!foldChangeVar) %>% pull()
    ycoordinate <- .data %>% ungroup() %>% filter(!!selected == 1) %>% select(!!significanceVariable) %>% pull()
    arrowLabelText <- .data %>% ungroup() %>% filter(!!selected == 1) %>% select(!!arrowLabelTextVar) %>% pull()
    arrow <- getVolcanoArrowAnnotation(xcoordinate, ycoordinate,arrowLabelText)
  }
  else {
    arrow <- list()
  }
  return(list(annotations = annotations, shapes = shapes, arrow = arrow,  parameters = parameters))
}

getCorrelationVolcanoDefaultAnnotations <- function (maxFoldChange, titleText = "", significanceThreshold, adjustedInd = FALSE, includeThresholdLabel = TRUE) {

  upAnchor <- 0.75
  downAnchor <- 0.25
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
        y = 1.05,
        text = "Positive",
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
        y = 1.05,
        text = "Negative",
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
        y = significanceThreshold * 1.2,
        text = ifelse(includeThresholdLabel,glue("&#9650; <b>{significanceThresholdAnnotation} < {significanceThreshold}</b>"),""),
        xref = "paper",
        yref = "y",
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
      ),
      list(
        x = 0.5,
        y = 1.075,
        text = titleText,
        showarrow = F,
        xref = "paper",
        yref = "paper",
        font = list(
          family = "Arial",
          size = 12
          )
      )
    )
  )
}
