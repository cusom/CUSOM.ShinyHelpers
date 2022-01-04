#' gets all possible volcano plot annotations in a single call. Combines list output from default and arrow annotations.
#'
#' @param .data dataframe containing fold change data along with selected point indicator "selected_"
#' @param foldChangeVar fold change variable
#' @param significanceVariable significance value variable
#' @param selected selected indicator column
#' @param arrowLabelTextVar name of column in dataframe to pull for arrow annotation text
#' @param ... - additional named arguments to include in calls to other functions.
#' @return returns list of lists of annotation objects
#'          -- annotations - list of default volcano annotations
#'          -- shapes - list of shapes - defaults to dotted line separating significance groups
#'          -- arrow - list of arrow annotations to highlight points on volcano plot
#'          -- parameters - metatdata used to generate annotation lists
#' @export
getVolcanoAnnotations <- function(.data, foldChangeVar, significanceVariable, selected, arrowLabelTextVar,...) {

  foldChangeVar <- enquo(foldChangeVar)
  significanceVariable <- enquo(significanceVariable)
  selected <- enquo(selected)
  arrowLabelTextVar <- enquo(arrowLabelTextVar)

  maxFoldChange <- getMaxAbsValue(.data,!!foldChangeVar,inf.rm = TRUE, buffer=1.1)
  includeArrow <- dim(.data %>% filter(!!selected == 1, !is.na(!!foldChangeVar), !is.na(!!significanceVariable)) )[1] > 0

  adjustmentMethodVar <- colnames(.data)[which(grepl('adjust',colnames(.data)))][1]
  tranformationVar <- colnames(.data)[which(grepl('log',colnames(.data)))][1]
  adjustmentMethod <- .data %>% ungroup() %>% select(!!adjustmentMethodVar) %>% unique() %>% pull()
  adjustedInd <- adjustmentMethod != "none"
  significanceThreshold <- ifelse(adjustedInd,0.1,0.05)
  significanceThresholdTransformed <- ifelse(!is.na(tranformationVar),-log10(significanceThreshold),significanceThreshold)

  parameters <- list(
    "maxFoldChange" = maxFoldChange,
    "includeArrow" = includeArrow,
    "adjustmentMethod" = adjustmentMethod,
    "adjustedInd" = adjustedInd,
    "significanceThreshold" = significanceThreshold,
    "significanceThresholdTransformed" = significanceThresholdTransformed
  )

  annotations <- getDefaultVolcanoAnnotations(
    maxFoldChange,
    significanceThreshold = significanceThreshold,
    adjustedInd = adjustedInd,
    ...
    )

  shapes <- getDefaultVolcanoLine(cutoffThreshold=significanceThresholdTransformed,color="black",lineType="dash")

  if(includeArrow) {

    xcoordinate <-    .data %>% ungroup() %>% filter(!!selected == 1) %>% select(!!foldChangeVar) %>% pull()
    ycoordinate <-    .data %>% ungroup() %>% filter(!!selected == 1) %>% select(!!significanceVariable) %>% pull()
    arrowLabelText <- .data %>% ungroup() %>% filter(!!selected == 1) %>% select(!!arrowLabelTextVar) %>% pull()

    arrow <- getVolcanoArrowAnnotation(xcoordinate,ycoordinate,arrowLabelText)

  }

  else {

    arrow <- list()

  }

  return(
    list(
      "annotations" = annotations,
      "shapes" = shapes,
      "arrow" = arrow,
      "parameters" = parameters
    )
  )

}


