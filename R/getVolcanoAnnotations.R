#' gets all possible volcano plot annotations in a single call. Combines list output from default and arrow annotations.
#'
#' @param .data dataframe containing fold change data along with selecte point indicator "selected_"
#' @param foldChangeVar fold change variable
#' @param pValueVar p value variable
#' @param selected selected indicator column
#' @param arrowLabelTextVar name of column in dataframe to pull for arrow annotation text
#' @param ... - additiaonal named arguments to include in calls to other functions.
#' @return returns list of lists of annotation objects
#'          -- annotations - list of default volcano annotations
#'          -- shapes - list of shapes - defaults to dotted line separating significance groups
#'          -- arrow - list of arrow annotations to highlight points on volcano plot
#'          -- parameters - metatdata used to generate annotation lists
#' @export
getVolcanoAnnotations <- function(.data, foldChangeVar, pValueVar, selected, arrowLabelTextVar, ... ) {

  foldChangeVar <- enquo(foldChangeVar)
  pValueVar <- enquo(pValueVar)
  selected <- enquo(selected)
  arrowLabelTextVar <- enquo(arrowLabelTextVar)

  maxFoldChange <- getMaxAbsValue(.data,!!foldChangeVar,inf.rm = TRUE, buffer=1.1)
  includeArrow <- dim(.data %>% filter(!!selected == 1, !is.na(!!foldChangeVar), !is.na(!!pValueVar)) )[1] > 0

  adjustmentMethodVar <- colnames(.data)[which(grepl('adjust',colnames(.data)))][1]
  tranformationVar <- colnames(.data)[which(grepl('log',colnames(.data)))][1]
  adjustmentMethod <- .data %>% ungroup() %>% select(!!adjustmentMethodVar) %>% unique() %>% pull()
  pValueAdjustedInd <- adjustmentMethod != "none"
  pValueThreshold <- ifelse(pValueAdjustedInd,0.1,0.05)
  pValueThresholdTransformed <- ifelse(!is.na(tranformationVar),-log10(pValueThreshold),pValueThreshold)

  parameters <- list(
    "maxFoldChange" = maxFoldChange,
    "includeArrow" = includeArrow,
    "adjustmentMethod" = adjustmentMethod,
    "pValueAdjustedInd" = pValueAdjustedInd,
    "pValueThreshold" = pValueThreshold,
    "pValueThresholdTransformed" = pValueThresholdTransformed
  )

  annotations <- getDefaultVolcanoAnnotations(maxFoldChange,
                                                  pValueThreshold = pValueThresholdTransformed,
                                                  pValueThresholdLabel = pValueThreshold,
                                                  pValueAdjustedInd = pValueAdjustedInd,
                                                  ...)

  shapes <- getDefaultVolcanoLine(cutoffThreshold=pValueThresholdTransformed,color="black",lineType="dash")

  if(includeArrow) {

    xcoordinate <-    .data %>% ungroup() %>% filter(!!selected == 1) %>% select(!!foldChangeVar) %>% pull()
    ycoordinate <-    .data %>% ungroup() %>% filter(!!selected == 1) %>% select(!!pValueVar) %>% pull()
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


