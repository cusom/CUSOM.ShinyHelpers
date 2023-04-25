#' gets all possible volcano plot annotations in a single call.
#' Combines list output from default and arrow annotations.
#'
#' @param .data dataframe containing fold change data along
#' with selected point indicator "selected_"
#' @param foldChangeVar fold change variable
#' @param significanceVariable significance value variable
#' @param selected selected indicator column
#' @param arrowLabelTextVar name of column in dataframe to
#' pull for arrow annotation text
#' @param ... - additional named arguments to include in calls to
#' other functions.
#' @return returns list of lists of annotation objects
#' -- annotations - list of default volcano annotations
#' -- shapes - list of shapes - defaults to dotted line separating
#' significance groups
#' -- arrow - list of arrow annotations to highlight points on volcano plot
#' -- parameters - metatdata used to generate annotation lists
#' @importFrom rlang enquo
#' @import dplyr
#' @export
getVolcanoAnnotations <- function(
  .data,
  foldChangeVar,
  significanceVariable,
  selected,
  arrowLabelTextVar,
  ...
) {

  foldChangeVar <- rlang::enquo(foldChangeVar)
  foldChangeVar <- rlang::enquo(foldChangeVar)
  significanceVariable <- rlang::enquo(significanceVariable)
  selected <- rlang::enquo(selected)
  arrowLabelTextVar <- rlang::enquo(arrowLabelTextVar)

  maxFoldChange <- getMaxAbsValue(
    .data,
    !!foldChangeVar,
    inf.rm = TRUE,
    buffer = 1.1
  )

  includeArrow <- dim(
    .data |>
      dplyr::filter(
        !!selected == 1,
        !is.na(!!foldChangeVar),
        !is.na(!!significanceVariable)
      )
    )[1] > 0

  adjustmentMethodVar <- colnames(.data)[which(grepl('adjust', colnames(.data)))][1]

  tranformationVar <- colnames(.data)[which(grepl('log', colnames(.data)))][1]

  adjustmentMethod <- .data |>
    dplyr::ungroup() |>
    dplyr::select(!!adjustmentMethodVar) |>
    dplyr::distinct() |>
    dplyr::pull()

  adjustedInd <- adjustmentMethod != "none"

  significanceThreshold <- ifelse(adjustedInd, 0.1, 0.05)

  significanceThresholdTransformed <- ifelse(
    !is.na(tranformationVar),
    -log10(significanceThreshold),
    significanceThreshold
  )

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

  shapes <- getDefaultVolcanoLine(
    cutoffThreshold = significanceThresholdTransformed,
    color = "black",
    lineType = "dash"
  )

  if (includeArrow) {

    xcoordinate <- .data |>
      dplyr::ungroup() |>
      dplyr::filter(!!selected == 1) |>
      dplyr::select(!!foldChangeVar) |>
      dplyr::pull()

    ycoordinate <- .data |>
      dplyr::ungroup() |>
      dplyr::filter(!!selected == 1) |>
      dplyr::select(!!significanceVariable) |>
      dplyr::pull()

    arrowLabelText <- .data |>
      dplyr::ungroup() |>
      dplyr::filter(!!selected == 1) |>
      dplyr::select(!!arrowLabelTextVar) |>
      dplyr::pull()

    arrow <- getVolcanoArrowAnnotation(
      xcoordinate,
      ycoordinate,
      arrowLabelText
    )

  } else {

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