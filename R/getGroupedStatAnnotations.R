#' function to generate stat annotations across multiple groups
#'
#' @param AnnotationAnchorLines list of annotation references (outputted from getStatAnnotationAnchorLines function)
#' @param statTest - string - name of statistical test perfomed
#' @param covariates - list - list of features included in linear model as covariates
#' @param adjustmentMethod string - name of multiple hypothesis correction method applied to significance variable
#' @param ... dots - additional arguments to be passed to other functions (like formatPValue)
#' @return list statistic annotations to be overlayed for each group
#' @export


getGroupedStatAnnotations <- function (AnnotationAnchorLines, statTest, covariates, adjustmentMethod, ...){

  annotations <- list()

  annotation <- list(
    type = "line",
    x = 0,
    y = 0,
    line = list(
      color = "blue"
    ),
    font = list(
      family = "Arial",
      color = "rgb(58, 62, 65)",
      size = 12
    ),
    text = "",
    xref = "x",
    yref = "paper",
    showarrow = FALSE
  )

  for (i in 1:length(AnnotationAnchorLines)) {

    statResult <- AnnotationAnchorLines[[i]]$statResult
    annotation[["x"]] <- AnnotationAnchorLines[[i]]$x1 - (AnnotationAnchorLines[[i]]$x1 - AnnotationAnchorLines[[i]]$x0)/2
    annotation[["y"]] <- 1.05
    annotation[["text"]] <- case_when(
      is.na(statResult) ~ "NA",
      !AnnotationAnchorLines[[i]]$isSignificant ~ "ns",
      statResult <= 0.001 ~  "***",
      statResult <= 0.01 ~ "**",
      TRUE ~ "*"
    )
    annotation[["hovertext"]] <- formatPValue(statResult, adjustmentMethod, ...)
    annotations <- c(annotations, list(annotation))
  }

  keyText = ifelse(
    adjustmentMethod != "none",
    '<span><b>Statistical Significance Key</b>:        ns q > 0.1         * q <= 0.1         ** q <= 0.01        *** q <= 0.001</span>',
    '<span><b>Statistical Significance Key</b>:        ns p > 0.05        * p <= 0.05        ** p <= 0.01        *** p <= 0.001</span>'
  )

  adjLetterString <- ifelse(adjustmentMethod=="none","p","q")
  covariatesString <- ifelse(!is.null(covariates) & statTest == "Linear Model",glue(' adjusted for {glue_collapse(covariates,", ", last = " and ")}'),'')
  correctionString <- ifelse(adjustmentMethod=="none","",glue(", corrected using the {adjustmentMethod} method"))

  hoverText <- glue("{adjLetterString}-values calculated based on a {statTest}{covariatesString}{correctionString}.")

  significanceKey <- list(
    type = "line",
    align = "left",
    x = 0.5,
    y = 1.12,
    hovertext = hoverText,
    text = keyText,
    font = list(
      size = 12
    ),
    xref = "paper",
    yref = "paper",
    showarrow = FALSE
  )

  annotations <- c(annotations, list(significanceKey))

  return(annotations)

}

