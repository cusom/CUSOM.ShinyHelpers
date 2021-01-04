#' function to generate stat annotations across multiple groups
#'
#' @param AnnotationAnchorLines list of annotation references (outputted from getStatAnnotationAnchorLines function)

#' @return list statistic annotations to be overlayed for each group
#' @export


getGroupedStatAnnotations <- function(AnnotationAnchorLines) {

  annotations <- list()

  annotation <- list(
    type = "line",
    x = 0,
    y = 0,
    line = list(color = "blue"),
    text = "*",
    xref = "x",
    yref = "paper",
    showarrow = FALSE
  )


  for (i in 1:length(AnnotationAnchorLines)) {

    statResult <- AnnotationAnchorLines[[i]]$statResult
    annotation[["x"]] <- AnnotationAnchorLines[[i]]$x1 - (AnnotationAnchorLines[[i]]$x1 - AnnotationAnchorLines[[i]]$x0) / 2
    annotation[["y"]] <- 1.05
    annotation[["text"]] <-  ifelse(statResult<=0.001,'***',ifelse(statResult<=0.01,'**','*'))

    annotations <- c(annotations, list(annotation))

  }

  return(annotations)

}
