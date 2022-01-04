#' function to generate stat annotations across multiple groups
#'
#' @param AnnotationAnchorLines list of annotation references (outputted from getStatAnnotationAnchorLines function)
#' @param adjustmentMethod string - name of multiple hypothesis correction method applied to significance variable  
#' @param ... dots - additional arguments to be passed to other functions (like formatPValue)
#' @return list statistic annotations to be overlayed for each group
#' @export


getGroupedStatAnnotations <- function (AnnotationAnchorLines, adjustmentMethod,...) {

  text = ifelse(
    adjustmentMethod!="none",
    "<span><b>Statistical Significance Key</b>:        ns q > 0.1         * q <= 0.1         ** q <= 0.01        *** q <= 0.001</span>", 
    "<b>Statistical Significance Key</b>:        ns p > 0.05         * p <= 0.05        ** p <= 0.01        *** p <= 0.001"
  )

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
    annotation[["text"]] <- ifelse(!AnnotationAnchorLines[[i]]$isSignificant,"ns", ifelse(statResult <= 0.001,"***", ifelse(statResult <= 0.01, "**", "*")))
    annotation[["hovertext"]] <- formatPValue(statResult,  adjustmentMethod, ...)
    
    annotations <- c(annotations, list(annotation))
    
  }

  significanceKey <- list(
    type = "line",
    align = "left",
    x = 0.5,   
    y = 1.215,  
    hovertext = "hover over any annotation below to see more detailed information",
    text = text,
    font = list(
      size = 12
    ),
    xref="paper",  
    yref ="paper",   
    showarrow = FALSE
  )

  annotations <- c(annotations, list(significanceKey))

  return(annotations)

}

