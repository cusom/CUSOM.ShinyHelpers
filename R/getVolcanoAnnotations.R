#' gets all possible volcano plot annotations in a single call. Combines list output from default and arrow annotations.
#'
#' @param .data dataframe containing fold change data along with selecte point indicator "selected_"
#' @param .xvar fold change variable
#' @param .yvar p value variable
#' @param .selected selected indicator column
#' @param .text name of column in dataframe to pull for arrow annotation text
#' @param pValueThreshold - p value threshold - passed to default arrow annotation function
#' @param upRegulatedText - text to use for "up regulated" groups -- down regulated text will simply replace "up" with "down"
#'
#' @return returns list of lists of plotly annotation objects
#'          -- up regulated arror at top of plot (with up regulated text)
#'          -- down regulated arrow at the top of plot (
#'          -- dp value threshold text with "p(a) > threshold" text and up arrow
#'          -- if a value is chosenl, adds in arrow annotation.
#' @export
getVolcanoAnnotations <- function(.data, .xvar, .yvar,.selected,.text, pValueThreshold,upRegulatedText) {

  .xvar <- enquo(.xvar)
  .yvar <- enquo(.yvar)
  .selected <- enquo(.selected)
  .text <- enquo(.text)

  maxFoldChange <- getMaxAbsValue(.data,!!.xvar,inf.rm = TRUE, buffer=1.1)

  default_a <- getDefaultVolcanoAnnotations(maxFoldChange,pValueThreshold,upRegulatedText)

  addArrow <- dim(.data %>% filter(!!.selected == 1))[1] > 0

  if(addArrow) {

    xcoordinate <- .data %>% ungroup() %>% filter(!!.selected == 1) %>% select(!!.xvar) %>% pull()
    ycoordinate <- .data %>% ungroup() %>% filter(!!.selected == 1) %>% select(!!.yvar) %>% pull()
    text <- .data %>% ungroup() %>% filter(!!.selected == 1) %>% select(!!.text) %>% pull()

    arrow_a <- getVolcanoArrowAnnotation(xcoordinate,ycoordinate,text)

    return(c(default_a,arrow_a))

  }

  else {

    return(default_a)

  }


}

