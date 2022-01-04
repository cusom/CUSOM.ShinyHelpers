#' function to generate list of line anchors for stat annotations
#'
#' @param .data dataframe
#' @param group column of group labels used to calcualte statistic
#' @param groupMembers string/vector - all possible group labels from group column
#' @param significanceVariable column containing statistical variable result (i.e. p.value from statistical test)
#' @param groupIsSignificant column - logical indicating whether significanceVariable value is significant.
#' @param includeInsignificantValues logical - whether to include results that are not significant. Defaults to FALSE
#' @return list of position references for each annotation
#' @export

getStatAnnotationAnchorLines <- function (.data, group, groupMembers, significanceVariable, groupIsSignificant, includeInsignificantValues = FALSE) {

  group <- enquo(group)
  significanceVariable <- enquo(significanceVariable)
  groupIsSignificant <- enquo(groupIsSignificant)
  
  dataGroupMembers <- .data %>% select(!!group) %>% unique() %>% pull()

  allGroupMembers <- intersect(groupMembers, dataGroupMembers)

  line <- list(
    type = "line", 
    x = 0, 
    y = 0, 
    line = list(
      color = "grey"
    ), 
    xref = "x", 
    yref = "paper", 
    showarrow = FALSE
  )

  lines <- list()

  xpositions <- seq(-0.2, length(allGroupMembers) - 1, 1)  

  i <- 0

  for (igroup in allGroupMembers) {
  
    i <- i + 1
  
    statResult <- .data %>% 
      filter(!!group == igroup) %>% 
      select(!!significanceVariable) %>% 
      unique() %>% 
      pull()

    isSignificant <- .data %>% 
      filter(!!group == igroup) %>% 
      select(!!groupIsSignificant) %>% 
      unique() %>% 
      pull()  
  
    if (!is.na(statResult) | includeInsignificantValues) {
      line[["x0"]] <- xpositions[i]
      line[["x1"]] <- xpositions[i] + 0.5
      line[["y0"]] <- 1
      line[["y1"]] <- 1
      line[["statResult"]] <- statResult
      line[["isSignificant"]] <- isSignificant
      line[["group"]] <- igroup
      lines <- c(lines, list(line))
    }
  }
  return(lines)
}
