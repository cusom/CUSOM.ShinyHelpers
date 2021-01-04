#' function to run stat tests for multiple groups
#'
#' @param .data dataframe
#' @param .groupVar string - column of group values
#' @param ... additional arguments to be passed to getStatTestByKeyGroup function

#' @return dataframe containing resulting p.value for each group calculated
#' @export


getGroupedStatTestByKeyGroup <- function(.data, groupVar, ...) {

  groupVar <- enquo(groupVar)

  groups <- .data %>% select(!!groupVar) %>% unique() %>% pull()

  statsData <- data.frame(Group = character(),
                          p.value = numeric(),
                          stringsAsFactors=FALSE,
                          check.names = FALSE)

  for(group in groups) {

    results <- .data %>%
      filter(!!groupVar==group) %>%
      CUSOMShinyHelpers::getStatTestByKeyGroup(...) %>%
      mutate(!!groupVar := group) %>%
      select(!!groupVar,p.value)

    statsData <- rbind(statsData,results)

  }

  return(statsData)

}
