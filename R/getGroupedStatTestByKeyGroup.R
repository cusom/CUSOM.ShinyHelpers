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

  statsData <- tibble()

  for(group in groups) {

    statsData <- statsData %>%
      bind_rows(
        .data %>%
          filter(!!groupVar == group) %>%
          CUSOMShinyHelpers::getStatTestByKeyGroup(...) %>%
          mutate(`:=`(!!groupVar, group)) %>%
          ungroup() %>%
          select(!!groupVar, p.value)
      )

  }

  return(statsData)

}
