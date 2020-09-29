#' Calculate statistical test across 2 groups for each key value in a dataframe
#'
#' @param .data A dataframe
#' @param .id A string or number - represents a unique observation within each key/group combination. PersonId, LabId, etc
#' @param .key A string or number - key value for dataframe. Statistics will be computed between groups for each key value
#' @param .group A string - column indicating group membership - should be binary.
#' @param .value A number - numerical value to use with statitical test.
#' @param .statTestName a string - indicating which statisical test to perform.
#' @param .addLog10 boolean - whether or not to add a -log10 transfromation of resulting p.values
#' @return dataframe containing resulting p.values for each key/group stat test

#' @export
GetStatTestByKeyGroup <- function(.data, .id, .key, .group, .value, .method, .addLog10 = TRUE) {

  .id <- enquo(.id)
  .key <- enquo(.key)
  .group <- enquo(.group)
  .value <- enquo(.value)

  keys      <- .data %>% pull(!!.key) %>% unique()
  groupLabels <- .data %>% pull(!!.group) %>% unique()

  D1 <- .data %>%
    filter(!!.group==groupLabels[1]) %>%
    select(!!.id, !!.key,!!.value) %>%
    pivot_wider(names_from = !!.key, values_from=!!.value, values_fill=NA) %>%
    select(-c(!!.id)) %>%
    as.data.frame()

  D2 <- .data %>%
    filter(!!.group!=groupLabels[1]) %>%
    select(!!.id, !!.key,!!.value)  %>%
    pivot_wider(names_from = !!.key, values_from=!!.value, values_fill=NA) %>%
    select(-c(!!.id)) %>%
    as.data.frame()

  StatResults <- colnames(D1) %>%
    set_names() %>%
    map(~ runStatsTest(.method, D1[, .x], D2[, .x])) %>%
    map_dfr(., broom::tidy, .id = quo_name(.key))

  if(.addLog10) {
    StatResults <- StatResults %>%
      mutate(`-log10pvalue` = -log10(p.value))
  }

  return(StatResults)

}

runStatsTest <- function(testName,x,y) {

  testName <- str_replace(testName,' ','.')
  testName <- tolower(testName)

  if(testName=="ks.test") {
    return(ks.test(x,y))
  }
  else if (testName=="t.test"){
    return(t.test(x,y))
  }
  else {
    return("Not Yet Implmented")
  }
}