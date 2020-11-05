#' Calculate statistical test between 2 groups for each key value in a dataframe
#'
#' @param .data A dataframe
#' @param .id A string or numeric column - represents a unique observation within each key/group combination. PersonId, LabId, etc
#' @param .key A string or numeric column - key value for dataframe. Statistics will be computed between groups for each key value
#' @param .group A string column indicating group membership - should be binary.
#' @param .value A numeric column - numerical value to use with statitical test between groups.
#' @param method a string - indicating which statisical test to perform. One of either ks.test, t.test, or wilcox.test
#' @param adjustMethod a string - indicating which method to be used to adjust P-values for multiple comparisons - defaults to "none"
#' @param addLog10 boolean - whether or not to add a -log10 transfromation of resulting p.values
#' @return dataframe containing resulting p.values for each key/group stat test

#' @export
getStatTestByKeyGroup <- function(.data, .id, .key, .group, .value, method, adjustMethod ="none", addLog10 = TRUE) {

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
    map(~ runStatMethod(method, D1[, .x], D2[, .x])) %>%
    map_dfr(., broom::tidy, .id = quo_name(.key))

  # check for valid results from test.
  if("p.value" %in% colnames(StatResults)) {
    StatResults$p.value <- as.numeric(gsub('.*<','\\1',format.pval(StatResults$p.value)))
    StatResults$p.value.original <- StatResults$p.value
    StatResults$p.value <- p.adjust(StatResults$p.value, getStatTestByKeyGroup.getAdjustmentMethodName(adjustMethod))
    StatResults$p.value.adjustment.method <- adjustMethod

    if(addLog10) {

      StatResults <- StatResults %>%
        mutate(`-log10pvalue` = -log10(p.value))

    }

  }

  # if NA was returned, return back empty results with error column
  else {

    StatResults <- StatResults %>%
      mutate(statistic = NA, p.value = NA, `-log10pvalue` = 0,
             method = method, error = paste0("Not Enough Observations to run ", method )
             )

  }

  return(StatResults)

}


runStatMethod <- function(method,x,y) {

  methodName <- getStatTestByKeyGroup.getMethodName(method)

  tryCatch({

    result <- do.call(methodName,args = list(x,y))

    return(result)


  }, error = function(err) {

    return(NA)

  })

}

#' Return implemented stat test methods
#' @return vector of all implemented stat test methods
#' @export
getStatTestByKeyGroup.methods <- c("Kolmogorov-Smirnov Test","Student's t-test","Wilcoxon test")

#' Return available adjustmemt methods
#' @return vector of all available adjustmemt methods
#' @export
getStatTestByKeyGroup.adjustment.methods <- p.adjust.methods

# internal lookup between label and method name for all stat tests
getStatTestByKeyGroup.getMethodName <- function(method) {

  statMethods <- data.frame(
    StatTestMethodLabel = c("Kolmogorov-Smirnov Test","Student's t-test","Wilcoxon test"),
    StatTestMethodName = c("ks.test","t.test","wilcox.test")
  )

  methodName <- as.character(statMethods[which(statMethods$StatTestMethodLabel == method),'StatTestMethodName'])

  if(length(methodName)==0) {
    msg <- paste0("'", method,"' Method Not Yet Implemented")
    stop(msg, call. = FALSE)
  }
  else {
    return(methodName)
  }

}

# internal lookup between label and method name for all adjustment methods
getStatTestByKeyGroup.getAdjustmentMethodName <- function(adjustment) {

  adjustmentMethods <- data.frame(
    AdjustmentMethodLabel = c("None","Bonferroni","Benjamini-Hochberg (FDR)"),
    AdjustmentMethodName = c("none","bonferroni","BH")
  )

  adjustmentName <- as.character(adjustmentMethods[which(adjustmentMethods$AdjustmentMethodLabel == adjustment),'AdjustmentMethodName'])

  if(length(adjustmentName)==0) {
    return(adjustment)
  }
  else {
    return(adjustmentName)
  }

}


