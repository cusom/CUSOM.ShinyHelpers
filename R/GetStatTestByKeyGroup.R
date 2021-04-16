#' Calculate statistical test between 2 groups for each key value in a dataframe
#'
#' @param .data A dataframe
#' @param .id A string or numeric column - represents a unique observation within each key/group combination. PersonId, LabId, etc
#' @param .key A string or numeric column - key value for dataframe. Statistics will be computed between groups for each key value
#' @param .group A string column indicating group membership - should be binary.
#' @param .value A numeric column - numerical value to use with statitical test between groups.
#' @param method a string - indicating which statisical test to perform. One of either ks.test, t.test, or wilcox.test
#' @param ... dots - accomodate additional arguments when function is called as parent router
#' @return dataframe containing resulting p.values for each key/group stat test

#' @export
getStatTestByKeyGroup <- function(.data,.id,.key,.group, baselineLabel, .response, method,...) {

  .id <- enquo(.id)
  .key <- enquo(.key)
  .group <- enquo(.group)
  .response <- enquo(.response)

  if(method=="Linear Model") {

    finalData <- .data %>%
      getLinearModel (!!.id,!!.key, !!.response,!!.group, ...)
  }

  else {

    foldChange <- .data %>%
      CUSOMShinyHelpers::summarizeByGroup(!!.response, !!.key, !!.group, na.rm = TRUE) %>%
      CUSOMShinyHelpers::calculateFoldChangeByKeyGroup(!!.key,!!.group, median, baselineLabel, inf.rm = TRUE)

    statsData <- .data %>%
      getPairwiseStatTestByKeyGroup(!!.id,!!.key,!!.group,!!.response,method,...)

    finalData <- inner_join(foldChange, statsData, by = quo_name(.key) )
  }

  return(finalData)

}

getLinearModel <- function(.data,.id, .key, .response, .group, adjustmentMethod, regressor, covariates, ...) {

  .id <- enquo(.id)
  .key <- enquo(.key)
  .response <- enquo(.response)
  .group <- enquo(.group)

  if(!is.null(covariates)) {

    modelCovariates <- .data %>%
      select(!!.key, !!.id, !!.response, !!covariates) %>%
      group_by(!!.key) %>%
      summarise_at(vars(!!covariates),n_distinct) %>%
      pivot_longer(!!covariates) %>%
      mutate(KeepVar = ifelse(value >=2,1,0)) %>%
      filter(KeepVar==1) %>%
      select(name) %>%
      distinct() %>%
      pull()

  }

  else {

    modelCovariates <- NULL

  }

  independentVars <- as.list(c(quo_name(.group),modelCovariates))

  ModelVarLevels <- levels(.data[[quo_name(.group)]])
  ivs = paste(map(independentVars, quo_name), collapse = " + ")
  lmformula = paste(quo_name(.response), " ~ ",  ivs)

  .data %>%
    select(!!.key, !!.id, !!.response, !!!independentVars) %>%
    nest(data = c(!!.id, !!.response, !!!independentVars)) %>%
    mutate(fit = map(data, ~lm(lmformula, data = .x)),
           tidied = map(fit,  broom::tidy)
    ) %>%
    unnest(tidied) %>%
    select(!!.key, term, estimate, p.value) %>%
    group_by(!!.key) %>%
    summarize(log2_denom = first(estimate),
              log2_num = nth(estimate, n = 2) + log2_denom,
              log2FoldChange = nth(estimate,  n = 2),
              FoldChange = 2^log2FoldChange,
              p.value.original = nth(p.value, n = 2)
    ) %>%
    arrange(p.value.original) %>%
    ungroup() %>%
    mutate(p.value = p.adjust(p.value.original, method = getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod),
                              n = length(p.value.original))) %>%
    mutate(p.value.adjustment.method = adjustmentMethod) %>%
    mutate(`-log10pvalue` = -log10(p.value)) %>%
    rename(`:=`(!!quo_name(ModelVarLevels[1]), log2_denom),
           `:=`(!!quo_name(ModelVarLevels[2]), log2_num)) %>%
    mutate("lmFormula" = lmformula, ivs = ivs)

}

getPairwiseStatTestByKeyGroup <- function (.data, .id, .key, .group, .response, method, adjustmentMethod, ...) {

  .id <- enquo(.id)
  .key <- enquo(.key)
  .group <- enquo(.group)
  .response <- enquo(.response)

  groupLabels <- .data %>% pull(!!.group) %>% unique()

  StatResults <- .data %>%
    select(!!.key,!!.group,!!.response) %>%
    group_by(!!.key,!!.group) %>%
    summarise(!!.response := list(!!.response)) %>%
    pivot_wider(names_from = !!.group, values_from = !!.response,values_fill = NA) %>%
    rename("x" = groupLabels[1], "y" = groupLabels[2]) %>%
    nest(data = c(y,x)) %>%
    mutate(fit = map(data, ~ runStatMethod(method,unlist(.x$x), unlist(.x$y))),
            tidied = map(fit, broom::tidy)
          ) %>%
    unnest(tidied) %>%
    select(-c(data,fit))


  if ("p.value" %in% colnames(StatResults)) {

    StatResults$p.value <- as.numeric(gsub(".*<", "\\1",format.pval(StatResults$p.value)))
    StatResults$p.value.original <- StatResults$p.value
    StatResults$p.value <- p.adjust(StatResults$p.value, getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod))
    StatResults$p.value.adjustment.method <- adjustmentMethod
    StatResults <- StatResults %>% mutate(`-log10pvalue` = -log10(p.value))

  }

  else {

    StatResults <- StatResults %>%
      mutate(statistic = NA, p.value = NA, `-log10pvalue` = 0, method = method, error = paste0("Not Enough Observations to run ",  method))
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
getStatTestByKeyGroup.methods <- c("Linear Model", "Kolmogorov-Smirnov Test","Student's t-test","Wilcoxon test")

#' Return available adjustmemt methodss
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


