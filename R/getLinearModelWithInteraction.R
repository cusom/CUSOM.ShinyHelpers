#' run linear model with interaction between independent variable and specificed interaction variable
#'
#' @param .data A dataframe
#' @param id A string or numeric column - represents a unique observation within each key/group combination. PersonId, LabId, etc
#' @param key A string or numeric column - key value for dataframe. Statistics will be computed between groups for each key value
#' @param response numeric column - used as response variable in linear model
#' @param independentVariable a string or numeric column - used as independent variable in linear model
#' @param covariates vector of features names in the dataset to be added to linear model as covariates
#' @param interactionVariable a string or numeric column in dataset to be added to interaction term with response variable
#' @param adjustmentMethod optional argument specifiying multiple hypothesis correction to apply to linear model result - defaults to "none"
#' @param ... dots - accomodate additional arguments when function is called as parent router
#' @return dataframe containing resulting p.values for each key/group stat test

#' @export
getLinearModelWithInteraction <- function (.data,id,key,response,independentVariable,covariates,interactionVariable,adjustmentMethod="none", ...) {

  id <- enquo(id)
  key <- enquo(key)
  response <- enquo(response)
  independentVariable <- enquo(independentVariable)
  interactionVariable <- enquo(interactionVariable)

  if (!is.null(covariates)) {

    modelCovariates <- .data %>%
      select(!!key, !!id, !!response,!!covariates) %>%
      group_by(!!key) %>%
      summarise_at(vars(!!covariates), n_distinct) %>%
      pivot_longer(!!covariates) %>%
      mutate(KeepVar = ifelse(value >= 2, 1, 0)) %>%
      filter(KeepVar == 1) %>%
      select(name) %>%
      distinct() %>%
      pull()

  }

  else {

    modelCovariates <- NULL

  }

  independentVars <- as.list(c(quo_name(independentVariable),modelCovariates))

  ivs = paste(paste(map(independentVars, quo_name), collapse = " + "),'+',quo_name(interactionVariable),collapse = "")

  interactionTerm <- paste(quo_name(independentVariable),'*',quo_name(interactionVariable))

  allVars <- paste0(ivs,' + ',interactionTerm)

  lmformula = paste(quo_name(response), " ~ ", allVars)

  interactionModelData <- .data %>%
    select(!!key, !!id, !!response,!!!independentVars,!!interactionVariable) %>%
    nest(data = c(!!id, !!response,!!!independentVars,!!interactionVariable)) %>%
    mutate(
      fit = map(data, ~lm(lmformula, data = .x)),
      tidied = map(fit, broom::tidy)
    ) %>%
    unnest(tidied) %>%
    select(term,estimate,std.error,statistic,p.value) %>%
    mutate(
      interaction.term.flag = str_detect(term,':') & str_detect(term,quo_name(independentVariable)) & str_detect(term,quo_name(interactionVariable)),
      `-log10pvalue` = -log10(p.value),
      p.value.adjustment.method = adjustmentMethod,
      lmFormula = lmformula,
      ivs = ivs
    )

  return(interactionModelData)

}
