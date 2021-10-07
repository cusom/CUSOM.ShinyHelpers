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
        select(!!key, !!id, !!response, !!covariates) %>% 
        group_by(!!key) %>% 
        summarise_at(vars(!!covariates), n_distinct) %>% 
        pivot_longer(!!covariates) %>% 
        mutate(KeepVar = ifelse(value >=2, 1, 0)) %>% 
        filter(KeepVar == 1) %>% 
        select(name) %>% 
        distinct() %>% 
        pull()
    }

  else {

    modelCovariates <- NULL

  }

  independentVariableClass <- .data %>% 
    select(!!independentVariable) %>% 
    pull() %>% 
    class()

  independentVars <- as.list(c(quo_name(independentVariable), modelCovariates))
    
  addInteractionTerm <- .data %>%
    select(!!interactionVariable) %>%
    summarise(n = n_distinct(!!interactionVariable)) %>%
    mutate(AddInteraction = ifelse(n >=2, TRUE, FALSE)) %>% 
    pull(AddInteraction)
    
  if(addInteractionTerm) {
  
    ivs =  glue_collapse(map(c(independentVars,quo_name(interactionVariable)), quo_name), sep = " + ")

    interactionTerm <- glue("{quo_name(independentVariable)} * {quo_name(interactionVariable)}")

    allVars <- glue("{ivs} + {interactionTerm}")

  }

  else {
  
    ivs = glue_collapse(map(independentVars, quo_name), sep = " + ")
    
    allVars <- ivs

  }

  lmformula = glue("{quo_name(response)} ~ {allVars}")

  rawModelData <- .data %>% 
    select(!!key, !!id, !!response, !!!independentVars, !!interactionVariable) %>% 
    nest(data = c(!!id,!!response, !!!independentVars, !!interactionVariable)) %>% 
    mutate(
        fit = map(data, ~lm(lmformula, data = .x)), 
        tidied = map(fit,broom::tidy)
    ) %>% 
    unnest(tidied) %>% 
    select(!!key,term,estimate, std.error, statistic, p.value) %>% 
    mutate(
        interaction.term.flag = str_detect(term, ":") 
        & str_detect(term, quo_name(independentVariable)) 
        & str_detect(term, quo_name(interactionVariable))
    ) %>%
    group_by(Analyte) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  interactionTermLocation <- rawModelData %>%
    filter(interaction.term.flag==TRUE) %>%
    group_by(rank) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    arrange(n) %>%
    top_n(1,n) %>%
    select(rank) %>%
    pull()

  if(length(interactionTermLocation)==0) {

      interactionTermLocation <- 999

  }

  linearModelData <- rawModelData %>%
    group_by(!!key) %>%
    summarize(
        log2_denom = first(estimate), 
        log2_num = nth(estimate, n = 2) + log2_denom, 
        log2FoldChange = ifelse(
            addInteractionTerm,
            nth(estimate,n=interactionTermLocation), 
            nth(estimate, n = 2)
        ), 
        FoldChange = 2^log2FoldChange, 
        p.value.original = nth(p.value,n = 2), 
        p.value.interaction = nth(p.value, n=interactionTermLocation) 
    ) %>% 
    arrange(p.value.original) %>% 
    ungroup() %>%
    mutate(
      p.value.original = case_when(
        is.na(p.value.interaction)~p.value.original, 
        TRUE ~ p.value.interaction
      )
    ) %>%
    select(-p.value.interaction) %>%
    mutate(
      p.value = p.adjust(
        p.value.original,
        method = getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod), 
        n = length(p.value.original)
      ), 
      `-log10pvalue` = -log10(p.value), 
      p.value.adjustment.method = adjustmentMethod,
      lmFormula = lmformula,  
      ivs = ivs
    ) 

    if (independentVariableClass %in% c("factor", "character")) {

      independentVariableLevels <- levels(.data[[quo_name(independentVariable)]])
      linearModelData <- linearModelData %>% 
      rename(
        `:=`(!!quo_name(independentVariableLevels[1]), log2_denom), 
        `:=`(!!quo_name(independentVariableLevels[2]), log2_num)
        )

    }

    else {

      linearModelData <- linearModelData %>% select(-log2_denom) %>% 
      rename(`:=`(!!quo_name(independentVariable), log2_num))

    }

  return(linearModelData)

}
