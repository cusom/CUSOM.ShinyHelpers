#' Calculate statistical test between 2 groups for each key value in a dataframe
#'
#' @param .data A dataframe
#' @param id A string or numeric column - represents a unique
#' observation within each key/group combination. PersonId, LabId, etc
#' @param key A string or numeric column - key value for dataframe.
#' Statistics will be computed between groups for each key value
#' @param response A numeric column - numerical value to use
#' with statitical test between groups.
#' @param independentVariable A string column indicating group membership - should be binary.
#' @param baselineLabel independentVariable label used for fold change comparison / calculation
#' @param testMethod a string - indicating which statisical
#' test to perform. One of either ks.test, t.test, or wilcox.test
#' @param ... dots - accomodate additional arguments
#' function is called as parent router
#' @return dataframe containing resulting p.values for each key/group stat test
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang :=
#' @import dplyr
#' @export
getStatTestByKeyGroup <- function(
  .data,
  id,
  key,
  response,
  independentVariable,
  baselineLabel,
  testMethod,
  ...
)
{

  id <- rlang::enquo(id)
  key <- rlang::enquo(key)
  response <- rlang::enquo(response)
  independentVariable <- rlang::enquo(independentVariable)

  if (testMethod == "Linear Model") {
    finalData <- .data |>
      getLinearModel(
        !!id,
        !!key,
        !!response,
        !!independentVariable,
        ...
      )
  } else {

    foldChange <- .data |>
      CUSOMShinyHelpers::summarizeByGroup(
        !!response,
        !!key,
        !!independentVariable,
        na.rm = TRUE
      ) |>
      CUSOMShinyHelpers::calculateFoldChangeByKeyGroup(
        !!key,
        !!independentVariable,
        median,
        baselineLabel,
        inf.rm = TRUE
      )

    statsData <- .data |>
      getPairwiseStatTestByKeyGroup(
        !!id,
        !!key,
        !!independentVariable,
        !!response,
        method = testMethod,
        ...
      )

    finalData <- dplyr::inner_join(
      foldChange,
      statsData,
      by = rlang::quo_name(key)
    )

  }

  return(finalData)

}

#' run linear model
#'
#' @param .data data for model
#' @param id id column - a linear model will be run for each id value in .data
#' @param key a unique identifier per id
#' @param response a numeric column to be used as response variable in linear model
#' @param independentVariable indepdendent variable to be used in linear model
#' @param covariates list of covariate columns to be included in linear model
#' @param adjustmentMethod p.value adjustment method to be applied
#' @param ... dots - accomodate additional arguments
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang :=
#' @import dplyr
getLinearModel <- function(
  .data,
  id,
  key,
  response,
  independentVariable,
  covariates,
  adjustmentMethod,
  ...
) {

  id <- rlang::enquo(id)
  key <- rlang::enquo(key)
  response <- rlang::enquo(response)
  independentVariable <- rlang::enquo(independentVariable)

  if (!is.null(covariates)) {
    modelCovariates <- .data |>
      dplyr::select(!!key, !!id, !!response, !!covariates) |>
      dplyr::group_by(!!key) |>
      dplyr::summarise_at(
        dplyr::vars(!!covariates),
        dplyr::n_distinct
      ) |>
      tidyr::pivot_longer(!!covariates) |>
      dplyr::mutate(KeepVar = ifelse(value >= 2, 1, 0)) |>
      dplyr::filter(KeepVar == 1) |>
      dplyr::select(name) |>
      dplyr::distinct() |>
      dplyr::pull()
  } else {
    modelCovariates <- NULL
  }

  independentVariableClass <- .data |>
    dplyr::select(!!independentVariable) |>
    dplyr::pull() |>
    class()

  independentVars <- as.list(
    c(
      rlang::quo_name(independentVariable),
      modelCovariates
    )
  )

  ivs <- paste(
    purrr::map(independentVars, rlang::quo_name),
    collapse = " + "
  )

  lmformula <- paste(rlang::quo_name(response), " ~ ", ivs)

  linearModelData <- .data |>
    dplyr::select(
      !!key,
      !!id,
      !!response,
      !!!independentVars
    ) |>
    tidyr::nest(
      data = c(!!id, !!response, !!!independentVars)
    ) |>
    dplyr::mutate(
      fit = purrr::map(data, ~stats::lm(lmformula, data = .x)),
      tidied = purrr::map(fit, broom::tidy)
    ) |>
    tidyr::unnest(tidied) |>
    dplyr::select(
      !!key,
      term,
      estimate,
      p.value
    ) |>
    dplyr::group_by(!!key) |>
    dplyr::summarize(
      log2_denom = dplyr::first(estimate),
      log2_num = dplyr::nth(estimate, n = 2) + log2_denom,
      log2FoldChange = dplyr::nth(estimate, n = 2),
      FoldChange = 2^log2FoldChange,
      p.value.original = dplyr::nth(p.value, n = 2)
    ) |>
    dplyr::arrange(p.value.original) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      p.value = stats::p.adjust(p.value.original,
      method = getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod),
      n = length(p.value.original))
    ) |>
    dplyr::mutate(p.value.adjustment.method = adjustmentMethod) |>
    dplyr::mutate(`-log10pvalue` = -log10(p.value)) |>
    dplyr::mutate(lmFormula = lmformula, ivs = ivs)

  if(independentVariableClass %in% c("factor", "character")) {

    independentVariableLevels <- levels(
      .data[[rlang::quo_name(independentVariable)]]
    )

    linearModelData <- linearModelData |>
      dplyr::rename(
        `:=`(
          !!rlang::quo_name(independentVariableLevels[1]),
          log2_denom
        ),
        `:=`(
          !!rlang::quo_name(independentVariableLevels[2]),
          log2_num
        )
        )

  } else {

    linearModelData <- linearModelData |>
      dplyr::select(-log2_denom) |>
      dplyr::rename(
        `:=`(
          !!rlang::quo_name(independentVariable),
          log2_num
        )
      )
  }

  return(linearModelData)

}

getPairwiseStatTestByKeyGroup <- function (
  .data,
  .id,
  .key,
  .group,
  .response,
  method,
  adjustmentMethod,
  ...
) {

  .id <- rlang::enquo(.id)
  .key <- rlang::enquo(.key)
  .group <- rlang::enquo(.group)
  .response <- rlang::enquo(.response)

  groupLabels <- .data |>
    dplyr::select(!!.group) |>
    dplyr::distinct() |>
    dplyr::pull()
  
  StatResults <- .data |>
    dplyr::select(!!.key, !!.group, !!.response) |>
    dplyr::group_by(!!.key, !!.group) |>
    dplyr::summarise(!!.response := list(!!.response)) |>
    tidyr::pivot_wider(
      names_from = !!.group,
      values_from = !!.response,
      values_fill = NA
    ) |>
    dplyr::rename(
      "x" = groupLabels[1],
      "y" = groupLabels[2]
    ) |>
    tidyr::nest(data = c(y, x)) |>
    dplyr::mutate(
      fit = purrr::map(
        data, ~ runStatMethod(
          method,
          unlist(.x$x),
          unlist(.x$y)
          )
        ),
      tidied = purrr::map(fit, broom::tidy)
    ) |>
    tidyr::unnest(tidied) |>
    dplyr::select(-c(data, fit))


  if ("p.value" %in% colnames(StatResults)) {

    StatResults$p.value <- as.numeric(
      gsub(
        ".*<",
        "\\1",
        format.pval(StatResults$p.value)
      )
    )
    StatResults$p.value.original <- StatResults$p.value
    StatResults$p.value <- stats::p.adjust(
      StatResults$p.value,
      getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod)
    )
    StatResults$p.value.adjustment.method <- adjustmentMethod
    StatResults <- StatResults |>
      dplyr::mutate(`-log10pvalue` = -log10(p.value))

  } else {

    StatResults <- StatResults |>
      dplyr::mutate(
        statistic = NA,
        p.value = NA,
        `-log10pvalue` = 0,
         method = method,
         error = paste0("Not Enough Observations to run ", method)
      )
  }

  return(StatResults)

}

runStatMethod <- function(method, x, y) {

  methodName <- getStatTestByKeyGroup.getMethodName(method)

  tryCatch({

    result <- do.call(methodName, args = list(x, y))

    return(result)


  }, error = function(err) {

    return(NA)

  })

}

#' Return implemented stat test methods
#' @return vector of all implemented stat test methods
#' @export
getStatTestByKeyGroup.methods <- c("Linear Model",
  "Kolmogorov-Smirnov Test",
  "Student's t-test",
  "Wilcoxon test"
  )

#' Return available adjustmemt methodss
#' @return vector of all available adjustmemt methods
#' @export
getStatTestByKeyGroup.adjustment.methods <- p.adjust.methods

# internal lookup between label and method name for all stat tests
getStatTestByKeyGroup.getMethodName <- function(method) {

  statMethods <- tibble::tibble(
    StatTestMethodLabel = c("Kolmogorov-Smirnov Test",
                            "Student's t-test",
                            "Wilcoxon test"
    ),
    StatTestMethodName = c("ks.test", "t.test", "wilcox.test")
  )

  methodName <- statMethods |>
    dplyr::filter(
      StatTestMethodLabel == method | StatTestMethodName == method
    ) |>
    dplyr::select(StatTestMethodName) |>
    dplyr::pull()

  if(length(methodName) == 0) {
    msg <- paste0("'", method, "' Method Not Yet Implemented")
    stop(msg, call. = FALSE)
  } else {
    return(methodName)
  }

}

# internal lookup between label and method name for all adjustment methods
getStatTestByKeyGroup.getAdjustmentMethodName <- function(adjustment) {

  adjustmentMethods <- data.frame(
    AdjustmentMethodLabel = c("None", "Bonferroni", "Benjamini-Hochberg (FDR)"),
    AdjustmentMethodName = c("none", "bonferroni", "BH")
  )

  adjustmentName <- as.character(
    adjustmentMethods[which(
      adjustmentMethods$AdjustmentMethodLabel == adjustment
      ),
    "AdjustmentMethodName"]
    )

  if(length(adjustmentName) == 0) {
    return(adjustment)
  } else {
    return(adjustmentName)
  }

}