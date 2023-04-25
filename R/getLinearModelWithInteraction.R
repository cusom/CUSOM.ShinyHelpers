#' run linear model with interaction between independent variable
#' and specificed interaction variable
#'
#' @param .data A dataframe
#' @param id A string or numeric column - represents a unique observation
#' within each key/group combination. PersonId, LabId, etc
#' @param key A string or numeric column - key value for dataframe.
#' Statistics will be computed between groups for each key value
#' @param response numeric column - used as response variable in
#' linear model
#' @param independentVariable a string or numeric column
#' used as independent variable in linear model
#' @param covariates vector of features names in the dataset
#' to be added to linear model as covariates
#' @param interactionVariable a string or numeric column in dataset to
#' be added to interaction term with response variable
#' @param adjustmentMethod optional argument specifiying multiple
#' hypothesis correction to apply to linear model result - defaults to "none"
#' @param ... dots - accomodate additional arguments when function is
#' called as parent router
#' @return dataframe containing resulting p.values for each key/group stat test
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang :=
#' @import dplyr
#' @importFrom stringr str_detect
#' @export
getLinearModelWithInteraction <- function(
  .data,
  id,
  key,
  response,
  independentVariable,
  covariates,
  interactionVariable,
  adjustmentMethod = "none",
  ...
) {

  id <- rlang::enquo(id)
  key <- rlang::enquo(key)
  response <- rlang::enquo(response)
  independentVariable <- rlang::enquo(independentVariable)
  interactionVariable <- rlang::enquo(interactionVariable)

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

  addInteractionTerm <- .data |>
    dplyr::select(!!interactionVariable) |>
    dplyr::summarise(
      n = dplyr::n_distinct(!!interactionVariable)
    ) |>
    dplyr::mutate(
      AddInteraction = ifelse(
        n >= 2,
        TRUE,
        FALSE
      )
    ) |>
    dplyr::pull(AddInteraction)

  if (addInteractionTerm) {

    ivs =  glue::glue_collapse(
      purrr::map(
        c(
          independentVars,
          rlang::quo_name(interactionVariable)
        ),
        rlang::quo_name
      ),
      sep = " + "
    )

    interactionTerm <- glue::glue(
      "{rlang::quo_name(independentVariable)} * {rlang::quo_name(interactionVariable)}"
      )

    allVars <- glue::glue("{ivs} + {interactionTerm}")

  } else {

    ivs <- glue::glue_collapse(
      purrr::map(
        independentVars,
        rlang::quo_name
      )
      , sep = " + "
    )

    allVars <- ivs

  }

  lmformula = glue::glue("{rlang::quo_name(response)} ~ {allVars}")

  rawModelData <- .data |>
    dplyr::select(
      !!key, 
      !!id, 
      !!response, 
      !!!independentVars, 
      !!interactionVariable
    ) |>
    tidyr::nest(
      data = c(!!id, !!response, !!!independentVars, !!interactionVariable)
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
      std.error,
      statistic,
      p.value
    ) |>
    dplyr::mutate(
      interaction.term.flag = stringr::str_detect(term, ":")
        & stringr::str_detect(term, rlang::quo_name(independentVariable))
        & stringr::str_detect(term, rlang::quo_name(interactionVariable))
    ) |>
    dplyr::group_by(!!key) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::ungroup()

  interactionTermLocation <- rawModelData |>
    dplyr::filter(interaction.term.flag == TRUE) |>
    dplyr::group_by(rank) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::arrange(n) |>
    dplyr::top_n(1, n) |>
    dplyr::select(rank) |>
    dplyr::pull()

  if (length(interactionTermLocation) == 0) {
      interactionTermLocation <- 999
  }

  linearModelData <- rawModelData |>
    dplyr::group_by(!!key) |>
    dplyr::summarize(
      log2_denom = dplyr::first(estimate),
      log2_num = dplyr::nth(estimate, n = 2) + log2_denom,
      log2FoldChange = ifelse(
        addInteractionTerm,
        dplyr::nth(estimate, n = interactionTermLocation),
        dplyr::nth(estimate, n = 2)
      ),
      FoldChange = 2^log2FoldChange,
      p.value.original = dplyr::nth(p.value, n = 2),
      p.value.interaction = dplyr::nth(p.value, n = interactionTermLocation)
    ) |>
    dplyr::arrange(p.value.original) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      p.value.original = dplyr::case_when(
        is.na(p.value.interaction) ~ p.value.original,
        TRUE ~ p.value.interaction
      )
    ) |>
    dplyr::select(-p.value.interaction) |>
    dplyr::mutate(
      p.value = stats::p.adjust(
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

      independentVariableLevels <- levels(
        .data[[rlang::quo_name(independentVariable)]]
      )

      linearModelData <- linearModelData |>
      dplyr::rename(
        `:=`(!!rlang::quo_name(independentVariableLevels[1]), log2_denom),
        `:=`(!!rlang::quo_name(independentVariableLevels[2]), log2_num)
        )

    }

    else {

      linearModelData <- linearModelData |>
        dplyr::select(-log2_denom) |>
        dplyr::rename(`:=`(!!rlang::quo_name(independentVariable), log2_num))

    }

  return(linearModelData)

}
