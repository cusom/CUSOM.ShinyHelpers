#' Calculate correlations between key var and all other keys in a dataset
#'
#' @param .data A dataframe
#' @param id A string or numeric column - represents a unique observation
#' within each key/group combination. PersonId, LabId, RecordID, etc
#' @param key A string or numeric column - key value for dataframe.
#' Correlations will be computed between comparison key and all other key values
#' @param response A numeric column - numerical value to use with
#' correlations test between keys.
#' @param comparisonLabel A string value - key label to compute
#' all correlations against
#' @param type a string - indicating which correlations test to perform.
#' One of either pearson or spearman -- see Hmisc::rcorr function
#' @param adjustmentMethod - nammed method to correct p.values
#' for multiple comparisons
#' @return dataframe containing resulting p.values for each key/group stat test
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @import dplyr
#' @importFrom Hmisc rcorr
#' @export

getCorrelationsByKey <- function(
  .data,
  id,
  key,
  response,
  comparisonLabel,
  type = c("pearson", "spearman"),
  adjustmentMethod = "none"
) {

  match.arg(type)

  id <- rlang::enquo(id)
  key <-  rlang::enquo(key)
  response <- rlang::enquo(response)
  adjustmentMethodName <- getStatTestByKeyGroup.getAdjustmentMethodName(
    adjustmentMethod
  )
  comparisonKeyName <- paste0("Comparison", rlang::quo_name(key))

  corr_matrix <- .data |>
    dplyr::select(!!id, !!key, !!response) |>
    tidyr::pivot_wider(
      id_cols = !!id,
      names_from = !!key,
      values_from = !!response
    ) |>
    dplyr::select(-!!id) |>
    as.matrix()

  correlations <- Hmisc::rcorr(corr_matrix, type = type)

  rho_matrix <- correlations$r |>
    tibble::as_tibble(rownames = rlang::quo_name(key))

  all_rho <- rho_matrix |>
    dplyr::select(!!key, tidyselect::matches(comparisonLabel)) |>
    dplyr::filter(!!key != comparisonLabel) |>
    tidyr::pivot_longer(
      -c(!!key),
      names_to = comparisonKeyName,
      values_to = "rho"
    )

  pval_matrix <- correlations$P |>
    tibble::as_tibble(rownames = rlang::quo_name(key))

  all_pval <- pval_matrix |>
    dplyr::select(!!key, tidyselect::matches(comparisonLabel)) |>
    dplyr::filter(!!key != comparisonLabel) |>
    tidyr::pivot_longer(
      -c(!!key),
      names_to = comparisonKeyName,
      values_to = "p.value"
    )

  corr_data <- all_rho |>
    dplyr::inner_join(
      all_pval,
      by = c(rlang::quo_name(key), comparisonKeyName)
    ) |>
    dplyr::group_by(!!key) |>
    dplyr::mutate(
      p.value.original = p.value,
      p.value = stats::p.adjust(
        p.value,
        method = adjustmentMethodName,
        n = length(p.value)
      ),
      p.value.adjustment.method = adjustmentMethod
    ) |>
    dplyr::ungroup()

  maxFinite <- corr_data |>
    dplyr::filter(p.value > 0) %$%
    min(p.value) |>
    (\(x) {-log10(x)})()

  corr_data <- corr_data |>
    dplyr::mutate(
      shape = ifelse(p.value == 0, "infinite", "finite"),
      p.value = ifelse(p.value == 0, 10^-(maxFinite * 1.05), p.value),
      `-log10pvalue` = -log10(p.value)
    )

  return(
    corr_data
  )
}