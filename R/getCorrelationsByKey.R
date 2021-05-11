#' Calculate correlations between key var and all other keys in a dataset
#'
#' @param .data A dataframe
#' @param id A string or numeric column - represents a unique observation within each key/group combination. PersonId, LabId, RecordID, etc
#' @param key A string or numeric column - key value for dataframe. Correlations will be computed between comparison key and all other key values
#' @param response A numeric column - numerical value to use with correlations test between keys.
#' @param comparisonLabel A string value - key label to compute all correlations against 
#' @param type a string - indicating which correlations test to perform. One of either pearson or spearman -- see Hmisc::rcorr function 
#' @param adjustmentMethod - nammed method to correct p.values for multiple comparisons
#' @return dataframe containing resulting p.values for each key/group stat test

#' @export

getCorrelationsByKey <- function(.data,id,key,response,comparisonLabel, type = c("pearson", "spearman"), adjustmentMethod = "none" ) {
  
  match.arg(type)
  
  id <- enquo(id)
  key <- enquo(key)
  response <- enquo(response)
  adjustmentMethodName <- getStatTestByKeyGroup.getAdjustmentMethodName(adjustmentMethod)
  comparisonKeyName <- paste0('Comparison',quo_name(key))
 
  corr_matrix <- .data %>%
    select(!!id,!!key,!!response) %>%
    pivot_wider(id_cols = !!id, names_from = !!key, values_from = !!response) %>%
    select(-!!id) %>%
    as.matrix()
  
  correlations <- Hmisc::rcorr(corr_matrix, type=type)
  
  rho_matrix <- correlations$r %>% as_tibble(rownames = quo_name(key))
  
  all_rho <- rho_matrix %>% 
    select(!!key, matches(comparisonLabel)) %>% 
    filter(!!key!=comparisonLabel) %>%
    pivot_longer(-c(!!key), names_to = comparisonKeyName, values_to = "rho")
  
  pval_matrix <- correlations$P %>% as_tibble(rownames = quo_name(key))
  
  all_pval <- pval_matrix %>% 
    select(!!key, matches(comparisonLabel)) %>% 
    filter(!!key!=comparisonLabel) %>%
    pivot_longer(-c(!!key), names_to = comparisonKeyName, values_to = "p.value")
  
  corr_data <- all_rho %>%
    inner_join(all_pval,by=c(quo_name(key), comparisonKeyName)) %>%
    group_by(!!key) %>% 
    mutate(
      p.value.original = p.value,
      p.value = p.adjust(p.value, method = adjustmentMethodName, n = length(p.value)), 
      p.value.adjustment.method = adjustmentMethod
    ) %>% 
    ungroup() 
  
  maxFinite <- corr_data %>%
    filter(p.value > 0) %$%
    min(p.value) %>%
    -log10(.)
  
  corr_data <- corr_data %>%
    mutate(
      shape = if_else(p.value == 0, "infinite", "finite"),
      p.value = if_else(p.value == 0, 10^-(maxFinite * 1.05), p.value), 
      `-log10pvalue` = -log10(p.value)
    )
  
  return(
    corr_data
  )
  
}