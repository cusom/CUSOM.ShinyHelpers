#' Run 2-sided fgseaMultilevel analysis (positive and negative) with adjusted pvals
#'
#' @param geneset list List of gene sets to check.
#' @param ranks Named vector of gene-level stats (Gene + "Fold Change" or "rho"). Names should be the same as in 'pathways'
#' @param minSize int default 15: Minimal size of a gene set to test. All pathways below the threshold are excluded.
#' @param maxSize int default 500: Maximal size of a gene set to test. All pathways above the threshold are excluded.
#' @param gseaParam int default 0: GSEA parameter value, all gene-level statis are raised to the power of 'gseaParam' before calculation of GSEA enrichment scores.
#' @param eps numeric default 0.0: This parameter sets the boundary for calculating the p value.
#' @return A tibble with GSEA results. Each row corresponds to a tested pathway. The columns are the following
#'    pathway:  name of the pathway as in 'names(pathway)';
#'    pval: an enrichment p-value;
#'    padj – a BH-adjusted p-value;
#'    log2err – the expected error for the standard deviation of the P-value logarithm.
#'    ES – enrichment score, same as in Broad GSEA implementation;
#'    NES – enrichment score normalized to mean enrichment of random samples of the same size;
#'    size – size of the pathway after removing genes not present in 'names(stats)'.
#'    leadingEdge – vector with indexes of leading edge genes that drive the enrichment, see http://software.broadinstitute.org/gsea/doc/GSEAUserGuideTEXT.htm#_Running_a_Leading.
#' @export
runfGSEA <- function(geneset, ranks, minSize = 15, maxSize = 500, gseaParam = 0, eps = 0.0) {

  fgseaRes_POSITIVE <- fgsea::fgseaMultilevel(
    pathways = geneset,
    stats = ranks,
    minSize = minSize,
    maxSize = maxSize,
    gseaParam = gseaParam,
    # nperm = 1000,
    eps = eps, # fgsea has a default lower bound eps=1e-10 for estimating P-values. If you need to estimate P-value more accurately, you can set the eps argument to zero
    scoreType = "pos"
  )
  # Run negative enrichment
  fgseaRes_NEGATIVE <- fgsea::fgseaMultilevel(
    pathways = geneset,
    stats = ranks,
    minSize = minSize,
    maxSize = maxSize,
    gseaParam = gseaParam,
    # nperm = 1000,
    eps = eps, # fgsea has a default lower bound eps=1e-10 for estimating P-values. If you need to estimate P-value more accurately, you can set the eps argument to zero
    scoreType = "neg"
  )

  # Combine positive and negative results + re-adjust pvals
  fgseaRes_POS_NEG <- inner_join(
    fgseaRes_POSITIVE %>%
      as_tibble(),
    fgseaRes_NEGATIVE %>%
      as_tibble(),
    by = c("pathway"),
    suffix = c("_POS", "_NEG")
  )

  fgseaRes_COMBINED <- bind_rows(
    fgseaRes_POS_NEG %>% filter(ES_POS > abs(ES_NEG)) %>% select(pathway) %>% inner_join(fgseaRes_POSITIVE, by=c("pathway")),
    fgseaRes_POS_NEG %>% filter(ES_POS < abs(ES_NEG)) %>% select(pathway) %>% inner_join(fgseaRes_NEGATIVE,by=c("pathway"))
  ) %>%
    mutate(padj = p.adjust(pval, method = "BH"))%>%
    arrange(padj, -abs(NES))

  return(fgseaRes_COMBINED)

}
