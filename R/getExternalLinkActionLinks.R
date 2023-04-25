#' Utility function to create standard external site links for a given analyte
#'
#' @param analyte_name string - name of analyte to inject into links
#' @param ns namespace for module
#'
#' @return shiny div containing shiny action button objects
#' @import dplyr
#' @importFrom shiny icon
#' @importFrom shiny tagList
#' @importFrom shiny actionLink
#' @export
getExternalLinkActionLinks <- function(analyte_name, ns) {
  external_links <- tibble::tibble(
    name = c("Pubmed", "GeneCards", "GTEx", "NCBI", "Wikipedia"),
    url = c(
      "https://www.ncbi.nlm.nih.gov/pubmed/?term=",
      "https://www.genecards.org/Search/Keyword?queryString=",
      "https://www.gtexportal.org/home/gene/",
      "https://www.ncbi.nlm.nih.gov/gene/?term=",
      "https://en.wikipedia.org/w/index.php?search="
    )
  )

  sapply(as.list(external_links$name), function(x) {
    url <- as.character(external_links[which(external_links$name == x), "url"])
    js <- glue::glue("window.open('{url}{analyte_name}',target = '_blank')")
    return(
      shiny::tagList(
        shiny::actionLink(
          inputId = ns(x),
          label = x,
          icon = shiny::icon("external-link-alt"),
          class = "action-button-externallink",
          onclick = js
        ),
        htmltools::tags$br()
      )
    )
  })
}
