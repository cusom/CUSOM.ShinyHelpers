#' get Plotly box and whisker plot showing age distribution by group
#'
#' @param .data dataframe containing group column to check
#' @param .group column, string - group value to compare percent
#' distribution by sex
#' @param .value column, numeric - age column to show distribution across groups
#' @return returns plolty box and whisker plot
#' @importFrom rlang enquo
#' @import dplyr
#' @import plotly
#' @export
getSelectedRecordsAgePlot <- function(
  .data,
  .group,
  .value
) {

  .group <- rlang::enquo(.group)
  .value <- rlang::enquo(.value)

  if (nrow(.data) > 0) {

    .data <- .data |>
      dplyr::mutate(
        y = !!.value,
        color = !!.group,
        text = paste0(!!.value)
      )

    f <- list(
      family = "Arial",
      color = "rgb(58, 62, 65)",
      size = 12
    )

    xaxis <- list(
      title = "",
      titlefont = f,
      showgrid = FALSE,
      zeroline = TRUE,
      showline = FALSE,
      showticklabels = TRUE
    )

    yaxis <- list(
      title = "",
      titlefont = f,
      showgrid = FALSE,
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = TRUE
    )

    p <- plotly::plot_ly(
      data = .data,
      y = ~ y,
      color= ~ color,
      colors = c("#BBBDC0", "#287BA5"),
      text = ~ text,
      hoverinfo = "text",
      type = "box",
      boxpoints = "all",
      jitter = 1,
      pointpos = 0
    ) |>
      plotly::layout(
        font = f,
        xaxis = xaxis,
        yaxis = yaxis,
        showlegend = FALSE
      )

    return(p)

  } else {
    return(NULL)
  }

}
