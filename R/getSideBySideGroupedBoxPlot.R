#' Plotly function to generate side-by-side box and whisker plots
#' comparing groups by highlighted group
#'
#' @param .data dataframe containing numeric values between 2 groups
#' @param .key dataframe key column
#' @param .group string - column indicating highlighted group
#' membership (A vs B)
#' @param .comparisonGroup string - column indicating the group
#' label that should be used as the comparison
#' @param .comparisonBaseline string - value indicating which of the comparison
#' group values should be used as comparison
#' @param .value numeric - column with numerical value to compare
#' between groups
#' @param .valueLabel string - column indicating the label for numerical
#' values used in boxplots
#' @param .text string - column containing text values to show in tooltip
#' @param showJitter boolean - show jittered points?
#' @param plotName string - name to attribute to plot
#' (used for tracking clicks, events, etc)
#' @return returns subplot 1 x 2 plotly box and whisker plot
#' @importFrom rlang enquo
#' @import dplyr
#' @import plotly
#' @export


getSideBySideGroupedBoxplot <- function(
  .data,
  .key,
  .group,
  .comparisonGroup,
  .comparisonBaseline,
  .value,
  .valueLabel,
  .text,
  showJitter = TRUE,
  plotName
) {

  .key <- rlang::enquo(.key) #labId
  .group <- rlang::enquo(.group) # highight group A vs B
  .comparisonGroup <- rlang::enquo(.comparisonGroup) # Analyte
  .comparisonBaseline <- rlang::enquo(.comparisonBaseline) # 1st analyte
  .value <- rlang::enquo(.value) # measuredValue
  .valueLabel <- rlang::enquo(.valueLabel) # concentration
  .text <- rlang::enquo(.text)

  if (nrow(.data) > 0) {

    .data <- .data |>
      dplyr::mutate(
        key = !!.key,
        group = !!.group,
        comparisonGroup = !!.comparisonGroup,
        value = !!.value,
        text = !!.text
      )

    yVariableLabel <- .data |>
      dplyr::distinct(!!.valueLabel) |>
      dplyr::pull()

    f <- list(
      family = "Arial",
      color = "rgb(58, 62, 65)",
      size = 18
    )

    x <- list(
      title = "",
      titlefont = f,
      showgrid = FALSE,
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = TRUE
    )

    y <- list(
      title = yVariableLabel,
      titlefont = f,
      showgrid = FALSE,
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = TRUE
    )

    margin <- list(
      autoexpand = TRUE,
      l = 100,
      r = 100,
      t = 100,
      b = 50
    )

    d1 <- .data |>
      dplyr::filter(comparisonGroup == !!.comparisonBaseline)

    baselineLabel <- d1 |>
      dplyr::select(comparisonGroup) |>
      dplyr::distinct() |>
      dplyr::pull()

    d2 <- .data |>
      dplyr::filter(comparisonGroup != !!.comparisonBaseline)

    comparisonLabel <- d2 |>
      dplyr::select(comparisonGroup) |>
      dplyr::distinct() |>
      dplyr::pull()

    if (nrow(d1) > 0 && nrow(d2) > 0) {

      p1 <- plotly::plot_ly(
        data = d1,
        x = ~ group,
        y = ~ value,
        color = ~ group,
        type = "box",
        text = ~ text,
        hoverinfo = "text",
        colors = c("#BBBDC0", "#287BA5"),
        boxpoints = ifelse(showJitter, "all", "none"),
        jitter = 1,
        pointpos = 0
      ) |>
      plotly::layout(
        showlegend = FALSE,
        xaxis = x,
        yaxis = y,
        font = f,
        margin = margin,
        font = list(
          family = "Noto Serif', serif",
          size = 30,
          color = "#004F80"
        )
      )

      p2 <- plotly::plot_ly(
        data = d2,
        x = ~ group,
        y = ~ value,
        color = ~ group,
        type = "box",
        text = ~ text,
        hoverinfo = "text",
        colors = c("#BBBDC0", "#287BA5"),
        boxpoints = ifelse(showJitter, "all", "none"),
        jitter = 1,
        pointpos = 0
      ) |>
      plotly::layout(
        showlegend = FALSE,
        xaxis = x,
        yaxis = y,
        font = f,
        margin = margin,
        font = list(
          family = "Noto Serif', serif",
          size = 30,
          color = "#004F80"
        )
      )

      p <- plotly::subplot(
        p1,
        p2,
        titleX = TRUE
      ) |>
      plotly::layout(
        xaxis = list(
          title = baselineLabel,
          fixedrange = TRUE
        ),
        xaxis2 = list(
          title = comparisonLabel,
          fixedrange = TRUE
        ),
        yaxis = list(
          title = yVariableLabel,
          fixedrange = TRUE
        ),
        yaxis2 = list(
          title = "",
          fixedrange = TRUE
        )
      )

      p$x$source <- plotName

      return(p)

    } else {

      return(NULL)

    }

  } else {

    return(NULL)

  }

}
