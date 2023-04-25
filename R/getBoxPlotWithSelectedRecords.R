#' Plotly function to generate standard box and
#' whisker plot with highighted / selected points
#'
#' @param .data dataframe containing numeric values between 2 groups
#' @param .key dataframe key column
#' @param .group string - group column - should only contain 2 groups
#' @param .groupBaselineLabel string - value indicating the
#' group label that should be used as the comparison
#' @param .value numeric - numeric value to compare between groups
#' @param .valueLabel string - column indicating the label for the .value values
#' @param .text string - column containing text values to show in tooltip
#' @param .selectedIndicator boolean - column indicating whether
#' the row has been selected via click or selected action
#' @param plotName string - name to attribute to plot
#' (used for tracking clicks, events, etc)
#'
#' @return returns plotly box and whisker plot
#' @importFrom rlang enquo
#' @import dplyr
#' @import plotly
#' @importFrom stats rnorm
#' @export
getBoxPlotWithSelectedRecords <- function(
  .data,
  .key,
  .group,
  .groupBaselineLabel,
  .value,
  .valueLabel,
  .text,
  .selectedIndicator,
  plotName
  ) {

  .key <- rlang::enquo(.key)
  .group <- rlang::enquo(.group)
  .value <- rlang::enquo(.value)
  .valueLabel <- rlang::enquo(.valueLabel)
  .text <- rlang::enquo(.text)
  .selectedIndicator <- rlang::enquo(.selectedIndicator)

  if (nrow(.data) > 0) {

    yVariableLabel <- .data |>
      dplyr::distinct(!!.valueLabel) |>
      dplyr::pull()

    baseline <- .data |>
      dplyr::filter(!!.group == .groupBaselineLabel) |>
      dplyr::mutate(
        key = !!.key,
        group = !!.group,
        value = !!.value,
        text = !!.text
      ) |>
      dplyr::add_tally() |>
      dplyr::mutate(
        x = stats::rnorm(
          n,
          mean = -1,
          sd = 0.15
        )
      )

    baseline_selected <- baseline |>
      dplyr::filter(!!.selectedIndicator == 1)

    comparison <- .data |>
      dplyr::filter(!!.group != .groupBaselineLabel) |>
      dplyr::mutate(
        key = !!.key,
        group = !!.group,
        value = !!.value,
        text = !!.text
      ) |>
      dplyr::add_tally() |>
      dplyr::mutate(
        x = stats::rnorm(
          n,
          mean = 1,
          sd = 0.15
        )
      )

    comparison_selected <- comparison |>
      dplyr::filter(!!.selectedIndicator == 1)

    x <- list(
      title = "",
      font = list(
        family = "Arial",
        color = "rgb(58, 62, 65)",
        size = 18
      ),
      showgrid = FALSE,
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = FALSE
    )

    y <- list(
      title = list(
        text = yVariableLabel,
        font = list(
          family = "Arial",
          size = 18
        )
      ),
      font = list (
        family = "Arial",
        color = "rgb(58, 62, 65)",
        size = 18
      ),
      showgrid = FALSE,
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = TRUE
    )

    margin <- list(
      autoexpand = TRUE,
      l = 25,
      r = 15,
      t = 20,
      b = 20
    )

    p <- plotly::plot_ly(
        type = "box",
        colors = c("#BBBDC0", "#287BA5")
      ) |>
      plotly::add_markers(
        y = baseline$value,
        x = -1,
        type = "box",
        boxpoints = FALSE,
        name = baseline$group,
        color = baseline$group,
        colors = c("#BBBDC0", "#287BA5")
      ) |>
      plotly::add_markers(
        y = baseline$value,
        text = baseline$text,
        hoverinfo = "text",
        key = baseline$key,
        x = baseline$x,
        marker = list(
          color = "#BBBDC0",
          size = 8
        ),
        showlegend = FALSE
      ) |>
      plotly::add_boxplot(
        y = comparison$value,
        x = 1,
        type = "box",
        boxpoints = FALSE,
        name = comparison$group,
        color = comparison$group,
        colors = c("#BBBDC0", "#287BA5")
      ) |>
      plotly::add_markers(
        y = comparison$value,
        text = comparison$text,
        hoverinfo = "text",
        key = comparison$key,
        x = comparison$x,
        marker = list(
          color = "#287BA5",
          size = 8
        ),
        showlegend = FALSE
      )

    if (nrow(baseline_selected) > 0) {
      p <- p |>
        plotly::add_markers(
          y = baseline_selected$value,
          text = baseline_selected$text,
          hoverinfo = "text",
          x = baseline_selected$x,
          marker = list(
            color = "orange",
            size = 8
          ),
          showlegend = FALSE
        )
    }

    if (nrow(comparison_selected) > 0) {
      p <- p |>
        plotly::add_markers(
          y = comparison_selected$value,
          text =  comparison_selected$text,
          hoverinfo = "text",
          x = comparison_selected$x,
          marker = list(
            color = "red",
             size = 8
          ),
          showlegend = FALSE
        )
    }

    p <- p |>
      plotly::layout(
        title = "",
        xaxis = x,
        yaxis = y,
        font = list(
          family = "Arial",
          size = 18,
          color = "rgb(58, 62, 65)"
        ),
        margin = margin,
        showlegend = TRUE
      )

    p$x$source <- paste0(plotName, "BoxPlot")

    return(p)

  } else {

    return(NULL)

  }

}
