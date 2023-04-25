#' Plotly function to generate standard box and 
#' whisker plot with highighted groups
#'
#' @param .data dataframe containing numeric values between 2 groups
#' @param key dataframe key column
#' @param group string - group column - should only contain 2 groups
#' @param groupBaselineLabel string - value indicating the
#' group label that should be used as the comparison
#' @param value numeric - numeric value to compare between groups
#' @param valueLabel string - column indicating the label for the .value values
#' @param text string - column containing text values to show in tooltip
#' @param highlightGroup string - column with highlight
#' groups (A vs B or 1 vs 2 etc...)
#' @param colors vector of colors to use for base boxplot and
#' jitter - defaults to grey/blue. Color used for baseline
#' should be passed as first value
#' @param highlightColors vector of colors to use for
#' highlighted points - defaults to orange/red
#' @param plotName string - name to attribute to plot
#' (used for tracking clicks, events, etc)
#'
#' @return returns plotly box and whisker plot with
#' secondary traces indicating highlighted groups
#' @importFrom rlang enquo
#' @import dplyr
#' @import plotly
#' @importFrom stats rnorm
#' @export
getBoxPlotWithHighlightGroup <- function(
  .data,
  key,
  group,
  groupBaselineLabel,
  value,
  valueLabel,
  text,
  highlightGroup,
  colors = c("#BBBDC0", "#287BA5"),
  highlightColors = c("orange", "red"),
  plotName
  ) {

  .key <- rlang::enquo(key)
  .group <- rlang::enquo(group)
  .value <- rlang::enquo(value)
  .valueLabel <- rlang::enquo(valueLabel)
  .text <- rlang::enquo(text)
  .highlightGroup <- rlang::enquo(highlightGroup)
  baselineColor <- colors[1]
  comparisonColor <- colors[2]

  if (nrow(.data) > 0) {

    .data <- .data |>
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
          mean = ifelse(!!.group == groupBaselineLabel, -1, 1
        ),
        sd = 0.15)
      )

    yVariableLabel <- .data |>
      dplyr::distinct(!!.valueLabel) |>
      dplyr::pull()

    baseline <- .data |>
      dplyr::filter(!!.group == groupBaselineLabel)

    comparison <- .data |>
      dplyr::filter(!!.group != groupBaselineLabel)

    highlightGroups <- .data |>
      dplyr::select(!!.highlightGroup) |>
      tidyr::drop_na() |>
      dplyr::distinct() |>
      dplyr::pull()

    highlight_A_baseline <- .data |>
      dplyr::filter(
        !!.group == groupBaselineLabel,
        !!.highlightGroup == highlightGroups[1]
      )

    highlight_A_comparison <- .data |>
      dplyr::filter(
        !!.group != groupBaselineLabel,
        !!.highlightGroup == highlightGroups[1]
      )

    highlight_B_baseline <- .data |>
      dplyr::filter(
        !!.group == groupBaselineLabel,
        !!.highlightGroup != highlightGroups[1]
      )

    highlight_B_comparison <- .data |>
      dplyr::filter(
        !!.group != groupBaselineLabel,
        !!.highlightGroup != highlightGroups[1]
      )

    p1 <- plotly::plot_ly(
      type = "box",
      colors = baselineColor
      ) |>
      plotly::add_boxplot(
        y = baseline$value,
        x = -1,
        type = "box",
        boxpoints = FALSE,
        name = baseline$group,
        color = baseline$group,
        legendgroup="baseline"
      ) |>
      plotly::add_markers(
        y = baseline$value,
        text = baseline$text,
        hoverinfo = "text",
        key = baseline$key,
        x = baseline$x,
        marker = list(
          color = baselineColor,
          size = 8
        ), 
        showlegend = FALSE,
        legendgroup="baseline"
      )

    if (nrow(highlight_A_baseline) > 0) {
      p1 <- p1 |>
        plotly::add_markers(
          y = highlight_A_baseline$value,
          text = highlight_A_baseline$text,
          hoverinfo = "text",
          x = highlight_A_baseline$x,
          marker = list(
            color = highlightColors[1],
            size = 8
          ),
          showlegend = TRUE,
          name = "Group A",
          legendgroup = "groupA"
        )
    }

    if (nrow(highlight_B_baseline) > 0) {
      p1 <- p1 |>
        plotly::add_markers(
          y = highlight_B_baseline$value,
          text = highlight_B_baseline$text,
          hoverinfo = "text", 
          x = highlight_B_baseline$x,
          marker = list(
            color = highlightColors[2],
            size = 8
          ),
          showlegend = TRUE,
          name = "Group B",
          legendgroup = "groupB"
        )
    }

    p1 <- p1 |>
      plotly::layout(
        showlegend = TRUE,
        legendgroup = "baseline",
        title = "",
        xaxis = list(
          title = list(
            text = "",
            standoff = 0,
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          tickfont = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 10
          ),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = FALSE,
          fixedrange = TRUE
        ),
        yaxis = list(
          title = list(
            text = yVariableLabel,
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          tickfont = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 10
          ),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          fixedrange = TRUE
        )
      )

    p2 <- plotly::plot_ly(
      type = "box", 
      colors = comparisonColor
      ) |>
      plotly::add_boxplot(
        y = comparison$value,
        x = 1,
        type = "box",
        boxpoints = FALSE,
        name = comparison$group,
        color = comparison$group,
        legendgroup = "comparison"
      ) |>
      plotly::add_markers(
        y = comparison$value,
        text = comparison$text,
        hoverinfo = "text",
        key = comparison$key,
        x = comparison$x,
        marker = list(
          color = comparisonColor,
          size = 8
        ),
        showlegend = FALSE,
        legendgroup = "comparison"
      )

    if (nrow(highlight_A_comparison) > 0) {
      showLegend <- ifelse(nrow(highlight_A_baseline) > 0, FALSE, TRUE)
      p2 <- p2 |>
        plotly::add_markers(
          y = highlight_A_comparison$value,
          text = highlight_A_comparison$text,
          hoverinfo = "text",
          x = highlight_A_comparison$x,
          marker = list(
            color = highlightColors[1],
            size = 8
          ),
          showlegend = showLegend,
          name = "Group A",
          legendgroup = "groupA"
        )
    }

    if (nrow(highlight_B_comparison) > 0) {
      showLegend <- ifelse(nrow(highlight_B_baseline) > 0, FALSE, TRUE)
      p2 <- p2 |>
        plotly::add_markers(
          y = highlight_B_comparison$value,
          text = highlight_B_comparison$text,
          hoverinfo = "text",
          x = highlight_B_comparison$x,
          marker = list(
            color = highlightColors[2],
            size = 8
          ),
          showlegend = showLegend,
          name = "Group B",
          legendgroup = "groupB"
        )
    }

    p2 <- p2 |>
      plotly::layout(
        showlegend = TRUE,
        legendgroup = "comparison",
        title = "",
        xaxis = list(
          title = list(
            text = "",
            standoff = 0,
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          tickfont = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 10
          ),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = FALSE,
          fixedrange = TRUE
        ),
        yaxis = list(
          title = list(
            text = yVariableLabel,
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          tickfont = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 10
          ),
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          fixedrange = TRUE
        )
      )

    p <- plotly::subplot(
        p1,
        p2,
        shareX = TRUE,
        shareY = TRUE,
        margin = 0.0
      ) |>
      plotly::layout(
        showlegend = TRUE,
        legend = list(
          title = list(
            text = "",
            font = list(
              family = "Arial",
              color = "rgb(58, 62, 65)",
              size = 14
            )
          ),
          font = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 14
          )
        ),
        title = list(
          font = list(
            family = "Arial",
            color = "rgb(58, 62, 65)",
            size = 18
          ),
          pad = list(
            t = 10,
            l = 5
          ),
          x = 0,
          xanchor = "left",
          xref = "container",
          y = 1
        ),
        margin = list(
          autoexpand = TRUE,
          l = 25,
          r = 15,
          t = 20,
          b = 20
        )
      )

    p$x$source <- paste0(plotName, "BoxPlot")

    return(p)

  } else {

    return(NULL)

  }

}
