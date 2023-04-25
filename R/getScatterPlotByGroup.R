#' generate scatter plot colored by group
#' optionally add linear model fitted values trace lines per group member.
#'
#' @param .data A dataframe
#' @param key A string or numeric column - key value for dataframe.
#' @param x A numeric column for x value in scatter -
#' independent variable used in linear model fits if applicable
#' @param y A numeric column for y value in scatter -
#' response variable used in linear model fits if applicable
#' @param group A string column indicating group membership - should be binary.
#' @param groupBaselineLabel string constant -
#' indicating the base reference value for group parameter
#' @param text string column indicating value
#' to display on hover / tooltip
#' @param addFitLines whether to add fit lines to the scatter
#' plot based on groups from group parameter
#' @param plotName string indicating the name of the plot
#' @return plolty scatter plot object
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang :=
#' @import dplyr
#' @import tidyr
#' @import plotly
#' @export
getScatterPlotByGroup <- function(
  .data,
  key,
  x,
  y,
  group,
  groupBaselineLabel,
  text,
  addFitLines = TRUE,
  plotName
) {

  key <- rlang::enquo(key)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)
  text <- rlang::enquo(text)

  if (nrow(.data) > 0) {

    groups <- .data |>
      dplyr::select(!!group) |>
      dplyr::distinct() |>
      dplyr::pull()

    xRange <- c(
      round(min(.data[[rlang::quo_name(x)]])),
      round(max(.data[[rlang::quo_name(x)]])) + 1
      )

    data1 <- .data |>
      dplyr::filter(!!group == groupBaselineLabel) |>
      dplyr::mutate(
        `:=`(name, groupBaselineLabel),
        color = "#BBBDC0"
      )

    data2 <- .data |>
      dplyr::filter(!!group != groupBaselineLabel) |>
      dplyr::mutate(
        `:=`(name, groups[which(groups !=  groupBaselineLabel)]),
        color = "#287BA5"
      )

    p <- plotly::plot_ly(
      type = "scatter",
      mode = "markers"
    )

    p <- p |>
      plotly::add_trace(
        data = data1,
        type = "scatter",
        mode = "markers",
        x = x,
        y = y,
        text = ~ text,
        hoverinfo = "text",
        name = ~ name,
        legendgroup = ~ name,
        marker = list(
          color = ~ color
        )
      ) |>
      plotly::add_trace(
        data = data2,
        type = "scatter",
        mode = "markers",
        x = x,
        y = y,
        text = ~ text,
        hoverinfo = "text",
        name = ~ name,
        legendgroup = ~ name,
        marker = list(
            color = ~ color
        )
      )

    if (addFitLines) {

      lmformula <- paste(rlang::quo_name(y), " ~ ", rlang::quo_name(x))

      fit1 <- .data |>
        dplyr::select(!!x, !!y) |>
        tidyr::nest(
          data = c(!!x, !!y)
        ) |>
        dplyr::mutate(
          fit = purrr::map(
            data,
            ~stats::lm(lmformula, data = .x)$fit
          )
        ) |>
        tidyr::unnest()

      fit1CI <- .data |>
        dplyr::select(!!x, !!y) |>
        tidyr::nest(data = c(!!x, !!y)) |>
        dplyr::mutate(
          fit = purrr::map(
            data,
            ~broom::augment(
              stats::lm(lmformula, data = .x),
              se_fit = TRUE
            )
          )
        ) |>
        tidyr::unnest(fit) |>
        dplyr::mutate(
            ymin = .fitted - 1.96 * .se.fit,
            ymax = .fitted + 1.96 * .se.fit
        ) |>
        dplyr::select(ymin, ymax)

        if (length(groups) == 2) {

          fit2 <- .data |>
            dplyr::filter(!!group == groupBaselineLabel) |>
            dplyr::select(!!x, !!y) |>
            tidyr::nest(data = c(!!x, !!y)) |>
            dplyr::mutate(
              fit = purrr::map(
                data,
                ~stats::lm(lmformula, data = .x)$fit
              )
            ) |>
            tidyr::unnest() |>
            dplyr::mutate(
              `:=`(name, groupBaselineLabel),
              color = "rgb(81, 81, 81)"
            )

          fit2CI <- .data |>
            dplyr::filter(!!group == groupBaselineLabel) |>
            dplyr::select(!!x, !!y) |>
            tidyr::nest(data = c(!!x, !!y)) |>
            dplyr::mutate(
              fit = purrr::map(
                data,
                ~broom::augment(
                  stats::lm(lmformula, data = .x),
                  se_fit = TRUE
                )
              )
            ) |>
            tidyr::unnest(fit) |>
            dplyr::mutate(
              ymin = .fitted - 1.96 * .se.fit,
              ymax = .fitted + 1.96 * .se.fit
            ) |>
            dplyr::select(ymin, ymax)

          fit3 <- .data |>
            dplyr::filter(!!group != groupBaselineLabel) |>
            dplyr::select(!!x, !!y) |>
            tidyr::nest(data = c(!!x, !!y)) |>
            dplyr::mutate(
              fit = purrr::map(
                data,
                ~stats::lm(lmformula, data = .x)$fit
              )
            ) |>
            tidyr::unnest() |>
            dplyr::mutate(
              `:=`(name, groups[which(groups !=  groupBaselineLabel)]),
              color = "rgb(48, 128, 255)"
            )

          fit3CI <- .data |>
            dplyr::filter(!!group != groupBaselineLabel) |>
            dplyr::select(!!x, !!y) |>
            tidyr::nest(data = c(!!x, !!y)) |>
            dplyr::mutate(
              fitted = purrr::map(
                data,
                ~broom::augment(
                  stats::lm(lmformula, data = .x),
                  se_fit = TRUE
                )
              )
            ) |>
            tidyr::unnest(fitted) |>
            dplyr::mutate(
              ymin = .fitted - 1.96 * .se.fit,
              ymax = .fitted + 1.96 * .se.fit
            ) |>
            dplyr::select(ymin, ymax)

            p <- p |>
              plotly::add_trace(
                data = fit2,
                type = "scatter",
                x = x,
                y = ~ fit,
                mode = "lines",
                name = ~ name,
                legendgroup = ~ name,
                showlegend = FALSE,
                line = list(
                  color = ~ color,
                  width = 2
                )
              ) |>
              plotly::add_ribbons(
                x = x,
                ymin = fit2CI$ymin,
                ymax = fit2CI$ymax,
                line = list(
                  color = ~ color
                ),
                fillcolor = ~ color,
                name = "",
                legendgroup = ~ name,
                showlegend = FALSE,
                opacity = 0.3
              ) |>
              plotly::add_trace(
                data = fit3,
                type = "scatter",
                x = x,
                y = ~ fit,
                mode = "lines",
                name = ~ name,
                legendgroup = ~ name,
                showlegend = FALSE,
                line = list(
                  color = ~ color,
                  width = 2
                )
              ) |>
              plotly::add_ribbons(
                x = x,
                ymin = fit3CI$ymin,
                ymax = fit3CI$ymax,
                line = list(
                  color = ~ color
                ),
                fillcolor = ~ color,
                name = "",
                legendgroup = ~ name,
                showlegend = FALSE,
                opacity = 0.3
              )
        } else {

          p <- p |>
            plotly::add_trace(
              data = fit1,
              type = "scatter",
              x = x,
              y = ~ fit,
              mode = "lines",
              name = groups,
              legendgroup = ~ groups,
              showlegend = FALSE,
              line = list(
                color = ifelse(
                  groups == groupBaselineLabel,
                  "rgb(81, 81, 81)",
                  "rgb(48, 128, 255)"
                ),
                width = 2
                )
              ) |>
            plotly::add_ribbons(
              x = x,
              ymin = fit1CI$ymin,
              ymax = fit1CI$ymax,
              line = list(
                color = ifelse(
                  groups == groupBaselineLabel,
                  "rgb(81, 81, 81)", 
                  "rgb(48, 128, 255)"
                )
              ),
              fillcolor = ifelse(
                groups == groupBaselineLabel,
                "rgb(81, 81, 81)", 
                "rgb(48, 128, 255)"
              ),
              name = "",
              legendgroup = ~ groups,
              showlegend = FALSE,
              opacity = 0.3
            )
        }

      }

    p <- p |>
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
        xaxis = list(
          title = list(
            standoff = 10,
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
          range = xRange,
          fixedrange = FALSE
        ),
        yaxis = list(
          title = list(
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
          fixedrange = FALSE
        ),
        margin = list(
          autoexpand = TRUE,
          l = 10,
          r = 30,
          t = 30
        )
      )

    p$x$source <- paste0(plotName, "ScatterPlot")

    return(p)

  } else {

    return(NULL)

  }

}
