#' generate scatter plot colored by group - optionally add linear model fitted values trace lines per group member.
#'
#' @param .data A dataframe
#' @param key A string or numeric column - key value for dataframe.
#' @param x A numeric column for x value in scatter - independent variable used in linear model fits if applicable
#' @param y A numeric column for y value in scatter - response variable used in linear model fits if applicable
#' @param group A string column indicating group membership - should be binary.
#' @param groupBaselineLabel string constant - indicating the base reference value for group parameter
#' @param text string column indicating value to display on hover / tooltip
#' @param addFitLines whether to add fit lines to the scatter plot based on groups from group parameter
#' @param plotName string indicating the name of the plot
#' @return plolty scatter plot object

#' @export
getScatterPlotByGroup <- function (.data, key, x, y, group, groupBaselineLabel, text, addFitLines = TRUE, plotName) {

  key <- enquo(key)
  x <- enquo(x)
  y <- enquo(y)
  group <- enquo(group)
  text <- enquo(text)

  if (nrow(.data) > 0) {

    groups <- .data %>% select(!!group) %>% unique() %>% pull()

    xRange <- c(round(min(.data[[quo_name(x)]])),round(max(.data[[quo_name(x)]]))+1)

    xaxis <- list(
      title = list(
        font = list(
          family = "Arial",
          size = 18
        )
      ),
      font = list(
        family = "Arial",
        color = "rgb(58, 62, 65)",
        size = 18
      ),
      showgrid = FALSE,
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = TRUE,
      range = xRange
    )

    yaxis <- list(
      title = list(
        font = list(
          family = "Arial",
          size = 18
        )
      ),
      font = list(
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
      l = 10,
      r = 30,
      t = 30
    )


    p <- plot_ly() %>%
      add_trace(
        data = .data,
        type="scatter",
        x = x,
        y = y,
        color= group,
        colors = c("#BBBDC0", "#287BA5"),
        text = text,
        hoverinfo = "text",
        mode = "markers"
      )

    if(addFitLines) {

      lmformula <- paste(quo_name(y), " ~ ", quo_name(x))

      fit1 <- .data %>%
        select(!!x,!!y) %>%
        nest(data = c(!!x, !!y)) %>%
        mutate(
          fit = map(data, ~lm(lmformula,data = .x)$fit)
        ) %>%
        unnest()

      if(length(groups)==2) {

        fit2 <- .data %>%
          filter(!!group==groupBaselineLabel) %>%
          select(!!x,!!y) %>%
          nest(data = c(!!x, !!y)) %>%
          mutate(
            fit = map(data, ~lm(lmformula,data = .x)$fit)
          ) %>%
          unnest() %>%
          mutate(name := groupBaselineLabel,
                 color = 'rgb(81, 81, 81)')

        fit3 <- .data %>%
          filter(!!group!=groupBaselineLabel) %>%
          select(!!x,!!y) %>%
          nest(data = c(!!x, !!y)) %>%
          mutate(
            fit = map(data, ~lm(lmformula,data = .x)$fit)
          ) %>%
          unnest() %>%
          mutate(name := groups[which(groups != groupBaselineLabel)],
                 color = 'rgb(48, 128, 255)')

        p <- p %>%
          add_trace(
            data = fit1,
            type="scatter",
            x = x,
            y = ~fit,
            mode = "lines",
            name="All",
            line = list(
              color = 'rgb(205, 12, 24)',
              width = 2
              )
            ) %>%
          add_trace(
            data = fit2,
            type="scatter",
            x = x,
            y = ~fit,
            mode = "lines",
            name= ~name,
            line = list(
              color = ~color,
              width = 2
              )
            ) %>%
          add_trace(
            data = fit3,
            type="scatter",
            x = x,
            y = ~fit,
            mode = "lines",
            name= ~name,
            line = list(
              color = ~color,
              width = 2
              )
            )

      }

      else {

        p <- p %>%
          add_trace(
            data = fit1,
            type="scatter",
            x = x,
            y = ~fit,
            mode = "lines",
            name= groups,
            line = list(
              color = ifelse(groups==groupBaselineLabel,"rgb(81, 81, 81)","rgb(48, 128, 255)"),
              width = 2
              )
            )

      }

    }

    p <- p %>%
      layout(
        xaxis = xaxis,
        yaxis = yaxis,
        margin = margin
      )

    p$x$source <- paste0(plotName, "ScatterPlot")

    return(p)
  }

  else {

    return(NULL)

  }

}
