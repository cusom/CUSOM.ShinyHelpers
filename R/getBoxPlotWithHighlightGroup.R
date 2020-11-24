#' Plotly function to generate standard box and whisker plot with highighted groups
#'
#' @param .data dataframe containing numeric values between 2 groups
#' @param key dataframe key column
#' @param group string - group column - should only contain 2 groups
#' @param groupBaselineLabel string - value indicating the group label that should be used as the comparison
#' @param value numeric - numeric value to compare between groups
#' @param valueLabel string - column indicating the label for the .value values
#' @param text string - column containing text values to show in tooltip
#' @param highlightGroup string - column with highlight groups (A vs B or 1 vs 2 etc...)
#' @param colors vector of colors to use for base boxplot and jitter - defaults to grey/blue. Color used for baseline should be passed as first value
#' @param highlightColors vector of colors to use for highlighted points - defaults to organge/red
#' @param plotName string - name to attribute to plot (used for tracking clicks, events, etc)
#'
#' @return returns plotly box and whisker plot with secondary traces indicating highlighted groups
#' @export
getBoxPlotWithHighlightGroup <- function(.data, key, group, groupBaselineLabel, value, valueLabel,
                                         text, highlightGroup, colors = c("#BBBDC0","#287BA5"), highlightColors = c("organge","red"), plotName) {

  .key <- enquo(key)
  .group <- enquo(group)
  .value <- enquo(value)
  .valueLabel <- enquo(valueLabel)
  .text <- enquo(text)
  .highlightGroup <- enquo(highlightGroup)
  baselineColor <- colors[1]
  comparisonColor <- colors[2]

  if(nrow(.data)>0){

    .data <- .data %>%
      mutate(key = !!.key, group = !!.group, value = !!.value, text = !!.text) %>%
      add_tally() %>%
      mutate(x = rnorm(n, mean = ifelse(!!.group == groupBaselineLabel, -1, 1), sd=0.15))

    yVariableLabel <- .data %>%
      distinct(!!.valueLabel) %>%
      pull()

    baseline <- .data %>% filter(!!.group == groupBaselineLabel)
    comparison <- .data %>% filter(!!.group != groupBaselineLabel)

    highlightGroups <- .data %>%
      select(!!.highlightGroup) %>%
      drop_na() %>%
      unique() %>%
      pull()

    highlight_A <- .data %>% filter(!!.highlightGroup == highlightGroups[1])
    highlight_B <- .data %>% filter(!!.highlightGroup != highlightGroups[1])

    f <- list(
      family = "Arial",
      color = "rgb(58, 62, 65)",
      size = 12
    )

    x <- list(
      title = "",
      font = list (
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
      title = list (
        text = yVariableLabel,
        font = list (
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

    margin <- list(autoexpand = TRUE,
                   l = 25,
                   r = 15,
                   t = 20,
                   b = 20)

    if ( median(baseline$value) < median(comparison$value) ) {
      plotColors <- c(baselineColor, comparisonColor)
    }
    else {
      plotColors <- c(comparisonColor, baselineColor)
    }

    p <- plot_ly(type='box', colors = plotColors) %>%
      add_boxplot(y = baseline$value,
                  x = -1,
                  type = "box",
                  boxpoints = FALSE,
                  name = baseline$group,
                  color = baseline$group
      ) %>%
      add_boxplot(y = comparison$value,
                  x = 1,
                  type = "box",
                  boxpoints = FALSE,
                  name= comparison$group,
                  color = comparison$group
      ) %>%
      add_markers(y = baseline$value,
                  text = baseline$text,
                  hoverinfo = 'text',
                  key = baseline$key,
                  x = baseline$x,
                  marker = list(
                    color = baselineColor,
                    size = 8
                  ),
                  showlegend = FALSE
      ) %>%
      add_markers(y = comparison$value,
                  text = comparison$text,
                  hoverinfo = 'text',
                  key = comparison$key,
                  x = comparison$x,
                  marker = list(
                    color = comparisonColor,
                    size = 8
                  ),
                  showlegend = FALSE
      )

    if(nrow(highlight_A)>0){

      p <- p %>%
        add_markers(y = highlight_A$value,
                    text = highlight_A$text,
                    hoverinfo = 'text',
                    x = highlight_A$x,
                    marker = list(
                      color = highlightColors[1],
                      size = 8
                    ),
                    showlegend = TRUE,
                    name = "Group A"
        )
    }

    if(nrow(highlight_B)>0){

      p <- p %>%
        add_markers(y = highlight_B$value,
                    text =  highlight_B$text,
                    hoverinfo = 'text',
                    x = highlight_B$x,
                    marker = list(
                      color = highlightColors[2],
                      size = 8
                    ),
                    showlegend = TRUE,
                    name = "Group B"
        )
    }

    p <- p %>%
      layout(
        title = '',
        xaxis = x,
        yaxis = y,
        font = list(
          family="Arial",
          size= 18,
          color= "rgb(58, 62, 65)"
        ),
        margin = margin,
        showlegend = TRUE
      )

    p$x$source <- paste0(plotName,"BoxPlot")

    return(p)

  } else {

    return(NULL)

  }

}
