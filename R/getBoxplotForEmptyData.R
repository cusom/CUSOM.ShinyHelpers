#' Utility Plotly function to return blank plot when useful
#'
#' @param text string defaults to "" - optional text to display in plot
#
#' @return returns empty Plotly scatter plot
#' @export

getBoxplotForEmptyData <- function(text="") {

  data <- as_tibble(list(x = 1, text = text))

  xaxis <- list(
    title = "",
    showgrid = FALSE,
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    fixedrange = TRUE
  )

  yaxis <- list(
    title = "",
    showgrid = FALSE,
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    fixedrange = TRUE
  )

  margin <- list(
    autoexpand = TRUE,
    l = 25,
    r = 15,
    t = 20,
    b = 20
  )

  plot_ly(
    data = data,
    x = ~x,
    y = ~x,
    text = ~ text,
    hoverinfo = 'none',
    type = "scatter",
    mode = "none",
    colors = "white",
    marker = list(
      color = "white"
    )
  ) %>%
    layout(
      title = '',
      xaxis = xaxis,
      yaxis = yaxis,
      margin = margin,
      showlegend = FALSE,
      annotations = list(
        x = 1,
        y = 1,
        text = ~text,
        xref = "x",
        yref = "y",
        showarrow = FALSE,
        arrowhead = 7,
        ax = 0,
        ay = 0,
        font = list(
          color = '#264E86',
          family = 'sans serif',
          size = 32
        )
      )
    ) %>% config(
      displayModeBar = FALSE,
      displaylogo = FALSE
    )
}
