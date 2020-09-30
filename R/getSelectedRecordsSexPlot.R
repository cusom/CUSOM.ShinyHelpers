#' get Plotly stacked bar chart showing distribution by sex for group variable.
#'
#' @param .data dataframe containing group column to check
#' @param .group column, string - group value to compare percent distribution by sex
#'
#' @return returns plolty stacked bar chart.
#' @export
getSelectedRecordsSexPlot <- function(.data, .group) {

  .group <- enquo(.group)

  .data <- .data %>%
    mutate(y = !!.group)

  f <- list(
    family = "Arial",
    color = "rgb(58, 62, 65)",
    size = 12
  )

  plot_ly(.data,
          x = ~ Male,
          y = ~ y,
          type = 'bar',
          name = 'Male',
          marker = list(color = c("#BBBDC0"))
  ) %>%
    add_trace(
      x = ~ Female,
      name = 'Female',
      marker = list(color = c("#287BA5"))
    ) %>%
    layout(
      title = list(
        family="'Noto Serif', serif",
        size=18,
        color="#004F80"
      ),
      font= f,
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5
      ),
      xaxis = list(
        showgrid = F,

        title= '',
        tickformat = "%",
        range = c(0,1)
      ),
      yaxis = list(
        showgrid = F,

        type = 'category',
        categoryorder="category descending",
        title=''
      ),
      barmode = 'stack',
      shapes = list(
        list(
          type = "line",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          x0 = 0.5,
          x1 = 0.5,
          line = list(color = "red",dash="dash")
        )
      )
    )
}
