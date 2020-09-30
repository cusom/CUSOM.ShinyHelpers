#' Plotly function to generate standard box and whisker plot
#'
#' @param .data dataframe containing numeric values between 2 groups
#' @param .key dataframe key column
#' @param .group string - group column - should only contain 2 groups
#' @param .value numeric - numeric value to compare between groups
#' @param .valueLabel string - column indicating the label for the .value values
#' @param .text string - column containing text values to show in tooltip
#' @param plotName string - name to attribute to plot (used for tracking clicks, events, etc)
#'
#' @return returns plotly box and whisker plot
#' @export
getBoxplot <- function(.data,.key,.group,.value,.valueLabel,.text, plotName) {

  .key <- enquo(.key)
  .group <- enquo(.group)
  .value <- enquo(.value)
  .valueLabel <- enquo(.valueLabel)
  .text <- enquo(.text)

  if(nrow(.data)>0){

    .data <- .data %>%
      mutate(key = !!.key, group = !!.group, value = !!.value, text = !!.text)

    yVariableLabel <- .data %>%
      distinct(!!.valueLabel) %>%
      pull()

    f <- list(
      family = "Arial",
      color = "rgb(58, 62, 65)",
      size = 12
    )

    xaxis <- list(
      title = "",
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

    yaxis <- list(
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

    p <- plot_ly(
      data = .data,
      y = ~ value,
      color= ~ group,
      colors = c("#BBBDC0", "#287BA5"),
      key = ~ key,
      text = ~ text,
      hoverinfo = 'text',
      type = "box",
      boxpoints = "all",
      jitter = 1,
      pointpos = 0
    ) %>%
      layout(
        title = '',
        xaxis = xaxis,
        yaxis = yaxis,
        font = list(
          family="Arial",
          size= 18,
          color= "rgb(58, 62, 65)"
        ),
        margin = margin,
        showlegend = FALSE
      )

    p$x$source <- paste0(plotName,"BoxPlot")

    return(p)

  }
  else {

    return(NULL)

  }

}
