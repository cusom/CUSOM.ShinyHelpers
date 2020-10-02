#' Plotly function to generate multiple box and whisker plots comparing group across secondary grouping level
#'
#' @param .data dataframe containing numeric values between 2 groups, across secondary grouping level
#' @param .key dataframe key column
#' @param .group string - column indicating highlighted group membership (A vs B)
#' @param .legendGroup string - column indicating the secondary group label
#' @param .value numeric - column with numerical value to compare between groups
#' @param .valueLabel string - column indicating the label for numerical values used in boxplots
#' @param .text string - column containing text values to show in tooltip
#' @param showJitter boolean - show jittered points?
#' @param plotName string - name to attribute to plot (used for tracking clicks, events, etc)
#'
#' @return returns subplot 1 x 2 plotly box and whisker plot
#' @export


getGroupedBoxplot <- function(.data,.key,.group,.legendGroup,.value,.valueLabel,.text, showJitter = TRUE, plotName) {
  # key = RecordID
  # x = celltype
  # y = MeasuredValue
  # legendGroup = Karyotype
  # color = Karytoye

  .key <- enquo(.key)
  .group <- enquo(.group)
  .legendGroup <- enquo(.legendGroup)
  .value <- enquo(.value)
  .valueLabel <- enquo(.valueLabel)
  .text <- enquo(.text)

  if(nrow(.data)>0){

    .data <- .data %>%
      mutate(key = !!.key, group = !!.group, legendGroup = !!.legendGroup, value = !!.value, text = !!.text)

    yVariableLabel <- .data %>%
      distinct(!!.valueLabel) %>%
      pull()

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

    margin <- list(autoexpand = TRUE,
                   l = 100,
                   r = 100,
                   t = 100,
                   b = 50)

    p <- plot_ly(
      data = .data,
      x = ~ group,
      y = ~ value,
      legendgroup = ~ legendGroup,
      color = ~ legendGroup,
      type = "box",
      text = ~ text,
      hoverinfo = 'text',
      colors = c("#BBBDC0", "#287BA5"),
      boxpoints = ifelse(showJitter,"all","none"),
      pointpos = 0
    ) %>%
      layout(
        showlegend = TRUE,
        boxmode = "group",
        title = list(
          x = 0.05,
          font=list(
            family="Noto Serif', serif",
            size= 24,
            color="rgb(0, 79, 128)"
          )
        ),
        xaxis = x,
        yaxis = y,
        font = f,
        margin = margin,
        font=list(
          family="Noto Serif', serif",
          size=30,
          color="#004F80"
        )
      )

    p$x$source <- plotName

    return(p)

  }

  else {

    return(NULL)

  }

}
