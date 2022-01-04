#' Plotly function to generate standard volcano plot from fold change data
#'
#' @param .data dataframe containing fold change data
#' @param foldChangeVariable fold change variable
#' @param significanceVariable p value variable
#' @param significanceGroup significance group column - each group results in a separate trace
#' @param text column containing text values to show in tooltip
#' @param key - key column in fold change data - will be used for capuring select events
#' @param plotName - name to attribute to plot (used for tracking clicks, events, etc)
#' @param color - column with color to use for each row / group in dataset - should be unique per significance group / trace
#' @param shape - optional shape - should be passed as a nammed column in source data. 
#' @return returns Plotly scatter plot showing fold change vs p value colored by significance group
#' @export
getVolcanoPlot <- function(.data, foldChangeVariable, significanceVariable, significanceGroup, text, key, plotName, color, shape = "circle") {

  foldChangeVariable <- enquo(foldChangeVariable)
  significanceVariable <- enquo(significanceVariable)
  significanceGroup <- enquo(significanceGroup)
  text <- enquo(text)
  key <- enquo(key)
  color <- enquo(color)
  shape <- enquo(shape)

  maxFoldChange <- getMaxAbsValue(.data, !!foldChangeVariable, inf.rm = TRUE, buffer = 1.1)

  maxPValue <- getMaxAbsValue(.data, !!significanceVariable, inf.rm = TRUE,buffer = 1.1) 

  if (maxPValue < 5) {
    maxPValue <- 5
  }

  f <- list(
    family = "Arial",
    color = "rgb(58, 62, 65)",
    size = 12
  )

  x <- list(
    title = list(
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
    zeroline = TRUE,
    showline = FALSE,
    showticklabels = TRUE,
    range = c(-maxFoldChange,maxFoldChange)
  )

  y <- list(
    title = list (

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
    zeroline = TRUE,
    showline = TRUE,
    showticklabels = TRUE,
    range = c(0,maxPValue)
  )

  margin <- list(
    autoexpand = TRUE,
    l = 10,
    r = 30,
    t = 30
  )

  unselectedOpacity <- ifelse(nrow(.data %>% filter(selectedPoint==1)) == 0,1.0,0.7)

  groups <- .data %>% 
    select(!!significanceGroup, !!shape) %>% 
    distinct() %>%
    mutate(
      sortOrder = case_when(
        str_detect(significanceGroup,"down") ~ 1,
        str_detect(significanceGroup,"up") ~ 999,
        TRUE ~ 500
      )
    ) %>%
    arrange(sortOrder)

  p <- plot_ly()

  for(i in 1:nrow(groups)) {
        
    i_group = as.character(groups[i,1])
    i_shape = as.character(groups[i,2])

    df <- .data %>% 
      filter(
        !!significanceGroup == i_group, 
        !!shape == i_shape
      )

    selectedIndex <- ifelse(nrow( df %>% filter(selectedPoint==1) ) > 0, which(df$selectedPoint==1) - 1, -1)
        
    groupColor <- df %>%
        select(!!color) %>%
        unique() %>%
        pull()
    
    p <- p %>% 
      add_trace(
        data = df, 
        type = "scatter", 
        x = foldChangeVariable, 
        y = significanceVariable, 
        name = i_group,
        text = text, 
        hoverinfo = "text", 
        mode = "markers", 
        color = significanceGroup, 
        colors = groupColor, 
        key = key, 
        marker = list(
          symbol = shape, 
          size = 8, 
          width = 2,
          color = groupColor 
        ), 
        selectedpoints = list(
          selectedIndex
        ), 
        selected = list(
          marker = list(
            color = "#ff0000", 
            opacity = 1, 
            size = 14
          )
        ), 
        unselected = list(
          marker = list(
            color = groupColor, 
            opacity = unselectedOpacity, 
            size = 8
          )
        ), 
        showlegend = TRUE
      ) 
  }
  
  p <- p %>% 
    layout(
      title = list(
        text = "", 
        font = list(
          family = "Arial", 
          size = 24, 
          color = "#004F80"
        )
      ), 
      showlegend = TRUE, 
      legend = list(
        x = 100, 
        y = 0.1
      ), 
      xaxis = x, 
      yaxis = y, 
      margin = margin, 
      font = list(
        family = "Arial", 
        size = 18, color = "rgb(58, 62, 65)"
      )
    )
  
  p$x$source <- paste0(plotName, "VolcanoPlot")
  
  p

}