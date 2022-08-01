#' Generate a plotly-based GSEA Enrichment plot 
#'
#' @param pathName GSEA Pathway Name (Gene Set Name)
#' @param stats GSEA ranked data ()
#' @param res GSEA scored data (Gene.set, Size, ES, NES, p.value, q.value, gene set) 
#' @param title string plot title 
#' @param gseaParam int GSEA parameter value, all gene-level statis are raised to the power of 'gseaParam' before calculation of GSEA enrichment scores.
#' @return plotly plot object
#' @export 
plotGSEAEnrichment <- function (pathName, stats, res, title = "", gseaParam=0) {

  ticksSize = 0.4
  
  pathwayNammed <- res %>% 
    filter(Gene.set == pathName) %>% 
    select(Leading.edge.genes) %>%
    pull() %>%
    str_split(.,",") %>%
    simplify() 

  label = paste0(
    "NES = ", (res %>% filter(Gene.set == pathName))$NES %>% round(2),
    "\n",
    "Q = ", (res %>% filter(Gene.set == pathName))$q.value %>% rstatix::p_format()
  )
  x_label <- length(stats)*0.99
  y_label <- ((res %>% filter(Gene.set == pathName))$ES)*0.95

  rnk <- rank(-stats) # rank highest values first
  ord <- order(rnk) # get correct order
  statsAdj <- stats[ord] # ensure ranked list is ordered correctly
  statsAdj <- sign(statsAdj) * (abs(statsAdj)^gseaParam) # gets sign and multiplies by absolute value ^ gsea param
  statsAdj <- statsAdj/max(abs(statsAdj))
  zero_cross <- statsAdj[statsAdj > 0] %>% length() # New; get Zero crossing point
  pathway <- unname(as.vector(na.omit(match(pathwayNammed, names(statsAdj)))))
  pathway <- sort(pathway)
  
  gseaRes <- calcGseaStat(statsAdj, selectedStats = pathway, returnAllExtremes = TRUE)
  bottoms <- gseaRes$bottoms
  tops <- gseaRes$tops
  n <- length(statsAdj)
  xs <- as.vector(rbind(pathway - 1, pathway))
  ys <- as.vector(rbind(bottoms, tops))

  diff <- (max(tops) - min(bottoms))/8
  x = y = NULL
  
  ESScore <- tibble(
    x = c(0, xs, n + 1),
    y = c(0, ys, 0)
  ) %>%
    left_join(
    tibble(
      x = pathway,
      names = pathwayNammed
    )
    , by = "x"
  ) %>%
    mutate(
      text = ifelse(
        is.na(names),
        "",
        glue("Gene: {names}\n Rank: {x}\n Enrichment Score: {y}")
      )
    )

  p <- ESScore %>%
    plot_ly(
      type = 'scatter', 
      mode = 'lines',
      name = "ES score",
      x = ~ x, 
      y = ~ y,
      text = ~ text,
      hoverinfo = 'text',
      line = list(
        color = 'green', 
        width = 2
      ) 
    ) 

  geneTicks <- tibble(
    x = pathway,
    y = -diff/2,
    xend = pathway, 
    yend = diff/2,
    names = pathwayNammed 
  ) %>%
    mutate(
      text = glue("Gene: {names}\n Rank: {x}")
    )

  p <- p %>%
    add_segments(
      data = geneTicks,
      name = "hits",
      x = ~ x,
      y =  ~ y, 
      xend = ~ xend,
      yend = ~ yend,
      text = ~ text,
      hoverinfo = 'text',
      line = list(
        color = "black",
        size = ticksSize
      )
    )
  
  maxX <- max(ESScore$x)
  maxY <- max(ESScore$y)
  
  p <- p %>%
    layout(
      showlegend = FALSE,
      title = list(
        text = title,
        x = 0, 
        xref = "paper",
        font = list(
          color = 'Black',
          family = 'Arial',
          size = 22
        )
      ),
      xaxis = list(
        title = list(
          text = "Rank"
        ),
        range = list(-500, maxX*1.05),
        showgrid = FALSE,
        zeroline = TRUE,
        showline = TRUE
      ), 
      yaxis = list(
        title = list(
          text = "Enrichment score"
        ),
        showgrid = FALSE,
        zeroline = TRUE,
        showline = TRUE
      ),
      margin = list(
        t = 75
      ),
      shapes = list(
        list(
          type = "line",
          layer = "below",
          xref = "paper",
          yref = "y",
          axref = "y",
          ayref = "y",
          y0 = max(tops),
          y1 = max(tops),
          x0 = 0,
          x1 = 1,
          text = "",
          hovertext = "test",
          line = list(
            color = "red",
            dash = "dot", 
            width = 1
          )
        ),
        list(
          type = "line",
          layer = "below",
          xref = "paper",
          yref = "y",
          axref = "y",
          ayref = "y",
          y0 = min(bottoms),
          y1 = min(bottoms),
          x0 = 0,
          x1 = 1,
          text = "",
          line = list(
            color = "red",
            dash = "dot", 
            width = 1
          )
        ),
        list(
          type = "line",
          layer = "below",
          xref = "x",
          yref = "paper",
          axref = "y",
          ayref = "y",
          y0 = 0,
          y1 = 1,
          x0 = zero_cross,
          x1 = zero_cross,
          text = "",
          line = list(
            color = "grey50",
            dash = "dash", 
            width = 1
          )
        )
      ),
      annotations = list(
        list(
          x = zero_cross + (n / 8),
          y = 0.025,
          text = paste0("Zero cross at ", zero_cross),
          xref = "x",
          yref = "y",
          showarrow = FALSE,
          font = list(
            color = 'Black',
            family = 'Arial',
            size = 12
          )
        ),
        list(
          x = x_label,
          y = y_label,
          text = label,
          xref = "x",
          yref = "y",
          showarrow = FALSE,
          font = list(
            color = 'Black',
            family = 'Arial',
            size = 12
          )
        )
      )
    )
  
  p
  
}