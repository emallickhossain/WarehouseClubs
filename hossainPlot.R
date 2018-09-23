#' Mallick's Custom Plotly Template
#'
#' @param data data.table. Data set to be plotted
#' @param x Character. Formula of x value names. "~x"
#' @param y Character. Formula of y value names. "~y".
#' @param title Character. Title of chart
#' @param xlab Character. Title of x-axis
#' @param ylab Character. Title of y-axis
#' @param yrange 2-element vector. Range of y-axis
#' @param ydtick Integer.
#' @param source Character. Sources of data to be plotted
#' @param yname Character. Name of y series
#' @param height Integer. Height of plot
#' @param width Integer. Width of plot
#' @param linewidth Integer. Width of line.
#' @param split Character. Groups for lines. Formula of group name. "~split".
#'
#' @return Plotly chart
#' @export
#'
hossainPlot <- function(data, x, y, yname, title, xlab, ylab, yrange, ydtick, source,
                        height = 800, width = 1200, linewidth = 10, split = NULL) {
  chart <- plot_ly(data = data, x = formula(x), height = height, width = width) %>%
    add_lines(y = formula(y), name = yname, split = formula(split), line = list(width = linewidth)) %>%
    layout(title = title, titlefont = list(size = 35),
           xaxis = list(title = xlab, titlefont = list(size = 30),
                        tickfont = list(size = 25), nticks = 10),
           yaxis = list(title = ylab, range = yrange, dtick = ydtick,
                        titlefont = list(size = 30), tickfont = list(size = 25)),
           # Pick positioning of legend so it doesn"t overlap the chart
           legend = list(yanchor = "top", y = 1.02, x = 0, font = list(size = 20)),
           # Adjust margins so things look nice
           margin = list(l = 140, r = 140, t = 70, b = 150, pad = 10),
           annotations = list(text = paste0("Source: ", source), font = list(size = 20),
                              showarrow = FALSE, align = "left", valign = "bottom",
                              xref = "paper", x = -0.03, yref = "paper", y = -0.25))
  return(chart)
}
