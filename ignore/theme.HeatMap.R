#' @title theme.HeatMap
#'
#' @description creates a custom theme for dQ/dV heat map data, used in report.Rmd
#'
#' @return theme_HeatMap for ggplot2
#' @export
#'
#' @examples
#' p <- ggplot(mtcars) +
#'      geom_point(aes(x=mpg, y=cyl), color='red', size=4) +
#'      theme.HeatMap()
#'
#' p <- p + theme.HeatMap()

theme.HeatMap <- theme_bw() +
                    theme(text = element_text(face="bold", size=16)) +
                    theme(axis.title.x = element_text(vjust=-0.3, size=16)) +
                    theme(axis.title.y = element_text(vjust=0.8, size=16)) +
                    theme(panel.border = element_rect(size=1,color = "black")) +
                    theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm")) +
                    theme(panel.grid.major = element_line(linewidth=0.5))
