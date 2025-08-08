#' @title theme1
#'
#' @description creates a custom theme for plotting data, used in report.Rmd
#'
#' @return theme1 for ggplot2
#' @export
#'
#' @examples
#' p <- ggplot(mtcars) +
#'      geom_point(aes(x=mpg, y=cyl), color='red', size=4) +
#'      theme1()
#'
#' p <- p + theme1()
theme1 <- function() {
  theme_bw() %+replace%
    theme(

      # L'ensemble de la figure
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),

      # Zone où se situe le graphique
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),

      # Les axes
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black"),

      # La légende
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),

      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}
