#' Default ggplot2 plotting scheme of Mirko Thalmann
#' @title plotting in a proper manner
#' @name plotTheme
#' @param plot a \code{list} constructed by ggplot2.
#' @return returns a list with the same components as plot
#' @author Thalmann, M.
#' @importFrom ggplot2 ggplot aes geom_point geom_segment coord_flip ggtitle labs theme_bw theme annotate scale_y_continuous element_blank element_text element_line rel aes_string element_rect
#' @importFrom utils globalVariables
#' @export

plot_thme <- function (plot){
  plot +
    theme(panel.background = element_rect(fill = "black")) +
    theme(panel.grid=element_blank(),panel.background=element_blank()) + theme_bw() +
    theme(plot.title = element_text(size = rel(1.75), hjust = 0)) +
    theme(legend.text = element_text(size = 11), legend.title = element_text(size = 11),
          legend.key = element_blank(), legend.box = 'horizontal') +
    theme(legend.background = element_rect(colour = "grey50")) +
    theme(axis.title.x = element_text(size=16), axis.text.x  = element_text(size=14)) +
    theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=14)) +
    theme(legend.position = "bottom")+ theme(strip.text.x = element_text(size = 14))+
    labs(title = "\n") +
    theme(plot.background = element_blank(),panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}
