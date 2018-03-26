#' Theme Heatmap
#'
#' @keywords ggtheme, Heatmap
#' @export
#' @examples
#'


theme_Heatmap<-function(){
  theme(axis.ticks=element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
}


