#' Plot Enrichment data
#'
#' @param data Data for plotting
#' @param xval Comparisons
#' @param yval Features
#' @param fillval Value for fill, factor
#' @param ncols Number of color levels, default: 5. length(enrbrks)
#' @keywords PlotEnrichment
#' @export
#' @examples
#' PlotEnrichment(data,xval,yval,fillval,ncols=5)
#'

PlotEnrichment<-function(data,xval,yval,fillval="logFDRbin",ncols=6) {
  enrcols<-colorRampPalette(c("gray90","steelblue1","blue4"))(n = ncols)
  data %>%
    ggplot(aes_string(x=xval,y=yval))+
    geom_tile(aes_string(fill=fillval))+
    scale_fill_manual(values=enrcols)+
    #xlab("Direction of change")+
    #ylab('Feature')+
    labs(fill='FDR')+
    theme(axis.ticks=element_blank(),
          panel.border=element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line = element_line(colour = NA),
          axis.line.x = element_line(colour = NA),
          axis.line.y = element_line(colour = NA),
          strip.text = element_text(colour = 'black', face='bold',size=10),
          axis.text.x= element_text(face='bold', colour='black', size=10, angle = 90, hjust = 1))
}

