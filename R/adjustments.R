#' Expand output of GLHT, calculate FDR
#'
#' @param data Data from hypothesise2
#' @param groupings Additional groupings of data over which FDR adjustments will be made
#' @param Sbrks Significance breaks in -log10 scale
#' @param Slbls Significance labels (one less than breaks)
#' @keywords FDR, pStars,
#' @export
#' @examples adjustments(x)

adjustments<-function(data,groupings=c(),
                      Sbrks=c(0,-log(0.05,10),2,3,4,1000),
                      Slbls=c('N.S.','<0.05','<0.01','<0.001','<0.0001')){

  data %>%
    group_by_(.dots=c(groupings,"Contrast")) %>%
    mutate(FDR=p.adjust(p.value,method = 'fdr'),
           pStars=pStars(p.value),
           FDRStars=pStars(FDR),
           PE=logFC+SE,
           NE=logFC-SE,
           logFDR=ifelse(-log10(FDR)<0,0,-log10(FDR)),
           logFDRbin=cut(logFDR,breaks=Sbrks,labels=Slbls,right=FALSE) ) %>%
    ungroup
}
