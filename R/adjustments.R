#' Expand output of GLHT, calculate FDR
#'
#' @param data Data from hypothesise2
#' @keywords FDR, pStars
#' @export
#' @examples
#' adjustments(x)
#'
#'

adjustments<-function(data,groupings=c()){

  if (length(groupings)>0 ){
    groupings<-c(groupings,"Contrast")
  } else {
    groupings<-c("Contrast")
  }

  data %>%
    group_by_("Contrast",groupings) %>%
    mutate(FDR=p.adjust(p.value,method = 'fdr'),
           pStars=pStars(p.value),
           FDRStars=pStars(FDR),
           PE=logFC+SE,
           NE=logFC-SE,
           logFDR=-log10(FDR)) %>%
    ungroup
}
