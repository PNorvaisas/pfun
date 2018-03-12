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

  data %>%
    group_by_(.dots=c(groupings,"Contrast")) %>%
    mutate(FDR=p.adjust(p.value,method = 'fdr'),
           pStars=pStars(p.value),
           FDRStars=pStars(FDR),
           PE=logFC+SE,
           NE=logFC-SE,
           logFDR=-log10(FDR)) %>%
    ungroup
}
