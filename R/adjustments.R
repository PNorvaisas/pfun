#' Expand output of GLHT, calculate FDR
#' 
#' @param data 
#' @keywords FDR, pStars
#' @export
#' @examples
#' adjustments(x)
#'
#'

adjustments<-function(data){
  data %>%
    group_by(Contrast) %>%
    mutate(pStars=pStars(p.value),
           FDR=p.adjust(p.value,method = 'fdr'),
           PE=logFC+SE,
           NE=logFC-SE,
           logFDR=-log10(FDR)) %>%
    ungroup
} 
