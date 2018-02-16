#' Get clean significance stars
#' 
#' @param x Vector witg p values
#' @keywords p, Stars
#' @export
#' @examples
#' pStars(x)
#'
#'


pStars<-function(x) {
  ps<-gsub('\\.| ', NA ,gtools::stars.pval(x))
  return(ps)
}

