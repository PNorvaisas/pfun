#' Calculate ellipse for 2D distirbution
#'
#' @param x Values in 1st dimension
#' @param y Values in 1st dimension
#' @param sc Scaling factor to SD
#' @keywords Ellipse
#' @export
#' @examples
#' 


getellipse<-function(x,y,sc=1) {
  as.data.frame(ellipse::ellipse( cor(x, y),
                                  scale=c(sd(x)*sc,sd(y)*sc),
                                  centre=c( mean(x),mean(y)) ))
}