#' Calculates Mean, SD, Min, Max for boxplot
#'
#' @param x Vector with numeric values
#' @keywords Mean, SD, Min, Max
#' @export
#' @examples
#' 


MinMeanSDMax <- function(x) {
  v <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}