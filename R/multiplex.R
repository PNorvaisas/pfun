#' Multiplex your data frame
#'
#' @param data Data to multiplex
#' @param groups Grouping variables to retain, everything else will be multiplexed
#' @param dim Number of dimensions in the final table. Minimum is two, anything more than 4 can take a very long time...
#' @keywords multiplex
#' @export
#' @examples
#' multiplex(data,groups,dim=)
#'
#'

multiplex<-function(data,groups,dims=3){

  cnt.vals<-base::setdiff(colnames(data),groups)

  axes<-paste0(c('y','z',rev(letters)[4:length(letters)]),"_")

  data.multi<-data %>%
    rename_(.dots = setNames(cnt.vals, paste0('x_',cnt.vals)))

  try(if(dims < 2) stop("Minimum number of dimensions is 2!"))

  joins=dims-1

  for (join in 1:joins){
    #print(join)
    data.multi<-data.multi %>%
      full_join(data %>%
                  rename_(.dots = setNames(cnt.vals, paste0(axes[join],cnt.vals))))
  }
  data.multi<-data.multi %>%
    select(groups,everything())

  return(data.multi)
}

