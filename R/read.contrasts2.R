#' Get contrasts for Linear Hypothesis Testing in HT manner.
#' Works with hypothesise2
#' 
#' @param cfile Excel file containing standard contrast table
#' @param csheet Sheet in Excel file containing contrast table. Default: 'Contrasts_values'
#' @param samples.selected Limit contrasts to selected samples
#' @param variables Variables to preserve in the table
#' @keywords read.contrasts contrasts
#' @export
#' @examples
#' read.contrasts()
#'
#'


read.contrasts2<-function(cfile,csheet='Contrasts_values') {
  cont.table<-readxl::read_xlsx(cfile,sheet=csheet) %>%
    data.frame
  rownames(cont.table)<-cont.table$Contrast
  contrasts<-cont.table$Contrast
  
  descriptions<-colnames(cont.table)[1:match('Reference',colnames(cont.table))]
  
  samples.all<-setdiff(colnames(cont.table),descriptions)
                       
  cont.table[,samples.all] <- sapply(cont.table[, samples.all], as.numeric)
  
  cont.table.clean<-cont.table[,descriptions,drop=FALSE]

  cont.matrix<-as.matrix(cont.table[, samples.all,drop=FALSE])
  
  return(list("Contrasts.table"=cont.table.clean,"Contrasts.matrix"=cont.matrix))
}

