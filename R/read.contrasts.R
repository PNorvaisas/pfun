#' Get contrasts for Linear Hypothesis Testing in HT manner.
#' Works with hypothesise2
#'
#' @param cfile Excel file containing standard contrast table
#' @param csheet Sheet in Excel file containing contrast table. Default: First sheet
#' @param samples.selected Limit contrasts to selected samples
#' @param variables Variables to preserve in the table
#' @keywords read.contrasts contrasts
#' @export
#' @examples
#' read.contrasts()
#'
#'


read.contrasts<-function(cfile,csheet=1) {
  cont.table<-readxl::read_xlsx(cfile,sheet=csheet) %>%
    data.frame
  contrasts<-cont.table$Contrast

  descriptions<-colnames(cont.table)[1:match('Reference',colnames(cont.table))]

  samples.all<-setdiff(colnames(cont.table),descriptions)

  cont.table<-cont.table %>%
    mutate_at(samples.all, as.numeric)

  cont.table.clean<-cont.table %>%
    select(descriptions) %>%
    mutate_all(as.factor)

  cont.matrix<-cont.table %>%
    select(samples.all) %>%
    as.matrix

  #rownames(cont.table.clean)<-contrasts
  rownames(cont.matrix)<-contrasts

  return(list("Contrasts.table"=cont.table.clean,"Contrasts.matrix"=cont.matrix))
}

