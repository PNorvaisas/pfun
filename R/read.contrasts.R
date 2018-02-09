#' Linear Hypothesis Testing in HT manner
#'
#' @param cfile Excel file containing standard contrast table
#' @param csheet Sheet in Excel file containing contrast table
#' @param samples.selected Limit contrasts to selected samples
#' @param variables Other variables to preserve in table
#' @keywords read.contrasts contrasts
#' @export
#' @examples
#' read.contrasts()
#'
#'


read.contrasts<-function(cfile,csheet,samples.selected,variables=c()) {
  cont.table<-readxl::read_xlsx(cfile,sheet=csheet)
  #cont.table$`FALSE`<-NULL
  rownames(cont.table)<-cont.table$Contrast
  contrasts<-cont.table$Contrast


  #print(mval)

  #Find columns which define samples
  #samples.all<-colnames(cont.table)[apply(cont.table,2,function(x) length(setdiff(x,c('-1','0','1')))==0)]
  samples.all<-colnames(cont.table)[apply(cont.table,2,function(x) all(!grepl("[a-zA-Z]",x)))]
  cont.table[,samples.all] <- sapply(cont.table[, samples.all], as.numeric)


  if ('m' %in% samples.all){
    mval<-TRUE
  } else {
    mval<-FALSE
  }
  samples.all<-setdiff(samples.all,c('m',variables))



  samples.found<-intersect(samples.all,samples.selected)
  samples.missing<-setdiff(samples.all,samples.found)


  if (length(samples.selected)>length(samples.found)){
    print("Some of the selected samples not present in the table!")
    print(setdiff(samples.selected,samples.found))
  } else {
    print("All samples found!")
    print(samples.found)
  }

  #print(samples.missing)

  #Find contrasts that do not require missing samples
  cont.use<-contrasts[apply(cont.table[,samples.found],1,function(x) any(x==1) | any(x==-1) )]

  if ( length(samples.missing)>0 ) {
    cont.nomiss<-contrasts[apply(cont.table[,samples.missing],1,function(x) all(x==0) )]
    cont.clean<-intersect(cont.nomiss,cont.use)
  } else {
    cont.clean<-cont.use
  }


  cont.table.clean<-cont.table[cont.clean,,drop=FALSE]

  if (mval==TRUE) {
    cont.table.selected<-cont.table[cont.clean, c(samples.found,'m'),drop=FALSE]
  } else {
    cont.table.selected<-cont.table[cont.clean, samples.found,drop=FALSE]
  }


  cont.matrix<-as.matrix(cont.table.selected)

  return(list("Contrasts.table"=cont.table.clean,"Contrasts.matrix"=cont.matrix))
}

