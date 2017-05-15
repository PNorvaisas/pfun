#' Linear Hypothesis Testing in HT manner
#'
#' @param cfile Excel file containing standard contrast table
#' @param csheet Sheet in Excel file containing contrast table
#' @param samples.selected Limit contrasts to selected samples
#' @keywords read.contrasts contrasts
#' @export
#' @examples
#' read.contrasts()
#' 
#' 


read.contrasts<-function(cfile,csheet,samples.selected) {
  cont.table<-read.and.clean(cfile,csheet)
  cont.table$`FALSE`<-NULL
  rownames(cont.table)<-cont.table$Contrast
  contrasts<-cont.table$Contrast
  
  #Find columns which define samples
  samples.all<-colnames(cont.table)[apply(cont.table,2,function(x) length(setdiff(x,c('-1','0','1')))==0)]
  cont.table[,samples.all] <- sapply(cont.table[, samples.all], as.numeric)
  
  
  #samples.all<-setdiff(colnames(cont.table),c(descriptors,c('Description','Contrast','Contrast_type','Target','Reference')))
  #print(samples.all)
  
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
  cont.clean<-contrasts[apply(cont.table[,samples.missing],1,function(x) all(x==0) )]
  cont.table.clean<-cont.table[cont.clean,]
  cont.table.selected<-cont.table[cont.clean, samples.found]
  
  cont.matrix<-as.matrix(cont.table.selected)
  
  return(list("Contrasts.table"=cont.table.clean,"Contrasts.matrix"=cont.matrix))
}

