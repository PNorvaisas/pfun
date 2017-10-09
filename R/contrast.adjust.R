#' Adjustment of contrasts
#'
#' @param contrasts.matrix Contrast matrix
#' @param contrasts.table Contrast table
#' @param adjustments.list Contrasts to be adjusted
#' @param data Data to estimate correlation of contrasts
#' @param suffix Suffixes added to the Group names
#' @keywords contrast.adjust
#' @export
#' @examples
#' contrast.adjust()

contrast.adjust<-function(contrasts.matrix,contrasts.table,adjustments.list,data,suffix='') {
  for (a in adjustments.list){
    print(a)
    selcontrast<-contrasts.matrix[a,]
    
    ref<-names(selcontrast[selcontrast==-1])
    targ<-names(selcontrast[selcontrast==1])
    
    targ.str<-paste0(targ,suffix,collapse='+')
    ref.str<-paste0(ref,suffix,collapse='+')
    #print(targ.str)
    #print(ref.str)
    
    fit<-lm(as.formula(paste0(targ.str,'~',ref.str)),data=data)
    res<-summary(fit)
    
    #outlierTest(genfit)
    ot<-car::outlierTest(fit)
    # qqPlot(genfit, main="QQ Plot")
    # 
    outlist<-c(names(ot$rstudent))
    # #Outliers:
    #print(paste0('Outliers in ',a))
    #print(data[outlist,])
    
    fit2<-lm(as.formula(paste0(targ.str,'~',ref.str)),data=subset(data,!rownames(data) %in% outlist))
    res2<-summary(fit2)
    
    estimates<-res2$coefficients[,'Estimate']
    
    if (suffix!=''){
      names(estimates)<-gsub(suffix,'',names(estimates))
    }
    
    names(estimates)<-gsub('\\(Intercept\\)','m',names(estimates))
    estimates[names(estimates)!='m']<-estimates[names(estimates)!='m']*-1
    print(estimates)
    
    contrasts.table[match(a,contrasts.table$Contrast),names(estimates)]<-estimates
    contrasts.matrix[a,names(estimates)]<-estimates
  }
  return(list('Matrix'=contrasts.matrix,'Table'=contrasts.table))
}

