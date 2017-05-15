#' Linear Hypothesis Testing in HT manner
#'
#' @param lmshape Table containing samples in rows and variables in columns
#' @param variables Variables to be tested in model
#' @param cont.matrix Contrast matrix with dummy variables
#' @param formula Formula that will be used to spread the effects of samples. DEFAULT to "0+Sample"
#' @keywords hypothesise
#' @export
#' @examples
#' hypothesise()



hypothesise<-function(lmshape,variables,cont.matrix,formula="0+Sample"){
  samples.indata<-unique(lmshape$Sample)
  samples.incontrasts<-colnames(cont.matrix)
  #print(samples.indata)
  #print(samples.incontrasts)
  
  if (length( setdiff(samples.indata,samples.incontrasts) )>0 ) {
    #More samples in data
    print('Some of the samples in data not described in contrasts!')
    samples.nocontrast<-setdiff(samples.indata,samples.incontrasts)
    samples.found<-intersect(samples.incontrasts,samples.indata)
    samples.miss<-c()
  } else if (length(setdiff(samples.incontrasts,samples.indata))>0) {
    #More samples in contrasts
    print('Some of the samples in contrasts not described in data!')
    samples.nocontrast<-c()
    samples.miss<-setdiff(samples.incontrasts,samples.indata)
    samples.found<-intersect(samples.incontrasts,samples.indata)
  } else {
    print('All samples from match in contrasts and data!')
    samples.nocontrast<-c()
    samples.miss<-c()
    samples.found<-intersect(samples.incontrasts,samples.indata)
  }
  
  
  cont.use<-rownames(cont.matrix)[ apply(cont.matrix[,samples.found],1, function(x) ( any(x==1) & any(x==-1) ) | any(x==1) ) ]
  
  #print(cont.use)
  if (length(samples.miss)>0) {
    cont.nomiss<-rownames(cont.matrix)[ apply(cont.matrix[,samples.miss],1,function(x) all(x==0) )]
    #Select contrasts that don't use missing samples
    cont.clean<-intersect(cont.nomiss,cont.use)
  } else {
    cont.clean<-cont.use
  }
  
  
  print('Selected contrasts:')
  print(cont.clean)
  
  lmshape<-subset(lmshape,Sample %in% samples.found)
  
  cont.matrix.clean<-cont.matrix[cont.clean,samples.found,drop=FALSE]
  
  #rownames(cont.matrix)
  #print(cont.matrix)
  #print(cont.matrix.clean)
  
  lmshape$Sample<-factor(lmshape$Sample,levels=colnames(cont.matrix.clean),labels=colnames(cont.matrix.clean))
  
  #Fix it to the smallest overlapping set
  
  allresults.t<-data.frame()
  prec<-0
  len<-length(variables)
  for (id in 1:length(variables)) {
    pr<-as.character(variables[id])
    precn<-id*100/len
    if (precn-prec>5) {
      print(paste(round(precn,digits=0),'%',sep=''))
      prec<-precn
    }
    model<-lm(paste("`",pr,"`~",formula,sep=""),lmshape)
    #Generalised linear hypothesis testing
    lmod_glht <- glht(model, linfct = cont.matrix.clean)
    result<-summary(lmod_glht,test=adjusted("none"))
    res<-ldply(result$test[c('coefficients','sigma','tstat','pvalues')])
    res$Variable<-pr
    allresults.t<-rbind(allresults.t,res)
  }
  allresults.m<-melt(allresults.t,id.vars = c('.id','Variable'),variable.name = 'Contrast',value.name = 'Value')
  allresults<-dcast(allresults.m,Variable+Contrast~`.id`,value.var = 'Value')
  allresults<-rename(allresults,c('coefficients'='logFC','sigma'='SE','pvalues'='p.value','tstat'='t.value'))
  allresults$PE<-allresults$logFC+allresults$SE
  allresults$NE<-allresults$logFC-allresults$SE
  allresults$FDR<-p.adjust(allresults$p.value,method = 'fdr')
  allresults$logFDR<--log10(allresults$FDR)
  
  allresults.m<-melt(allresults,id.vars = c('Contrast','Variable'),measure.vars = c('logFC','SE','t.value','p.value','FDR'),
                     variable.name = 'Stats',value.name = 'Value')
  
  allresults.castfull<-dcast(allresults.m,Variable~Contrast+Stats,value.var = 'Value')
  
  allresults.msimple<-subset(allresults.m,Stats %in% c('logFC','FDR'))
  allresults.cast<-dcast(allresults.msimple,Variable~Contrast+Stats,value.var = 'Value')
  
  return(list("All"=allresults,"Cast"=allresults.cast,"Melt"=allresults.m,"CastFull"=allresults.castfull))
}

