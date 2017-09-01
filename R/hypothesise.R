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



hypothesise<-function(lmshape,variables,cont.matrix,formula="0+Group",weights.mat=NA){
  grpcol<-gsub("0\\+","",formula)
  groups.indata<-unique(lmshape[,grpcol])
  groups.incontrasts<-colnames(cont.matrix)
  #print(groups.indata)
  #print(groups.incontrasts)
  
  if(!is.null(dim(weights.mat))){
    print('Using weights!')
  }
  
  if ('m' %in% groups.incontrasts){
    print('H0 values found!')
    mval<-TRUE
    groups.incontrasts<-setdiff(groups.incontrasts,'m')
  } else {
    mval<-FALSE
  }
  
  if (length( setdiff(groups.indata,groups.incontrasts) )>0 ) {
    #More groups in data
    print('Some of the groups in data not described in contrasts!')
    groups.nocontrast<-setdiff(groups.indata,groups.incontrasts)
    groups.found<-intersect(groups.incontrasts,groups.indata)
    groups.miss<-c()
  } else if (length(setdiff(groups.incontrasts,groups.indata))>0) {
    #More groups in contrasts
    print('Some of the groups in contrasts not described in data!')
    groups.nocontrast<-c()
    groups.miss<-setdiff(groups.incontrasts,groups.indata)
    groups.found<-intersect(groups.incontrasts,groups.indata)
  } else {
    print('All groups from contrasts and data match!')
    groups.nocontrast<-c()
    groups.miss<-c()
    groups.found<-intersect(groups.incontrasts,groups.indata)
  }
  
  #Find contrasts that have at least one 
  cont.use<-rownames(cont.matrix)[ apply(cont.matrix[,groups.found],1, function(x) any(x!=0) ) ]
  
  #print(cont.use)
  if (length(groups.miss)>0) {
    #Remove contrasts that use missing groups
    cont.nomiss<-rownames(cont.matrix)[ apply(cont.matrix[,groups.miss],1,function(x) all(x==0) )]
    cont.clean<-intersect(cont.nomiss,cont.use)
  } else {
    cont.clean<-cont.use
  }
  
  
  print('Selected contrasts:')
  print(cont.clean)
  
  #lmshape<-subset(lmshape,Sample %in% groups.found)
  lmshape<-lmshape[lmshape[,grpcol] %in% groups.found,]
  if(!is.null(dim(weights.mat))){
    weights.mat<-weights.mat[weights.mat[,grpcol] %in% groups.found,]
  }
  #print(dim(weights.mat))
  
  if (mval==TRUE) {
    cont.matrix.clean<-cont.matrix[cont.clean,c(groups.found,'m'),drop=FALSE]
  } else {
    cont.matrix.clean<-cont.matrix[cont.clean,groups.found,drop=FALSE]
  }
  
  #print(cont.matrix.clean)

  
  #rownames(cont.matrix)
  #print(cont.matrix)
  #print(cont.matrix.clean)
  
  lmshape[,grpcol]<-factor(lmshape[,grpcol],levels=groups.found,labels=groups.found)
  if(!is.null(dim(weights.mat))){
    weights.mat[,grpcol]<-factor(weights.mat[,grpcol],levels=groups.found,labels=groups.found)
  }
  #print(dim(weights.mat))
  
  #Fix it to the smallest overlapping set
  
  allresults.t<-data.frame()
  prec<-0
  len<-length(variables)
  for (id in 1:length(variables)) {
    pr<-as.character(variables[id])
    #print(pr)
    precn<-id*100/len
    if (precn-prec>5) {
      print(paste(round(precn,digits=0),'%',sep=''))
      prec<-precn
    }
    print(weights.mat[,pr])
    
    
    if(!is.null(dim(weights.mat))){
      model<-lm(formula=paste("`",pr,"`~",formula,sep=""),data=lmshape,weights = weights.mat[,paste(pr)])
    } else{
      model<-lm(formula=paste("`",pr,"`~",formula,sep=""),data=lmshape)
    }
      
    #Generalised linear hypothesis testing
    if (mval==TRUE) {
      lmod_glht <- glht(model, linfct = cont.matrix.clean[,c(groups.found)],rhs=cont.matrix.clean[,'m'])
    } else {
      lmod_glht <- glht(model, linfct = cont.matrix.clean[,c(groups.found)])
    }
    
    result<-summary(lmod_glht,test=adjusted("none"))
    res<-ldply(result$test[c('coefficients','sigma','tstat','pvalues')])
    res$Variable<-pr
    allresults.t<-rbind(allresults.t,res)
  }
  
  
  allresults.m<-melt(allresults.t,id.vars = c('.id','Variable'),variable.name = 'Contrast',value.name = 'Value')
  allresults<-dcast(allresults.m,Variable+Contrast~`.id`,value.var = 'Value')
  allresults<-rename(allresults,c('coefficients'='logFC','sigma'='SE','pvalues'='p.value','tstat'='t.value'))
  
  if (mval==TRUE){
    allresults<-merge(allresults,cont.matrix.clean[,c('m'),drop=FALSE],by.x='Contrast',by.y=0,all.x=TRUE)
    allresults$logFC<-allresults$logFC-allresults$m
    allresults$m<-NULL
  }
  
  allresults$PE<-allresults$logFC+allresults$SE
  allresults$NE<-allresults$logFC-allresults$SE
  allresults$FDR<-p.adjust(allresults$p.value,method = 'fdr')
  allresults$logFDR<--log10(allresults$FDR)
  
  allresults.m<-melt(allresults,id.vars = c('Contrast','Variable'),measure.vars = c('logFC','SE','NE','PE','t.value','p.value','FDR'),
                     variable.name = 'Stats',value.name = 'Value')
  
  allresults.castfull<-dcast(allresults.m,Variable~Contrast+Stats,value.var = 'Value')
  
  allresults.msimple<-subset(allresults.m,Stats %in% c('logFC','FDR'))
  allresults.cast<-dcast(allresults.msimple,Variable~Contrast+Stats,value.var = 'Value')
  
  return(list("All"=allresults,"Cast"=allresults.cast,"Melt"=allresults.m,"CastFull"=allresults.castfull))
}

