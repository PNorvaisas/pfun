#' Linear Hypothesis Testing in HT manner using Dplyr
#'
#' @param lmshape Table containing samples in rows and variables in columns
#' @param variables Variables to be tested in model
#' @param cont.matrix Contrast matrix with dummy variables
#' @param fml Formula that will be used to spread the effects of samples. DEFAULT to "0+Group"
#' @keywords hypothesise
#' @export
#' @examples
#' hypothesise2()


hypothesise2<-function(lmdata,formula,cont.matrix,weights.col=NA,variable=NA) {

  if(!is.na(variable)) {
    print(unique(lmdata[,variable]))
    #print(unique(as.character(lmdata[,variable])))
  }

  grps<-unlist(strsplit(formula,'~'))[2]
  valvar<-unlist(strsplit(formula,'~'))[1]
  grpcol<-gsub("0\\+","",grps)


  lmcompleteness<-lmdata %>%
    group_by_(grpcol) %>%
    summarise_(.dots = setNames(paste0("sum(!is.na(",valvar,"))"), 'Observations')) %>%
    data.frame

  lmcomplete<-lmcompleteness %>%
    filter(Observations>0)


  print(lmcompleteness)

  groups.indata<-as.character(unique(lmcomplete[,grpcol]))
  groups.incontrasts<-colnames(cont.matrix)

  #print(groups.indata)
  #print(groups.incontrasts)


  #Remember m column, but remove it from matrix
  if ('m' %in% groups.incontrasts){
    #print('H0 values found!')
    mval<-TRUE
    groups.incontrasts<-base::setdiff(groups.incontrasts,'m')
  } else {
    mval<-FALSE
  }

  if (length( base::setdiff(groups.indata,groups.incontrasts) )>0 ) {
    #More groups in data
    print('Some of the groups in data not described in contrasts!')
    groups.nocontrast<-base::setdiff(groups.indata,groups.incontrasts)
    groups.found<-intersect(groups.incontrasts,groups.indata)
    groups.miss<-c()
  } else if (length(base::setdiff(groups.incontrasts,groups.indata))>0) {
    #More groups in contrasts
    print('Some of the groups in contrasts not described in data!')
    groups.nocontrast<-c()
    groups.miss<-base::setdiff(groups.incontrasts,groups.indata)
    groups.found<-intersect(groups.incontrasts,groups.indata)
  } else {
    print('All groups from contrasts and data match!')
    groups.nocontrast<-c()
    groups.miss<-c()
    groups.found<-intersect(groups.incontrasts,groups.indata)
  }

  #Only use contrasts that do not depend on missing groups
  #Find contrasts that have at least one

  groups.removed<-base::setdiff(groups.incontrasts,groups.found)
  if (length(groups.removed>0)) {
    cont.remove<-rownames(cont.matrix)[ apply(cont.matrix[,groups.removed,drop=FALSE],1, function(x) any(x!=0) ) ]
  } else {
    cont.remove<-c()
  }
  #print(cont.remove)
  #cont.miss<-rownames(cont.matrix)[ apply(cont.matrix[,groups.found,drop=FALSE],1, function(x) any(x!=0) ) ]


  #print(cont.use)
  #if (length(groups.miss)>0) {
  #Remove contrasts that use missing groups
  #cont.nomiss<-rownames(cont.matrix)[ apply(cont.matrix[,groups.miss,drop=FALSE],1,function(x) all(x==0) )]

  cont.clean<-base::setdiff(rownames(cont.matrix),cont.remove)

  #Remove leftover groups for clarity
  groups.clean<-colnames(cont.matrix[,groups.found])[ apply(cont.matrix[cont.clean,groups.found,drop=FALSE],2, function(x) any(x!=0) ) ]

  if (mval==TRUE) {
    cont.matrix.clean<-cont.matrix[cont.clean,c(groups.clean,'m'),drop=FALSE]
  } else {
    cont.matrix.clean<-cont.matrix[cont.clean,groups.clean,drop=FALSE]
  }

  #If no complete contrasts left - skip
  if(sum(dim(cont.matrix.clean))<2 ){
    print("No comparisons to make!")
    return(data.frame())
  }

  print(cont.matrix.clean)

  lmdata<-lmdata%>%
    filter_(paste0(grpcol,"%in% groups.clean")) %>%
    mutate_(paste0(grpcol,"=factor(",grpcol,",levels=groups.clean,labels=groups.clean)"))

  if(!is.null(dim(weights.col))){
    model<-lm(as.formula(formula), data=lmdata, weights=weights.col)
  } else{
    model<-lm(as.formula(formula), data=lmdata)
  }


  if (mval==TRUE) {
    lmod_glht <- multcomp::glht(model, linfct = cont.matrix.clean[,c(groups.clean),drop=FALSE],rhs=cont.matrix.clean[,'m'])
  } else {
    lmod_glht <- multcomp::glht(model, linfct = cont.matrix.clean[,c(groups.clean),drop=FALSE])
  }

  result<-multcomp:::summary.glht(lmod_glht,test=multcomp::adjusted("none"))
  res<-plyr::ldply(result$test[c('coefficients','sigma','tstat','pvalues')])


  allresults<-res %>%
    gather(Contrast,Value,-`.id`) %>%
    spread(`.id`,Value) %>%
    rename(logFC=coefficients,SE=sigma,p.value=pvalues,t.value=tstat)

  #m adjustment
  if (mval==TRUE){
    allresults<-allresults %>%
      left_join(cont.matrix.clean[,c('m'),drop=FALSE],by.x='Contrast',by.y=0) %>%
      mutate(logFC=logFC-m,
             m=NULL)
  }

  return(allresults)
}

