#' Melt table for enrichemnt without losing NAs
#'
#' @param data Table with data
#' @param idvariables Column with IDs to keep in melt
#' @param selstats Column with measurements to melt
#' @keywords enrichment melt
#' @export
#' @examples
#'

enrichmentmelt<-function(data,idvariables,selstats){
  statcols<-c()
  for (s in selstats) {
    statcols<-c(statcols,colnames(data)[grepl(paste0('_',s),colnames(data))])
  }
  data.m<-melt(data,id.vars=idvariables,measure.vars = statcols,
               variable.name = 'Column',value.name = 'Value')
  
  data.m$Contrast<-gsub(paste0('_',paste0(selstats,collapse = '|_')),'',data.m$Column,fixed=FALSE)
  data.m$Stat<-mapply(gsub,paste0(data.m$Contrast,'_'),'',data.m$Column,USE.NAMES = FALSE,fixed=TRUE)
  data.c<-dcast(data.m,paste0(paste(idvariables,collapse = '+'),'+Contrast~Stat'),value.var = 'Value')
  return(data.c)
}