#' Calculate enrichment
#'
#' @param data Table with data
#' @param term Columns containing terms to calculate enrichment for
#' @param IDs Columns with IDs of entities that each term contains
#' @param comparisons Column containing differnet comparisons that will be tested
#' @param change Column containing value for direction of change
#' @param sign Column containing significance of change
#' @keywords enrichment
#' @export
#' @examples
#'

enrichment<-function(data,terms,IDs,comparisons,change,sign){
  allIDs<-length(unique(data[,IDs]))
  
  data.sum<-ddply(data,c(terms,comparisons),here(summarise),
                  Term_total=length(get(IDs)),
                  All=sum(get(sign)<0.05, na.rm = TRUE),
                  Up=sum(get(sign)<0.05 & get(change)>0, na.rm = TRUE),
                  Down=sum(get(sign)<0.05 & get(change)<0, na.rm = TRUE)
  )
  data.total<-ddply(data,c(comparisons),here(summarise),
                    All=sum(get(sign)<0.05 & !duplicated(get(IDs)),na.rm = TRUE),
                    Up=sum(get(sign)<0.05 & get(change)>0 & !duplicated(get(IDs)), na.rm = TRUE),
                    Down=sum(get(sign)<0.05 & get(change)<0 & !duplicated(get(IDs)), na.rm = TRUE)
  )
  data.m<-melt(data.sum,measure.vars=c('All','Up','Down'),variable.name='Test',value.name = 'Term')
  data.tm<-melt(data.total,measure.vars=c('All','Up','Down'),variable.name='Test',value.name = 'Comparison')
  data.c<-merge(data.m,data.tm,by=c(comparisons,'Test'),all.x=TRUE)
  
  data.c$p<-phyper(data.c$Term-1,data.c$Comparison,allIDs-data.c$Comparison,data.c$Term_total,lower.tail=FALSE)
  data.c<-ddply(data.c,c(comparisons,'Test'),here(mutate),FDR=p.adjust(p,method='fdr'))
  return(data.c)  
}