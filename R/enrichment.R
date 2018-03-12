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
  #allIDs<-length(unique(data[,IDs]))
  data.sum<-plyr::ddply(data,c(terms,comparisons),plyr::here(summarise),
                  Term_total=length(get(IDs)),
                  All=sum(get(sign)<0.05, na.rm = TRUE),
                  Up=sum(get(sign)<0.05 & get(change)>0, na.rm = TRUE),
                  Down=sum(get(sign)<0.05 & get(change)<0, na.rm = TRUE)
  )
  data.total<-plyr::ddply(data,c(comparisons),plyr::here(summarise),
                    Comparison_total=length(get(IDs)),
                    All=sum(get(sign)<0.05 & !duplicated(get(IDs)),na.rm = TRUE),
                    Up=sum(get(sign)<0.05 & get(change)>0 & !duplicated(get(IDs)), na.rm = TRUE),
                    Down=sum(get(sign)<0.05 & get(change)<0 & !duplicated(get(IDs)), na.rm = TRUE)
  )
  data.m<-reshape2::melt(data.sum,measure.vars=c('All','Up','Down'),variable.name='Test',value.name = 'Term')
  data.tm<-reshape2::melt(data.total,measure.vars=c('All','Up','Down'),variable.name='Test',value.name = 'Comparison')
  data.c<-merge(data.m,data.tm,by=c(comparisons,'Test'),all.x=TRUE)

  data.c$p<-phyper(data.c$Term-1,data.c$Comparison,data.c$Comparison_total-data.c$Comparison,data.c$Term_total,lower.tail=FALSE)
  data.c<-plyr::ddply(data.c,c(comparisons,'Test'),plyr::here(mutate),FDR=p.adjust(p,method='fdr'))
  return(data.c)
}
