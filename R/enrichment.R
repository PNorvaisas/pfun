#' Calculate enrichment
#'
#' @param data Table with data
#' @param grouping Columns over which data needs to be grouped
#' @param feature Feature for which enrichment is to be calculated
#' @param Sbrks Enrichment breaks in -log10 scale
#' @param Slbls Enrichment labels (one less than breaks)
#' @keywords enrichment
#' @export
#' @examples
#'

enrichment<-function(data,grouping,feature,
                 Sbrks=c(0,-log(0.05,10),2,3,4,1000),
                 Slbls=c('N.S.','<0.05','<0.01','<0.001','<0.0001') ){
  data %>%
    mutate(Up=logFC>0 & FDR <0.05,
           Down=logFC<0 & FDR <0.05,
           All=FDR <0.05) %>%
    #gather different thresholds
    gather(Type,Pass,Up,Down,All) %>%
    group_by_(.dots=c(grouping,"Type")) %>%
    mutate(Total_size=n(),
           Total_pass=sum(Pass,na.rm = TRUE)) %>%
    group_by_(.dots=c(grouping,"Type",feature,"Total_size","Total_pass") )%>%
    summarise(Class_size=n(),
              Class_pass=sum(Pass,na.rm = TRUE)) %>%
    group_by_(.dots=c(grouping,"Type")) %>%
    mutate(p.value=phyper(Class_pass-1,Total_pass,Total_size-Total_pass,Class_size,lower.tail =FALSE),
           FDR=p.adjust(p.value,method="fdr"),
           FE=(Class_pass/Class_size)/(Total_pass/Total_size),
           logFDR=ifelse(-log10(FDR)<0,0,-log10(FDR)),
           logFDRbin=cut(logFDR,breaks=Sbrks,labels=Slbls,right=FALSE)) %>%
    ungroup %>%
    mutate(Type=factor(Type,levels=c("All","Up","Down")))
}
