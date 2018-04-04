#' Calculate enrichment
#'
#' @param data Table with data
#' @param groups Groups for which enrichment is to be calculated. Can be column name string or vector of such names
#' @param Sbrks Enrichment breaks in -log10 scale
#' @param Slbls Enrichment labels (one less than breaks)
#' @keywords enrichment
#' @export
#' @examples
#'

enrichment<-function(data,groups,
                 Sbrks=c(0,-log(0.05,10),2,3,4,1000),
                 Slbls=c('N.S.','<0.05','<0.01','<0.001','<0.0001') ){
  
  grouping<-group_vars(data)
    
  data %>%
    mutate(Up=logFC>0 & FDR <0.05,
           Down=logFC<0 & FDR <0.05,
           All=FDR <0.05) %>%
    #gather different thresholds
    gather(Type,Pass,Up,Down,All) %>%
    group_by_(.dots=c(grouping,"Type")) %>%
    mutate(Total_size=n(),
           Total_pass=sum(Pass,na.rm = TRUE)) %>%
    group_by_(.dots=c(grouping,"Type",groups,"Total_size","Total_pass") )%>%
    summarise(Class_size=n(),
              Class_pass=sum(Pass,na.rm = TRUE)) %>%
    group_by_(.dots=c(grouping,"Type")) %>%
    # Class_pass-1 because we are interested in what's the probability of drawing Class_pass or more. Otherwise, it would be Class_pass+1 or more
    #Good explanation at: https://blog.alexlenail.me/understanding-and-implementing-the-hypergeometric-test-in-python-a7db688a7458
    mutate(p.value=phyper(Class_pass-1,Total_pass,Total_size-Total_pass,Class_size,lower.tail =FALSE),
           FDR=p.adjust(p.value,method="fdr"),
           FE=(Class_pass/Class_size)/(Total_pass/Total_size),
           logFDR=ifelse(-log10(FDR)<0,0,-log10(FDR)),
           logp=ifelse(-log10(p)<0,0,-log10(p)),
           logpbin=cut(logp,breaks=Sbrks,labels=Slbls,right=FALSE)) %>%
    ungroup %>%
    mutate(Type=factor(Type,levels=c("All","Up","Down")))
}
