#' Calculate enrichment
#'
#' @param data Table with data
#' @param groups Groups for which enrichment is to be calculated. Can be column name string or vector of such names
#' @param featureid Unique feature ID column 
#' @param Sbrks Enrichment breaks in -log10 scale
#' @param Slbls Enrichment labels (one less than breaks)
#' @keywords enrichment
#' @export
#' @examples
#'

enrichment<-function(data,groups,featureid,
                      Sbrks=c(0,-log(0.05,10),2,3,4,1000),
                      Slbls=c('N.S.','<0.05','<0.01','<0.001','<0.0001'),
                      enrtype="regular"){
  
  print(paste0("Performing ",enrtype," enrichment analysis!"))
  
  grouping<-group_vars(data)
  
  print(paste("Grouping: ", paste(grouping,collapse=', ') ))
  print(paste("Groups: ", paste(groups,collapse=', ') ))
  
  prep<-data %>%
    mutate(Up=logFC>0 & FDR <0.05,
           Down=logFC<0 & FDR <0.05,
           All=FDR<0.05) %>%
    #gather different thresholds
    gather(Type,Pass,Up,Down,All)
  
  
  
  print("Direction of change breakdown done!")
  
  #Removes duplicated instances
  
  uniques<-prep %>% 
    group_by_(.dots=c(grouping,featureid,"Pass","Type")) %>%
    summarise %>%
    group_by_(.dots=c(grouping,"Type")) %>%
    summarise(Unique_size=n(),
              Unique_pass=sum(Pass,na.rm = TRUE) ) %>%
    ungroup
  
  print("Calculating unique changes done!")
  
  enrichment<-prep %>%
    group_by_(.dots=c(grouping,"Type")) %>%
    mutate(Total_size=n(),
           Total_pass=sum(Pass,na.rm = TRUE)) %>%
    left_join(uniques) %>%
    group_by_(.dots=c(grouping,"Type",groups,"Total_size","Total_pass","Unique_size","Unique_pass") ) %>%
    summarise(Class_size=n(),
              Class_pass=sum(Pass,na.rm = TRUE)) %>%
    group_by_(.dots=c(grouping,"Type")) %>% #Group by both the initial grouping and type of change for correct FDR adjustment
    # Class_pass-1 because we are interested in what's the probability of drawing Class_pass or more. Otherwise, it would be Class_pass+1 or more
    #Good explanation at: https://blog.alexlenail.me/understanding-and-implementing-the-hypergeometric-test-in-python-a7db688a7458
    mutate(p.value=case_when(enrtype=="regular" ~ phyper(Class_pass-1,Unique_pass,Unique_size-Unique_pass,Class_size,lower.tail=FALSE),
                             TRUE ~ phyper(Class_pass-1,Total_pass,Total_size-Total_pass,Class_size,lower.tail=FALSE)
                             ) ,
           FE=case_when(enrtype=="regular" ~ (Class_pass/Class_size)/(Unique_pass/Unique_size),
                        TRUE ~  (Class_pass/Class_size)/(Total_pass/Total_size)
                         ) ,
           FDR=p.adjust(p.value,method="fdr"),
           logFDR=ifelse(-log10(FDR)<0,0,-log10(FDR)),
           logp=ifelse(-log10(p.value)<0,0,-log10(p.value)),
           logpbin=cut(logp,breaks=Sbrks,labels=Slbls,right=FALSE),
           logFDRbin=cut(logFDR,breaks=Sbrks,labels=Slbls,right=FALSE)) %>%
    ungroup %>%
    mutate(Type=factor(Type,levels=c("All","Up","Down")))
  
  return(enrichment)
}
