#' Get formated results from LM dataset
#'
#' @param data LM output (do not ungroup it!)
#' @param contrasts.desc Contrast descriptors
#' @keywords results
#' @export
#' @examples
#' getresults(data,contrasts.desc)
#'
#'

getresults<-function(data,contrasts.desc) {
  grp.vars<-group_vars(data)
  cnt.vars<-colnames(contrasts.desc)

  results<-contrasts.desc %>%
    mutate(Contrast=as.character(Contrast)) %>%
    right_join(data,by='Contrast') %>%
    #Adjustments within contrast
    adjustments %>%
    #Set contrast levels
    mutate(Contrast=factor(Contrast,levels=contrasts.desc$Contrast,labels=contrasts.desc$Contrast),
           Description=factor(Description,levels=contrasts.desc$Description,labels=contrasts.desc$Description)) %>%
    select(cnt.vars,grp.vars,everything())

  results.m<-results %>%
    gather(Stat,Value,logFC:logFDR)

  results.castfull<-results.m %>%
    arrange(Contrast,desc(Stat)) %>%
    unite(CS,Contrast,Stat) %>%
    select(grp.vars,CS,Value) %>%
    spread(CS,Value) %>%
    mutate_at(vars(-contains('Stars'),-one_of(grp.vars)),as.numeric)

  results.cast<-results.m %>%
    filter(Stat %in% c('logFC','FDR')) %>%
    arrange(Contrast,desc(Stat)) %>%
    unite(CS,Contrast,Stat) %>%
    select(grp.vars,CS,Value) %>%
    spread(CS,Value) %>%
    mutate_at(vars(-contains('Stars'),-one_of(grp.vars)),as.numeric)

  return(list('results'=results,'cast'=results.cast,'castfull'=results.castfull))

}
