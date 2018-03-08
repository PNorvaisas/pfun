#' Get formated results from LM dataset
#'
#' @param data LM hypothesise output (do not ungroup it!)
#' @param contrasts.desc Contrast descriptors
#' @keywords results
#' @export
#' @examples
#' getresults(data,contrasts.desc)
#'
#'

getresults<-function(data,contrasts.desc,groupings=c()) {
  grp.vars<-group_vars(data)
  cnt.vars<-colnames(contrasts.desc)

  results<-contrasts.desc %>%
    mutate(Contrast=as.character(Contrast)) %>%
    right_join(data,by='Contrast') %>%
    #group_by_(groupings c('Contrast')) %>%
    #Adjustments within contrast and original grouping
    adjustments(groupings) %>%
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


  cnt.vals<-base::setdiff(colnames(results),c(cnt.vars,grp.vars))

  results.multi<-results %>%
    rename_(.dots = setNames(cnt.vals, paste0('x_',cnt.vals))) %>%
    full_join(results %>%
                rename_(.dots = setNames(cnt.vals, paste0('y_',cnt.vals)))) %>%
    full_join(results %>%
                rename_(.dots = setNames(cnt.vals, paste0('z_',cnt.vals)))) %>%
    select(grp.vars,everything())


  return(list('results'=results,
              'cast'=results.cast,
              'castfull'=results.castfull,
              'multi'=results.multi))

}
