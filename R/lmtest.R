#' Simple linear modelling with 2-way interaction
#'
#' @param data Table containing samples in rows and variables in columns
#' @param formula Formula for linear model in the form of "Values~Factor1*Factor2"
#' @keywords hypothesise
#' @export
#' @examples
#' lmtest


lmtest<-function(data,formula) {
  
  val<-formula %>% str_split("~") %>% unlist(.) %>% first()
  facs<-formula %>% str_split("~") %>% unlist(.) %>% nth(2) %>% str_split("\\*") %>% unlist
  fac1<-facs[1]
  fac2<-facs[2]
  
  facregex<-paste(fac1,fac2,sep="|")
  
  groupings<-group_vars(data)
  
  #Effect of first factor
  stat1<-data %>%
    group_by_(.dots=c(groupings,fac2)) %>%
    do(broom::tidy(lm(as.formula(paste(val,fac1,sep="~")),data=.)) ) %>%
    ungroup %>%
    filter(term!='(Intercept)') %>%
    mutate(term=str_replace_all(term,facregex,""),
           Contrast_type=fac1 )%>%
    rename_(.dots=setNames("term",fac1))
  
  
  #Effect of second factor
  stat2<-data %>%
    group_by_(.dots=c(groupings,fac1)) %>%
    do(broom::tidy(lm(as.formula(paste(val,fac2,sep="~")),data=.)) ) %>%
    ungroup %>%
    filter(term!='(Intercept)') %>%
    mutate(term=str_replace_all(term,facregex,""),
           Contrast_type=fac2)%>%
    rename_(.dots=setNames("term",fac2))
  
  #Effect of interaction
  stat3<-data %>%
    do(broom::tidy(lm(as.formula(formula),data=.)) ) %>%
    ungroup %>%
    filter(str_detect(term,":")) %>%
    mutate(term=str_replace_all(term,facregex,""),
           Contrast_type="Interaction")%>%
    separate(term,c(fac1,fac2))
  
  stat<-stat1 %>%
    rbind(stat2) %>%
    rbind(stat3) %>%
    rename(SE=std.error,
           logFC=estimate) %>%
    mutate(PE=logFC+SE,
           NE=logFC-SE,
           Prc=2^logFC*100,
           PrcNE=2^NE*100,
           PrcPE=2^PE*100,
           pStars=pStars(p.value)) %>%
    mutate_at(c(fac1,fac2,"Contrast_type"),as.factor) %>%
    select(one_of(groupings),"Contrast_type",fac1,fac2,everything())
  
  return(stat)
}


