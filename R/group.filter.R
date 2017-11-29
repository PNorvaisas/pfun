#' Adjustment of contrasts
#'
#' @param dt edgeR DGEList object
#' @param thrs CPM theshold for groups
#' @keywords adjust.contrast
#' @export
#' @examples
#' group.filter()

group.filter<-function(dt,thrs) {
  dtcpm<-edgeR::cpm(dt)
  groups<-as.character(unique(dt$samples$group))
  allkeep<-apply(dtcpm,1, function(x) all(x>thrs))
  for (gr in groups){
    selection<-data.frame( rowSums( dtcpm[,dt$samples$group==gr] > thrs ) >= sum( dt$samples$group==gr ) )
    colnames(selection)<-gr
    allkeep <- cbind(allkeep, selection )
  }
  keep<-apply(allkeep,1,function(x) any(x))

  return(list('Allkeep'=allkeep,'Keep'=keep))
}
