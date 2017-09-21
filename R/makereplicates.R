#' Make a vector with unique replicate indexes
#'
#' @param data Table containing columns over which unique repicate indices need to be generated
#' @keywords makereplicates
#' @export
#' @examples
#'


makereplicates<-function(data){
  data$ReplicateUniq<-1
  alluniq<-FALSE
  while(!alluniq){
    print(max(data[,'ReplicateUniq']))
    duplics<-duplicated(data)
    alluniq<-all(duplics==FALSE)
    if (!alluniq) {
      data[duplics,'ReplicateUniq']<-max(data[duplics,'ReplicateUniq'])+1
    }
  }
  return(data$ReplicateUniq)
}


