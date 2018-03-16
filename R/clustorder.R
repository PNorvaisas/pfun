#' Order factors using hierarchical clustering
#'
#' @param data Data which will be used in clustering
#' @param features Features to cluster (rows in heatmap)
#' @param groups Groups for values which will be clustered (columns in heatmap)
#' @param value Value which will be used for clustering
#' @param scalesel Scaling ("row","col","none"), default: "none"
#' @param dst.method Distance method for clustering, default: 'euclidean'
#' @param cl.method Clustering method, default: 'ward.D2'
#' @param fill Value used to fill NAs
#' @param descending Reverse factor order?
#' @keywords clustorder
#' @export
#' @examples
#' clustorder(data,features,groups,value,dst.method='euclidean',cl.method='ward.D2',fill=0,descending=TRUE,scalesel='none')
#'
#'


clustorder<-function(data,features,groups,value,dst.method='euclidean',cl.method='ward.D2',fill=0,descending=TRUE,scalesel='none'){

  heatsum<-data %>%
    unite_("grpvar",from=groups,remove = FALSE) %>%
    select_(features,"grpvar",value) %>%
    spread_("grpvar",value) %>%
    data.frame

  heatsum[is.na(heatsum)]<-fill

  rownames(heatsum)<-heatsum[,features]
  heatsum[,features]<-NULL

  if (scalesel=='none') {
    heatmat<-as.matrix(heatsum)
  } else if (scalesel=='col') {
    heatmat<-scale(as.matrix(heatsum))
  } else if (scalesel=='row') {
    heatmat<-t(scale(t(as.matrix(heatsum))))
  } else {
    stop(paste0('Unknown scaling!: ',scalsel), call. = FALSE)
  }

  d<-dist(heatmat,method = dst.method)
  h<-hclust(d,method=cl.method)
  ordered<-rownames(heatsum[h$order,])
  if (descending){
    ordered<-rev(ordered)
  }

  data<-data %>%
    mutate_(.dots = setNames(paste0("factor(",features,", levels=c('",paste0(ordered,collapse="','"),"'))" ), features ))

  return(data)
}
