#' Draw heatmap
#'
#' @param data Data used for heatmap
#' @param ID Observations in columns
#' @param feature feature in rows
#' @param value Value to be used in heatmap
#' @param info Table with descriptors of interest
#' @param grouping Groups of observations for ellipses
#' @param scalink Scale feature for hierarchical clustering?
#' @param dmethod Distance measure for hierarchical clustering
#' @param cmethod Clustering method for hierarchical clustering
#' @keywords PCAprep
#' @export
#' @examples
#' PCAprep(data,ID,feature,value,info,cols=list(),scalesel='row',rnames=FALSE,reorder.rows=TRUE,reorder.columns=TRUE,name='Z-score',title="Heatmap",cdr="euclidean",cdc="euclidean",cmr="ward.D2",cmc="ward.D2",rmax=10)
PCAprep<-function(data,ID,feature,value,info,grouping="All",scaling=TRUE,dmethod="euclidean",cmethod="ward.D2") {
  
  pcashape<-data %>%
    select_(ID,feature,value) %>%
    spread_(feature,value) %>%
    data.frame(check.names=FALSE)
  
  rownames(pcashape)<-pcashape[,ID]
  pcashape[,ID]<-NULL
  
  if (scaling) {
    hpcashape<-scale(pcashape)
  } else {
    hpcashape<-pcashape
  }
  
  HC<-dist(hpcashape,method=dmethod) %>% hclust(method=cmethod)
  
  pca <- prcomp(pcashape,
                center = TRUE,
                scale = TRUE)
  
  pcadata<-data.frame(pca$x) %>%
    rownames_to_column(ID) %>%
    left_join(info)
  
  if (grouping=="All") {
    groups<-base::setdiff(colnames(info),ID)
  } else {
    groups<-grouping
  }
  
  
  pcaresult<-summary(pca)$importance
  PC1prc<-round(pcaresult['Proportion of Variance',][[1]]*100,0)
  PC2prc<-round(pcaresult['Proportion of Variance',][[2]]*100,0)
  PC3prc<-round(pcaresult['Proportion of Variance',][[3]]*100,0)
  PC4prc<-round(pcaresult['Proportion of Variance',][[4]]*100,0)
  
  ellipses<-pcadata %>%
    group_by(.dots=groups) %>%
    do(getellipse(.$PC1,.$PC2,1) ) %>%
    data.frame
  
  return(list("PC1prc"=PC1prc,"PC2prc"=PC2prc,
              "PC3prc"=PC3prc,"PC4prc"=PC4prc,
              "Loadings"=pca[2],"pca"=pca,
              "Ellipses"=ellipses,'HC'=HC,
              "pcadata"=pcadata,"pcashape"=pcashape))
}
