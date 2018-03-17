#' Draw heatmap
#'
#' @param data Data used for heatmap
#' @param ID Observations in columns
#' @param feature feature in rows
#' @param value Value to be used in heatmap
#' @param info Table with descriptors of interest
#' @param cols Colorcoding list
#' @keywords clustorder
#' @export
#' @examples
#' HMap(data,ID,feature,value,info,cols=list(),scalesel='row',rnames=FALSE,reorder.rows=TRUE,reorder.columns=TRUE,name='Z-score',title="Heatmap",cdr="euclidean",cdc="euclidean",cmr="ward.D2",cmc="ward.D2",rmax=10)


HMap<-function(data,ID,feature,value,info,cols=list(),
               scalesel='row',rnames=FALSE,reorder.rows=TRUE,reorder.columns=TRUE,name='Z-score',
               cdr="euclidean",cdc="euclidean",cmr="ward.D2",cmc="ward.D2",rmax=10) {
  
  heatshape<-data %>%
    select(ID,feature,value) %>%
    spread_(ID,value) %>%
    data.frame
  
  rownames(heatshape)<-heatshape[,feature]
  heatshape[,feature]<-NULL
  
  #Order anotation by heatmap colnames
  hanot<-info[colnames(heatshape),,drop=FALSE]
  
  
  if (ID %in% colnames(hanot)){
    hanot<-hanot %>%
      select(-one_of(ID))
  }
  
  
  if (length(cols)>0) {
    ha<-ComplexHeatmap::HeatmapAnnotation(df=hanot, col = cols )
  } else {
    ha<-ComplexHeatmap::HeatmapAnnotation(df=hanot)
  }
  
  if (scalesel=='none') {
    heatmat<-as.matrix(heatshape)
  } else if (scalesel=='col') {
    heatmat<-scale(as.matrix(heatshape))
  } else if (scalesel=='row') {
    heatmat<-t(scale(t(as.matrix(heatshape))))
  } else {
    stop(paste0('Unknown scaling!: ',scalesel), call. = FALSE)
  }
  
  if (!rnames) {
    rownames(heatmat)<-NULL
  }
  
  ComplexHeatmap::Heatmap(heatmat,
          column_names_side = 'top',
          top_annotation = ha,
          name = name,
          clustering_distance_rows=cdr,
          clustering_distance_columns=cdc,
          clustering_method_rows=cmr,
          clustering_method_columns=cmc,
          row_dend_reorder = reorder.rows,
          column_dend_reorder = reorder.columns,
          row_names_max_width = unit(rmax, "cm"))
}
