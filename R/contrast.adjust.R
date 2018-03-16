#' Calculate covariation and adjustments for contrasts
#'
#' @param contr.mat Contrast matrix with contrast names as rownames
#' @param adjustments Adjustment table generated with "covariation" function
#' @keywords contrast.adjust, adjust, contrast
#' @export
#' @examples
#' contrast.adjust()

contrast.adjust<-function(contr.mat,adjustments){
  contr.matrix<-contr.mat

  for (ci in 1:nrow(adjustments) ){
    cont<-as.character(adjustments[ci,"Contrast"])
    print(cont)
    xvar<-as.character(adjustments[ci,"x"])
    yvar<-as.character(adjustments[ci,"y"])

    if (xvar %in% colnames(contr.matrix) & cont %in% rownames(contr.matrix)) {
      contr.matrix[cont,xvar]<-adjustments[ci,'neg_a']
      contr.matrix[cont,'m']<-adjustments[ci,'b']
    } else if (xvar %in% rownames(contr.matrix) & yvar %in% rownames(contr.matrix) & cont %in% rownames(contr.matrix)) {
      diff<-contr.matrix[yvar,]-contr.matrix[xvar,]
      contr.matrix[cont,names(diff)]<-as.numeric(diff)
    }
  }
  return(contr.matrix)
}


