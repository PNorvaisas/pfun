#' Calculate covariation and adjustments for contrasts
#'
#' @param adjustments Adjustment table with
#' @param datam Contrast table
#' @keywords covariation
#' @export
#' @examples
#' covariation()

covariation<-function(adjustments,datam,Outliers=TRUE,Plot=FALSE) {

  cont<-as.character(adjustments$Contrast)
  xcont<-as.character(adjustments$x)
  ycont<-as.character(adjustments$y)

  title<-paste0(cont,": ",ycont, " vs ",xcont)

  print(title)

  if (xcont %in% datam$x_Contrast & ycont %in% datam$y_Contrast) {
    seldata<-datam %>%
      filter(x_Contrast==xcont & y_Contrast==ycont)

    genfit<-lm(y_logFC~x_logFC,data=seldata,na.action = na.exclude)
    genres<-summary(genfit)

    if (Outliers) {
      car::outlierTest(genfit)
      ot<-car::outlierTest(genfit)

      outlist<-c(names(ot$rstudent))
      #Outliers:
      #print(paste('Outliers in ',grp,sep=''))
      #print(seldata[outlist,])

      seldataf<-subset(seldata,!rownames(seldata) %in% outlist)

      #Get final fit
      genfit<-lm(y_logFC~x_logFC,data=seldataf, na.action = na.exclude)
      genres<-summary(genfit)
    }

    if (Plot){
      car::qqPlot(genfit, main=title)
    }
    result<-data.frame('a'=genres$coefficients[[2]],
                       'neg_a'=-genres$coefficients[[2]],
                       'b'=genres$coefficients[[1]],
                       'a_p'=genres$coefficients[[8]],
                       'b_p'=genres$coefficients[[7]],
                       'r2'=genres$r.squared)

    return(result)
  } else {
    return(data.frame(NA) )
  }

}

