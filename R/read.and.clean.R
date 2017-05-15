#' Read Excel and csv files
#'
#' @param fname Table containing sampels in rows and variables in columns
#' @param sheet Sheet to be used
#' @param sep Separator in csv file. DEFAULTS to ","
#' @keywords read.and.clean
#' @export
#' @examples
#' read.and.clean()

read.and.clean<-function(fname,sheet='',sep=',') {
  print(fname)
  #print(grepl('.xlsx',fname))
  #print(grepl('.csv',fname))
  if (grepl('xlsx',fname)) {
    dft<-read.xlsx2(fname,sheetName = sheet,
                    stringsAsFactors = FALSE,
                    header=FALSE)
  } else if (grepl('csv',fname)) {
    dft<-read.csv(fname,sep=sep,quote = "\"",stringsAsFactors = FALSE,
                  header=FALSE)
    #dft[1,which(dft[1,] == "")] <- "Metabolite Set"
  }
  #print(length(colnames(dft)))
  colnames(dft)<-dft[1,]
  df<-dft[-1,]
  
  return(df)
}
