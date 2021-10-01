#' Manage all porcess of producing report
#'
#' This function is a report manager
#'
#' @param file Path to the input file
#' @param out Path to export destination
#' @param shiny.preview display a preview of report with shiny (not yet implemented)
#' @return report
#' @export

report_manager<- function(file,out=NULL,shiny.preview=FALSE,format="pdf"){
  content<-report_content(file=file,out=out)
  if(shiny.preview==FALSE){
    report_execute(content=content,format=format)
  }else{
    report_shiny(content=content)
  }
}
