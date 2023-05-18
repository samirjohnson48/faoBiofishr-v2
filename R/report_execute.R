#' Execute report
#'
#' This function execute report
#'
#' @param content list of object produce by report_content()
#' @param format type of output
#' @return report document
#' @export

report_execute<-function(content,format="pdf"){
  template_file<-system.file("extdata","report_template.Rmd", package="faoBiofishr")
  return (rmarkdown::render(template_file, output_file = file.path(content$path$report,"biodiversity_trends_report_test.pdf"),output_format = "pdf_document", params = list(content)))
}
