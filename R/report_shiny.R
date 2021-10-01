#' Produce Shiny preview
#'
#' This function provide shiny preview of report to reedit it (not yef finished)
#'
#' @param content list of object produce by report_content()
#' @return shiny application
#' @export

report_shiny<-function(content){
  require(shiny)
  shinyApp(
  ui = fluidPage(
    useShinydashboard(),
    fluidRow(tags$h1("REPORT PREVIEW")),
    fluidRow(
      actionButton("button", "Produce report")
    ),
    fluidRow(tags$h2("INTRODUCTION")),
    fluidRow(
      box(title=NULL,width=12,collapsible = TRUE,
        column(6,textAreaInput("intro_text","",value=content$text$intro_text,cols=50,rows=50)),
        column(6,uiOutput("intro_text"))
      )
    ),
    fluidRow(tags$h3("Part 1: Commonness of species in FAO Fisheries and Aquaculture")),
    fluidRow(tags$h3("Commoness of Taxonomic Group record in FishStatJ")),
    fluidRow(tags$h3("Global Repartition")),
    fluidRow(tags$h4("FIGURE 1")),
    fluidRow(
      plotOutput('fig1')
    ),
    fluidRow(
      box(title=NULL,width=12,collapsible = TRUE,
        column(6,textAreaInput("text1","",value=content$text$text1,cols=50,rows=10)),
        column(6,uiOutput("text1"))
      )
    )
  ),

  server =function(input, output, session) {

    output$intro_text <- renderUI(HTML(markdown::markdownToHTML(text = input$intro_text)))
    output$fig1 <- renderPlot({content$fig$fig1})
    output$text1 <- renderUI(HTML(markdown::markdownToHTML(text = input$text1)))

    Content<-reactive({
      content$text$text1<-input$text1
      content$text$intro_text<-input$intro_text
      content})

    observeEvent(input$button, {
      report_execute(Content())
    })

  }
  )
}
