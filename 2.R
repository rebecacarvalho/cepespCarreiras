
require(shiny)
require(httr)

url <- "/api/consulta/athena/status?id=<ID>/get"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput('GETargs',"GET string"),
      actionButton('sendGET','Send')
    ),
    mainPanel(
      verbatimTextOutput('GETresponse'),
      dataTableOutput('table1')
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$sendGET, {
    getargs <- input$GETargs
    
    if( is.null(input$GETargs) ) getargs <- ""
    
    res                <- GET(sprintf("%s%s",url, getargs))
    output$GETresponse <- renderPrint(content(res))
    
    output$table1 <- renderDataTable( as.data.frame(content(res)$args) )
  })
}

runApp(shinyApp(ui,server))