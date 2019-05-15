library(shiny)
library(httr)
library(dplyr)

ui <- fluidPage(
  dataTableOutput("tbl")
)

server <- function(input, output, session) {
  output$tbl <- renderDataTable( {
    # GET request from an API
    req <- httr::GET(url = "cepesp.io/api/consulta/tse?ano=2018&cargo=1&agregacao_regional=0&agregacao_politica=2&ignore_version=true")
    req_parsed <- httr::content(req)
    
    # Convert to data.frame
    dplyr::bind_rows(req_parsed)
  })
}

shinyApp(ui, server)