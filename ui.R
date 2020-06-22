# Titulo: Shiny do aplicativo de Carreiras
# Autor: Rebeca Carvalho



# Objetivos

#'         - Criar uma interface que seja amigável ao usuário.




# 1. User interface -------------------------------------------------------


ui <- fluidPage(
    
    useShinyjs(),
    
    tags$style(HTML(
                    ".container-fluid {
                    height: 60px;}
                    
                    .col-sm-10 {
                    padding-right: 0px;
                    padding-left: 200px;}
                    
                    .tab-content{
                    diplay: none;
                    
                    
                    }")),
    
    tags$head(includeCSS("styles.css")),
    
    tags$div(class = "btn-header", 
             checked = NA,
             tags$a(href = "http://cepespdata.io/", 
                    class="btn btn-primary", 
                    "CEPESP Data")),
    
    
    title = "CEPESP Carreiras", 
    
    
    navbarPage(title = "CEPESP Carreiras",
                   theme = shinytheme("flatly"),
        
        
               tabPanel("Eleições",
                        
                        
                        fluidRow(
                        div(style="display:inline;
                                   padding-right: 50px;",
                                
                                        column(10, align="center", 
                                            autocomplete_input(
                                                        id = "CANDIDATO",
                                                        label = NULL,
                                                        options = candidatos,
                                                        width = "100%",
                                                        max_options = 20,
                                                        placeholder = 'Digite o nome do candidato')),
            
                                    column(1,
                                        actionBttn(inputId = "BCAL1",
                                                   color = "default",
                                                   icon = icon("search"), 
                                                   style = "material-flat",
                                                   size = "md"),
                                                   block = TRUE,
                                                   no_outline = TRUE))),
                        
                        
                        tags$style(type = "text/css",
                                   ".dataTables_filter, .dataTables_info { display: none;}",
                                   ".dataTable( {'lengthChange': false});"),
                        br(),
                        column(11,
                               absolutePanel(top = 50, 
                                             right = 0,
                                             left = 100,
                        dataTableOutput("perfil"))),
                        
                        column(11,
                               absolutePanel(top = 400,
                                            left = 100,
                        dataTableOutput("eleicoes")))),
                                
                                         
                            
        
        tabPanel("Sobre", htmlOutput("Note"))),
        
        add_busy_spinner(spin = "fulfilling-bouncing-circle",
                         position = "top-right",
                         margins = c(300, 650)),
        
        
        tags$footer(class = "rodape",
                    
                    style =
                        
                        "max-width: 100%;
              noprint: none; 
              padding: 10px 0;
              min-height: 40px;
              position: relative;
              clear: both;
              background-color: #222d32;;
              color: #fff;
              font-family: 'Segoe UI';
              font-size: 14px;
              text-align: left;
              z-index: 999999;
              height: 3em;
              margin-top: 90em;",
                    
                    tags$div(class = "rodape-container",
                             
                             style =
                                 
                                 "max-width: 960px;
                                  margin: 0 auto;
                                  position: relative;
                                  display: flex;
                                  flex-wrap: wrap;
                                  box-sizing: border-box;
                                  padding: 0;",
                             
                             
                             tags$div(class = "rodape-texto", "© 2020 CEPESP Todos os direitos reservados.",
                                      
                                      style = 
                                          
                                          "
                                           max-width: 50%;
                                           align: left;
                                           flex: 1 1 200px;
                                           display: flex;
                                           padding-left: 5%;
                                           padding-top: 10px;
                                           font-size: .9em;
                                           box-sizing: border-box;
                                           margin: 0;
                                           padding: 0;"))))


