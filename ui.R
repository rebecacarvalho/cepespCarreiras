# Titulo: Shiny do aplicativo de Carreiras
# Autor: Rebeca Carvalho



# Objetivos

#'         - Criar uma interface que seja amigável ao usuário.




# 1. User interface -------------------------------------------------------


ui <- fluidPage(
    
   
    
    useShinyjs(),
    
    tags$head(includeCSS("styles.css")),
    
    tags$head(tags$link(rel="shortcut icon", 
                        href="favicon_cepesp.ico",
                        type="image/vnd.microsoft.icon")),
    
    tags$div(class = "btn-header", 
             checked = NA,
             tags$a(id = "cepesp",
                    href = "http://cepespdata.io/",
                    class="btn btn-primary cepesp", 
                    "CEPESP DATA")),
    
    
    title = "CEPESP Carreiras", 
    
    
    navbarPage(title = div(tags$a(href = "http://www.cepesp.io/",
                                          img(src="logo_cepesp.png",
                                              style="width: 220px;
                                      height: 48px;"))),
               collapsible= TRUE,
               fluid = TRUE,
               id = "CEPESP Carreiras",
        
        
               tabPanel("ELEIÇÕES",
                        
                        
                        fluidRow(
                             column(10, align="center", 
                                            selectizeInput(
                                                        inputId =  "CANDIDATO",
                                                        label = NULL,
                                                        choices = c(""),
                                                        selected = NULL,
                                                        width = "100%",
                                                        options = list(
                                                                       placeholder = 'Digite o nome do candidato'))),
            
                                    column(1,
                                        actionBttn(inputId = "BCAL1",
                                                   color = "default",
                                                   icon = icon("search"), 
                                                   style = "material-flat",
                                                   size = "md"),
                                                   block = TRUE,
                                                   no_outline = TRUE)),
                        
                        
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
                               absolutePanel(top = 350,
                                            left = 100,
                        dataTableOutput("eleicoes")))),
                                
                                         
                            
        
        tabPanel("SOBRE", htmlOutput("Note"))),
        
        add_busy_spinner(spin = "fulfilling-bouncing-circle",
                         position = "top-right",
                         margins = c(300, 650)),
        
        
    tags$div(class="container-fluid p-4",
             img(src="logo_cepesp.png",
                 style="width: 220px;
        height: 48px;")),
    
    tags$footer(class = "rodape",
                tags$div(class = "rodape-container",
                         tags$div(class = "rodape-texto", 
                                  "© 2020 CEPESP Todos os direitos reservados."))))