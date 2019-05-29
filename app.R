
# Titulo: Shiny do aplicativo de Carreiras
# Autor: Rebeca Carvalho


rm(list = ls())

# Pacotes utilizados


library(cepespR)
library(knitr)
library(plyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shiny)
library(readr)
library(shiny)
library(shinythemes)
library(magrittr)
library(plotly)
library(DT)
library(R.utils)


# 1. Data ----------------------------------------------------------------

df <- read_rds("df.rds")


# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  
  tags$head(title = "CepespCarreiras",
    tags$style(HTML(".navbar .navbar-nav {float: left}
          .navbar .navbar-header {float: right}"))),
  
  title = "CEPESP Carreiras", 
  
  
  navbarPage(
    
    tags$div(class = "header", checked = NA,
             
             tags$a(href = "http://www.cepesp.io/cepesp-data/", class = 
                      "w-hidden-medium w-hidden-small w-hidden-tiny" ,"CEPESP Data",
                    style = 
                    "top: -2px;   
                    background-color:white;
                    border-bottom-color:#1897d5;
                    right: 0;
                    color: #1897d5;
                    width: 170px;
                    padding: 10px 15px; 
                    padding-top: 10px;
                    padding-right: 15px;
                    padding-bottom: 10px;
                    padding-left: 15px;
                    border-style: none solid solid;
                    border-width: 0 1px 1px;
                    border-radius: 0 0 3px 3px;
                    border-color: #1897d5;
                    border-image-source: initial;
                    border-image-slice: initial;
                    border-image-width: initial;
                    border-image-outset: initial;
                    border-image-repeat: initial;
                    text-align: center;
                    vertical-align: middle;
                    text-decoration: none;
                    text-decoration-line: none;
                    text-decoration-style: initial;
                    text-decoration-color: initial;
                    user-select: none;
                    border: 1px solid transparent;
                    color: #1897d5;
                    font-size: 14px;
                    font-weight: bold;
                    line-height: 12px;
                    text-transform: uppercase;
                    display: inline-block!important;
                    display: flex;
                    flex-direction: column;
                    padding-left: 0;
                    margin-bottom: 0;
                    font-family: Gotham,Open Sans,sans-serif!important;
                    box-sizing: border-box;
                    transition-property: color, border-color, box-shadow;
                    transition-duration: 0.15s, 0.15s, 0.15s, 0.15s;
                    -timing-function: ease-in-out, ease-in-out, ease-in-out, ease-in-out;
                    transition-delay: 0s, 0s, 0s, 0s;
                    webkit-tap-highlight-color: rgba(0,0,0,0);
                    list-style: none;
                    list-style-type: none;
                    list-style-position: initial;
                    list-style-image: initial"
                    
                                      )),
    
    id = "CepespCarreiras", theme = shinytheme("flatly"),
             
             
             tabPanel("Eleições",
                      
                      
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"), width = 3,
                                     
                                     selectizeInput(inputId = "UE",
                                                    label = NULL,
                                                    choices = c("","Todas as unidades eleitorais", "AC", 
                                                                "AL", "AM", "AP", "BA", "BR", "CE", "DF", "ES",
                                                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", 
                                                                "PI","PR", "RJ", "RN", "RO", "RR", "RS", "SC", 
                                                                "SE", "SP", "TO"),
                                                    selected = NULL, 
                                                    options = list(placeholder = 'Escolha uma unidade eleitoral')),
                                     
                                     uiOutput("PARTIDO"),
                                     
                                     uiOutput("CANDIDATO"),
                                     
                                     actionButton(inputId = "BAL1",
                                                  label = strong("Atualizar"),
                                                  width = "95%")
                                     
                        ),
                        
                        mainPanel(
                          absolutePanel(top = 0, right = 0, left = 100),
                          tags$style(type = "text/css",
                                     ".dataTables_filter, .dataTables_info { display: none; }",
                                     ".dataTable( {'lengthChange': false});"),
                          br(),
                          dataTableOutput("perfil"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          dataTableOutput("eleicoes")
                        ))),
             
             tabPanel("Sobre", htmlOutput("Note"))))
             
             
  




# 3. Server ---------------------------------------------------------------

server <- function(input, output){
  
  #Sobre
  
  output$Note <- renderUI({
    note <- paste0("
                   <font size='3'> 
                   O CepespCarreiras foi criado a partir de dados do TSE, tratados e agregados pela equipe do CepespData, 
                   permitindo a consulta aos cargos já concorridos e ocupados por um mesmo político.  Atualmente, inclui 
                   as eleições nacionais e estaduais (1998 a 2018). As eleições a nível municipal serão incorporadas futuramente.</font>")
    HTML(note)
  })
  
  
  
  # Selecao do partido  
  
  output$PARTIDO <- renderUI({
    ue <- req(input$UE)
   if(ue == "Todas as unidades eleitorais"){
      selectizeInput(inputId = "PARTIDO",
                     label = NULL,
                     choices = unique(c("", "AVANTE", "DC", "DEM", "MDB","NOVO", "PAN", "PATRI", "PC do B", 
                                 "PCB", "PCO", "PDT", "PEN", "PFL", "PGT", "PHS", "PL", "PMB",
                                 "PMDB", "PMN", "PODE", "PP", "PPB", "PPL", "PPS", "PR", "PRB", 
                                 "PRN", "PRONA", "PROS", "PRP", "PRTB", "PSB", "PSC", "PSD", "PSDB", 
                                 "PSDC", "PSL", "PSN", "PSOL", "PST", "PSTU", "PT", "PT do B", "PTB",
                                 "PTC", "PTN", "PV", "REDE", "SD", "SOLIDARIEDADE")),
                     selected = NULL,
                     options = list(placeholder = 'Escolha a sigla do partido'))
    } else{
      selectizeInput(inputId = "PARTIDO",
                     label = NULL,
                     choices = c("", "Todos os partidos",
                                 unique(df[df$`Sigla da Unidade Eleitoral`== req(input$UE), "Sigla do Partido"])),
                     selected = NULL,
                     options = list(placeholder = 'Escolha a sigla do partido'))
    }
  })
  
 
  # Selecao do candidato  
  
  
  
  output$CANDIDATO <- renderUI({
    ue <- req(input$UE)
    partido <- req(input$PARTIDO)
    if(ue == req(input$UE) & partido == "Todos os partidos"){
      selectizeInput(inputId = "CANDIDATO",
                     label = NULL,
                     choices = c("",unique(df[df$`Sigla da Unidade Eleitoral`== req(input$UE), "Nome de urna"])),
                     selected = NULL,
                     options = list(placeholder = 'Digite o nome do candidato'))
    }
    else if(ue == "Todas as unidades eleitorais" & partido == req(input$PARTIDO)){
      selectizeInput(inputId = "CANDIDATO",
                     label = NULL,
                     choices = c("",unique(df[df$`Sigla do Partido` == req(input$PARTIDO), "Nome de urna"])),
                     selected = NULL,
                     options = list(placeholder = 'Digite o nome do candidato'))
    } else if(ue == req(input$UE) & partido == req(input$PARTIDO)){
      selectizeInput(inputId = "CANDIDATO",
                     label = NULL,
                     choices = # Aqui estão os candidatos quando escolhido UE e partido
                       if(nrow( # Quando o número de candidatos no distrito escolhido (req(input$UE)) for igual...
                         unique(df[df$`Sigla da Unidade Eleitoral`== input$UE & df$`Sigla do Partido` == input$PARTIDO,])
                         )==1){ # a 1, 
                      c(rbind("", # crie um candidato extra chamado "", além dos candidatxs reais
                      unique(df[df$`Sigla da Unidade Eleitoral`== input$UE & df$`Sigla do Partido` == input$PARTIDO, "Nome de urna"])))                   
                      }
                          else{ # Nos outros casos, o código é igual às outras condições
                            c("",unique(df[df$`Sigla da Unidade Eleitoral`== input$UE & df$`Sigla do Partido` == input$PARTIDO, "Nome de urna"]))
                          },
                     selected = NULL,
                     options = list(placeholder = 'Digite o nome do candidato'))
    } 
    else{
      return(NULL)
    }
  })
  
 
      
# 3.1. Tabelas ------------------------------------------------------------  
      
      
# 3.1.2. Perfil do candidato  ---------------------------------------------------------
      
      
      output$perfil <- DT::renderDataTable(
        bperfil()
      )
      
      
# 3.1.2. Eleicoes ---------------------------------------------------------      
      
      output$eleicoes <- DT::renderDataTable(
        beleicoes()
        
      )
      
 
      
# 3.2. Botao de acao ------------------------------------------------------
      
# 3.2.1. Perfil do candidato -----------------------------------------------------------
      
      
      
      bperfil <- eventReactive(input$BAL1, {
        datatable(options = list(dom = 't', paging = FALSE, ordering = FALSE),{
          ue <- req(input$UE)
          partido <- req(input$PARTIDO)
          candidato <- req(input$CANDIDATO)
          if(ue == "Todas as unidades eleitorais" & partido == req(input$PARTIDO)){
            df %>% 
              dplyr::filter(`Nome de urna`== req(input$CANDIDATO)) %>% 
              dplyr::select(Nome, CPF, `Número do Título Eleitoral`, Sexo, `Cor ou Raça`, `Grau de Instrução`, `Ocupação`,
                     `Estado Civil`, Nacionalidade, `Estado de Nascimento`, `Município de Nascimento`) %>% 
              unique()
            
          } 
          else if(req(ue == input$UE) & partido == "Todos os partidos"){
            df %>% 
              dplyr::filter(`Nome de urna` == req(input$CANDIDATO)) %>% 
              dplyr::select(Nome, CPF, `Número do Título Eleitoral`, Sexo, `Cor ou Raça`, `Grau de Instrução`, `Ocupação`,
                     `Estado Civil`, Nacionalidade, `Estado de Nascimento`, `Município de Nascimento`) %>% 
              unique()
          } else {
            df %>% 
              dplyr::filter(`Nome de urna`== req(input$CANDIDATO)) %>% 
              dplyr::select(Nome, CPF, `Número do Título Eleitoral`, Sexo, `Cor ou Raça`, `Grau de Instrução`, `Ocupação`,
                     `Estado Civil`, Nacionalidade, `Estado de Nascimento`, `Município de Nascimento`) %>% 
              unique()
          }    
        })
      })
      
# 3.2.2. Eleicoes -----------------------------------------------------------     
      
      
      beleicoes <- eventReactive(input$BAL1, {
        datatable(options = list(dom = 't', paging = FALSE, ordering = FALSE),{
          ue <- req(input$UE)
          partido <- req(input$PARTIDO)
          candidato <- req(input$CANDIDATO)
         if(ue == "Todas as unidades eleitorais" & req(partido == input$PARTIDO)){
            df %>% 
              dplyr::filter(`Nome de urna` == req(input$CANDIDATO)) %>% 
              dplyr::select(`Ano da Eleição`, `Nº do Turno`, Cargo, `Sigla da Unidade Eleitoral`, `Situação da Candidatura`, 
                     `Situação de Totalização do Turno`,`Número de urna`, `Sigla do Partido`,
                     `Composição da Coligação`, Votos)
          }
          else if(ue == req(input$UE) & partido == "Todos os partidos"){
            df %>% 
              dplyr::filter(`Nome de urna` == req(input$CANDIDATO)) %>% 
              dplyr::select(`Ano da Eleição`, `Nº do Turno`, Cargo, `Sigla da Unidade Eleitoral`, `Situação da Candidatura`,
                     `Situação de Totalização do Turno`,`Número de urna`, `Sigla do Partido`,
                     `Composição da Coligação`, Votos)
          } else{
            df %>% 
              dplyr::filter(`Nome de urna` == req(input$CANDIDATO)) %>%  
              dplyr::select(`Ano da Eleição`, `Nº do Turno`, Cargo, `Sigla da Unidade Eleitoral`, `Situação da Candidatura`,
                     `Situação de Totalização do Turno`,`Número de urna`,`Sigla do Partido`,
                     `Composição da Coligação`, Votos)
            
          }
        })
      })
      
}




# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)
