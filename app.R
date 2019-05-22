
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



# 1. Data ----------------------------------------------------------------

df <- read_delim("~/cepesp/cepesp_carreiras/df.txt", 
                 ";", escape_double = FALSE, trim_ws = TRUE)

df <- df %>% 
  arrange(desc(`Ano da Eleição`)) %>% 
  na.omit()

df$`Nome de urna (última eleição)` <- str_to_title(df$`Nome de urna (última eleição)`)

df$`Situação de Totalização do Turno` <- str_to_title(df$`Situação de Totalização do Turno`)


df$`Nome de urna` <- paste0(df$`Nome de urna (última eleição)`," ","(", df$`Último Partido`,
                            "-",df$`Última UE`,")")

df <- df %>% 
  dplyr::arrange(`Nome de urna`, `Sigla do Partido`) 

df$`Número do Título Eleitoral` <- as.numeric(df$`Número do Título Eleitoral`) 
 


# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  
  
  navbarPage("CepespCarreiras", theme = shinytheme("flatly"),
             
             
             tabPanel("Eleições",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),br(), width = 3,
                                     
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
             
             tabPanel("Sobre")
             
             
  ),

tags$footer("© 2019 CEPESP Todos os direitos reservados.", align = "left", style = "
              position:absolute;
              bottom:0;
              width:25%;
              height:40px;   /* Height of the footer */
              color: #2c3e50;
              padding: 10px;
              background-color: white;
              z-index: 1000;")

  )




# 3. Server ---------------------------------------------------------------

server <- function(input, output){
  
  nomes <- reactive({
   df$`Número do Título Eleitoral` = df$`Nome de urna`
  })
  
  
  
  # Selecao do partido  
  
  output$PARTIDO <- renderUI({
    ue <- req(input$UE)
   if(ue == "Todas as unidades eleitorais"){
      selectizeInput(inputId = "PARTIDO",
                     label = NULL,
                     choices = c("", "AVANTE", "DC", "DEM", "MDB","NOVO", "PAN", "PATRI", "PC do B", 
                                 "PCB", "PCO", "PDT", "PEN", "PFL", "PGT", "PHS", "PL", "PMB",
                                 "PMDB", "PMN", "PODE", "PP", "PPB", "PPL", "PPS", "PR", "PRB", 
                                 "PRN", "PRONA", "PROS", "PRP", "PRTB", "PSB", "PSC", "PSD", "PSDB", 
                                 "PSDC", "PSL", "PSN", "PSOL", "PST", "PSTU", "PT", "PT do B", "PTB",
                                 "PTC", "PTN", "PV", "REDE", "SD", "SOLIDARIEDADE"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha a sigla do partido'))
    } else{
      selectizeInput(inputId = "PARTIDO",
                     label = NULL,
                     choices = c("", "Todos os partidos",
                                 df[df$`Sigla da Unidade Eleitoral`== req(input$UE), "Sigla do Partido"]),
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
                     choices = c("",df[df$`Sigla da Unidade Eleitoral`== req(input$UE), "Nome de urna"]),
                     selected = NULL,
                     options = list(placeholder = 'Digite o nome do candidato'))
    }
    else if(ue == "Todas as unidades eleitorais" & partido == req(input$PARTIDO)){
      selectizeInput(inputId = "CANDIDATO",
                     label = NULL,
                     choices = c("",df[df$`Sigla do Partido` == req(input$PARTIDO), "Nome de urna"]),
                     selected = NULL,
                     options = list(placeholder = 'Digite o nome do candidato'))
    } else {
      selectizeInput(inputId = "CANDIDATO",
                     label = NULL,
                     
                     choices = c("",df[df$`Sigla da Unidade Eleitoral`== req(input$UE) & 
                                    df$`Sigla do Partido` == req(input$PARTIDO), "Nome de urna"]),
                     selected = NULL,
                     options = list(placeholder = 'Digite o nome do candidato'))
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
              dplyr::filter(`Nome de urna` == req(input$CANDIDATO)) %>% 
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
