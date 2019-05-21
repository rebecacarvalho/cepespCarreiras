
# Titulo: Shiny do aplicativo de Carreiras
# Autor: Rebeca Carvalho


rm(list = ls())

# Pacotes utilizados

library(cepespR)
library(knitr)
library(plyr)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinyalert)
library(shinyBS)
library(ggplot2)
library(shiny)
library(readr)
library(shiny)
library(shinythemes)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(DT)



# 1. Data ----------------------------------------------------------------

df <- readRDS("df.rds")

df <- df %>% 
  arrange(desc(`Ano da Eleição`)) %>% 
  na.omit()

df$`Nome de urna (última eleição)` <- str_to_title(df$`Nome de urna (última eleição)`)

df$`Nome de urna` <- paste0(df$`Nome de urna (última eleição)`," ","(", df$`Sigla do Partido`,
                            "-",df$`Sigla da Unidade Eleitoral`,")")

df <- df %>% 
  dplyr::arrange(`Nome de urna`, `Sigla do Partido`)



# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  
  
  navbarPage("CepespCarreiras", theme = shinytheme("flatly"),
             
             
             tabPanel("Eleições",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),br(), width = 3,
                                     
                                     selectizeInput(inputId = "UE",
                                                    label = NULL,
                                                    choices = c("","Todas as unidades eleitorais", "AC", "AL", "AM", "AP", "BA", "BR", "CE", "DF", "ES",
                                                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI",
                                                                "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                                                    selected = NULL, 
                                                    options = list(placeholder = 'Escolha uma unidade eleitoral')),
                                     
                                     uiOutput("PARTIDO"),
                                     
                                     uiOutput("CANDIDATO"),
                                     
                                     actionButton(inputId = "BAL1",
                                                  label = strong("Atualizar"),
                                                  width = "95%")
                                     
                        ),
                        
                        mainPanel(
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"
                          ),
                          
                          absolutePanel(top = 0, right = 0, left = 100),
                          tags$style(type = "text/css",
                                     ".dataTables_filter, .dataTables_info { display: none; }",
                                     ".dataTable( {'lengthChange': false});"),
                          br(),
                          dataTableOutput("perfil", width = "110%"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          dataTableOutput("eleicoes", width = "110%")
                        ))),
             
             tabPanel("Sobre")
             
             
  ))




# 3. Server ---------------------------------------------------------------

server <- function(input, output)
{
  
  
  # Selecao do partido  
  
  output$PARTIDO <- renderUI({
    ue <- input$UE
    if(ue == "Todas as unidades eleitorais"){
      selectizeInput(inputId = "PARTIDO",
                     label = NULL,
                     choices = c("","Todos os partidos", "AVANTE", "DC", "DEM", "MDB","NOVO", "PAN", "PATRI", "PC do B", 
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
                     choices = c("", "Todos os partidos",df[df$`Sigla da Unidade Eleitoral`== input$UE, "Sigla do Partido"]),
                     selected = NULL,
                     options = list(placeholder = 'Escolha a sigla do partido'))
    }
  })
  
  
  
  
  # Selecao do candidato  
  
  
  
  output$CANDIDATO <- renderUI({
    ue <- input$UE
    partido <- input$PARTIDO
    if(ue == "Todas as unidades eleitorais" & partido == "Todos os partidos"){
      selectizeInput(inputId = "CANDIDATO",
                     label = NULL,
                     choices = unique(df$`Nome de urna`),
                     selected = NULL,
                     options = list(placeholder = 'Digite o nome do candidato'))
    } 
    else if(ue == input$UE & partido == "Todos os partidos"){
      selectizeInput(inputId = "CANDIDATO",
                     label = NULL,
                     choices = df[df$`Sigla da Unidade Eleitoral`== input$UE, "Nome de urna"],
                     selected = NULL,
                     options = list(placeholder = 'Digite o nome do candidato'))
    }
    else if(ue == "Todas as unidades eleitorais" & partido == input$PARTIDO){
      selectizeInput(inputId = "CANDIDATO",
                     label = NULL,
                     choices = df[df$`Sigla do Partido` == input$PARTIDO, "Nome de urna"],
                     selected = NULL,
                     options = list(placeholder = 'Digite o nome do candidato'))
    } else {
      selectizeInput(inputId = "CANDIDATO",
                     label = NULL,
                     
                     choices = df[df$`Sigla da Unidade Eleitoral`== input$UE & df$`Sigla do Partido` == input$PARTIDO, "Nome de urna"],
                     selected = NULL,
                     options = list(placeholder = 'Digite o nome do candidato'))
    }
  })  
      
      
      
      
      
      # 3.1. Tabelas ------------------------------------------------------------  
      
      
      
      # Perfil do Candidato
      
      
      output$perfil <- DT::renderDataTable(
        bperfil()
      )
      
      
      # Eleicoes
      
      output$eleicoes <- DT::renderDataTable(
        beleicoes()
        
      )
      
      
      
      # 3.2. Botao de acao ------------------------------------------------------
      
      bperfil <- eventReactive(input$BAL1, {
        datatable({
          ue <- input$UE
          partido <- input$PARTIDO
          candidato <- input$CANDIDATO
          if(ue == "Todas as unidades eleitorais" & partido == "Todos os partidos"){
            df %>% 
              filter(`Nome de urna` == input$CANDIDATO) %>% 
              select(Nome, CPF, `Número do Título Eleitoral`, Sexo, `Cor ou Raça`, `Grau de Instrução`, Ocupação,
                     `Estado Civil`, Nacionalidade, `Estado de Nascimento`, `Município de Nascimento`) %>% 
              unique()
          }
          else if(ue == "Todas as unidades eleitorais" & partido == input$PARTIDO){
            df %>% 
              filter(`Nome de urna` == input$CANDIDATO) %>% 
              select(Nome, CPF, `Número do Título Eleitoral`, Sexo, `Cor ou Raça`, `Grau de Instrução`, Ocupação,
                     `Estado Civil`, Nacionalidade, `Estado de Nascimento`, `Município de Nascimento`) %>% 
              unique()
            
          } 
          else if(ue == input$UE & partido == "Todos os partidos"){
            df %>% 
              filter(`Nome de urna` == input$CANDIDATO ) %>% 
              select(Nome, CPF, `Número do Título Eleitoral`, Sexo, `Cor ou Raça`, `Grau de Instrução`, Ocupação,
                     `Estado Civil`, Nacionalidade, `Estado de Nascimento`, `Município de Nascimento`) %>% 
              unique()
          } else {
            df %>% 
              filter(`Sigla da Unidade Eleitoral` == input$UE & `Sigla do Partido` == input$PARTIDO & `Nome de urna` == input$CANDIDATO) %>% 
              select(Nome, CPF, `Número do Título Eleitoral`, Sexo, `Cor ou Raça`, `Grau de Instrução`, Ocupação,
                     `Estado Civil`, Nacionalidade, `Estado de Nascimento`, `Município de Nascimento`) %>% 
              unique()
          }    
        })
      })
      
      
      beleicoes <- eventReactive(input$BAL1, {
        datatable({
          ue <- input$UE
          partido <- input$PARTIDO
          candidato <- input$CANDIDATO
          if(ue == "Todas as unidades eleitorais" & partido == "Todos os partidos"){
            df %>% 
              filter(`Nome de urna` == input$CANDIDATO) %>% 
              select(`Ano da Eleição`, `Nº do Turno`, Cargo, `Sigla da Unidade Eleitoral`, `Situação da Candidatura`, `Número de urna`, `Sigla do Partido`,
                     `Sigla Atual do Partido`,`Composição da Coligação`, Votos)
          }
          else if(ue == "Todas as unidades eleitorais" & partido == input$PARTIDO){
            df %>% 
              filter(`Nome de urna` == input$CANDIDATO) %>% 
              select(`Ano da Eleição`, `Nº do Turno`, Cargo, `Sigla da Unidade Eleitoral`, `Situação da Candidatura`, `Número de urna`, `Sigla do Partido`,
                     `Sigla Atual do Partido`,`Composição da Coligação`, Votos)
          }
          else if(ue == input$UE & partido == "Todos os partidos"){
            df %>% 
              filter(`Nome de urna` == input$CANDIDATO) %>% 
              select(`Ano da Eleição`, `Nº do Turno`, Cargo, `Sigla da Unidade Eleitoral`, `Situação da Candidatura`, `Número de urna`, `Sigla do Partido`,
                     `Sigla Atual do Partido`,`Composição da Coligação`, Votos)
          } else{
            df %>% 
              filter(`Sigla da Unidade Eleitoral` == input$UE & `Sigla do Partido` == input$PARTIDO & `Nome de urna` == input$CANDIDATO) %>%  
              select(`Ano da Eleição`, `Nº do Turno`, Cargo, `Sigla da Unidade Eleitoral`, `Situação da Candidatura`, `Número de urna`,`Sigla do Partido`,
                     `Sigla Atual do Partido`,`Composição da Coligação`, Votos)
            
          }
        })
      })
      
}


# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)
