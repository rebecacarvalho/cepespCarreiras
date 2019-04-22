
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

siglas_atuais_partidos <- read_delim("~/cepesp/cepesp_carreiras/siglas_atuais_partidos.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

df <- df %>% 
  arrange(desc(`Ano da Eleição`)) %>% 
  na.omit()

df <- df %>% 
  dplyr::arrange(Nome)

ue <- as.data.frame(df$`Sigla da Unidade Eleitoral`)

ue <- ue %>% 
  dplyr::arrange(df$`Sigla da Unidade Eleitoral`) %>% 
  unique() %>% 
  rename("Sigla da Unidade Eleitoral" = "df$`Sigla da Unidade Eleitoral`") %>% 
  na.omit()

sigla <- as.data.frame(df$`Sigla do Partido`)

sigla <- sigla %>% 
  dplyr::arrange(df$`Sigla do Partido`) %>% 
  unique() %>% 
  rename("Sigla do Partido" = "df$`Sigla do Partido`") %>% 
  na.omit()


# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  
  
  navbarPage("CepespCarreiras", theme = shinytheme("flatly"),
             
             
             tabPanel("Eleições",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),width = 3,
                                     
                                     selectInput(inputId = "UE",
                                                 label = "Selecione a unidade eleitoral do candidato",
                                                 choices = c("Todos", "AC", "AL", "AM", "AP", "BA", "BR", "CE", "DF", "ES",
                                                             "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI",
                                                             "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                                                 selected = "Todos"),
                                     
                                     uiOutput("PARTIDO"),
                                     
                                     uiOutput("CANDIDATO"),
                                     
                                     actionButton(inputId = "BAL1",
                                                  label = strong("Atualizar"),
                                                  width = "95%")
                                     
                        ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100),
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
             
             
             ))
                     
      
 

# 3. Server ---------------------------------------------------------------

server <- function(input, output)
{

  
  
# Selecao do partido  
    
  output$PARTIDO <- renderUI({
    ue <- input$UE
    if(ue == "Todos"){
      selectizeInput(inputId = "PARTIDO",
                     label = "Selecione a sigla do partido do candidato",
                     choices = c("Todos", "AVANTE", "DC", "DEM", "MDB","NOVO", "PAN", "PATRI", "PC do B", 
                                           "PCB", "PCO", "PDT", "PEN", "PFL", "PGT", "PHS", "PL", "PMB",
                                           "PMDB", "PMN", "PODE", "PP", "PPB", "PPL", "PPS", "PR", "PRB", 
                                           "PRN", "PRONA", "PROS", "PRP", "PRTB", "PSB", "PSC", "PSD", "PSDB", 
                                           "PSDC", "PSL", "PSN", "PSOL", "PST", "PSTU", "PT", "PT do B", "PTB",
                                           "PTC", "PTN", "PV", "REDE", "SD", "SOLIDARIEDADE"),
                     selected = "Todos")
    } else{
    selectInput(inputId = "PARTIDO",
                label = "Selecione a sigla do partido do candidato",
                choices = df[df$`Sigla da Unidade Eleitoral`== input$UE, "Sigla do Partido"])
    }
    })
    
  
  
  
# Selecao do candidato  
  
  output$CANDIDATO <- renderUI({
    ue <- input$UE
    partido <- input$PARTIDO
    if(ue == "Todos" & partido == "Todos"){
      selectizeInput(inputId = "CANDIDATO",
                     label = "Digite o nome do candidato",
                     choices = unique(df$Nome))
    } else{
      selectInput(inputId = "CANDIDATO",
                label = "Digite o nome do candidato",
     
                   choices = df[df$`Sigla da Unidade Eleitoral`== input$UE & df$`Sigla do Partido` == input$PARTIDO, "Nome"])
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
  
  
   candidato <- reactive({
  dplyr::filter(df$`Ano da Eleição` == input$ANO_ELEICAO & df$Cargo == input$DESCRICAO_CARGO)
})  
  



# 3.2. Botao de acao ------------------------------------------------------

   bperfil <- eventReactive(input$BAL1, {
  datatable(
    df %>% 
      filter(`Sigla da Unidade Eleitoral` == input$UE & `Sigla do Partido` == input$PARTIDO & Nome == input$CANDIDATO) %>% 
      select(Nome, CPF, `Número do Título Eleitoral`, Sexo, `Cor ou Raça`, `Grau de Instrução`, Ocupação,
             `Estado Civil`, Nacionalidade, `Estado de Nascimento`, `Município de Nascimento`) %>% 
      unique()
      
  )
})
   
   
   beleicoes <- eventReactive(input$BAL1, {
     datatable(
       df %>% 
         filter(`Sigla da Unidade Eleitoral` == input$UE & `Sigla do Partido` == input$PARTIDO & Nome == input$CANDIDATO) %>% 
         select(`Ano da Eleição`, `Nº do Turno`, Cargo, `Sigla da Unidade Eleitoral`, `Situação da Candidatura`, `Sigla do Partido`,
                `Sigla da Coligação`, `Composição da Coligação`, Votos)
    )
   })
  
}


# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)

            