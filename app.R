
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
library(shinythemes)
library(magrittr)
library(plotly)
library(DT)

cepespR::get_careers()

# 1. Data ----------------------------------------------------------------


career <- function(nome, nome_urna = TRUE) {
  if(nome_urna){
    consulta <- cepespR::get_careers(NOME_URNA_CANDIDATO = nome)
  } else {
    consulta <- cepespR::get_careers(NOME_CANDIDATO = nome)
  }
  
  args <- expand.grid(ID_DIM_CANDIDATO = consulta$ID)
  all_options <- purrr::pmap(args, cepespR::get_careers_elections)
  all_options <- purrr::map(all_options, dplyr::select, NOME_CANDIDATO, NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO, NUM_TURNO, SIGLA_PARTIDO, SIGLA_UE)
  
  x<-NA
  for (i in all_options) {
    x <- rbind(x, as.data.frame(i))
  }
  x <- x %>% filter(is.na(NOME_CANDIDATO)==F)
  all_options <- x
  # Filtro p/ manter apenas o último ano de eleição
  all_options <- all_options %>% 
    filter(NUM_TURNO==1) %>%
    group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
    mutate(ultimaeleicao=max(ANO_ELEICAO)) %>%
    ungroup() %>%
    filter(ANO_ELEICAO==ultimaeleicao)
  # Crio a variável com o texto do dropdown
  all_options$dropdown <- paste0(all_options$NOME_CANDIDATO," - ",all_options$SIGLA_UE," - ",all_options$SIGLA_PARTIDO)
  all_options <- all_options[,c("dropdown","NUM_TITULO_ELEITORAL_CANDIDATO")]
  rm(consulta,x,args,i)
  all_options
}




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
                     choices = career(),
                     options = list(placeholder = 'Digite o nome do candidato'))
    } 
    else if(ue == input$UE & partido == "Todos os partidos"){
      selectizeInput(inputId = "CANDIDATO",
                     label = NULL,
                     choices = carrer[df$`Sigla da Unidade Eleitoral`== input$UE, "Nome de urna"],
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

