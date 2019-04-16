
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

df <- df %>% 
  arrange(desc(`Ano da Eleição`))


candidatos <- as.data.frame(df$Nome)

candidatos <- unique(candidatos)

candidatos <- candidatos %>% 
  dplyr::arrange(`df$Nome`) %>% 
  rename("Nome" = "df$Nome")

# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  
  
  navbarPage("CepespCarreiras", theme = shinytheme("flatly"),
             
             
             tabPanel("Eleições",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),width = 3,
                                     
                                     selectInput(inputId = "CANDIDATO",
                                               label = "Digite o nome do candidato",
                                               choices = candidatos$Nome,
                                               selected = " "),
                                     
                                     
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
      filter(Nome == input$CANDIDATO) %>% 
      select(Nome, CPF, `Número do Título Eleitoral`, Sexo, `Cor ou Raça`, `Grau de Instrução`, Ocupação,
             `Estado Civil`, Nacionalidade, `Estado de Nascimento`, `Município de Nascimento`) %>% 
      unique()
      
  )
})
   
   
   beleicoes <- eventReactive(input$BAL1, {
     datatable(
       df %>% 
         filter(Nome == input$CANDIDATO) %>% 
         select(`Ano da Eleição`, `Nº do Turno`, Cargo, `Sigla da Unidade Eleitoral`, `Situação da Candidatura`, `Sigla do Partido`,
                `Sigla da Coligação`, `Composição da Coligação`, Votos)
    )
   })
  
}


# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)

            