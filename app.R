
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



# 2. User interface -------------------------------------------------------


ui <- fluidPage(
  
  
  navbarPage("CepespCarreiras", theme = shinytheme("flatly"),
             
             
             tabPanel("Perfil do candidato",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),width = 3,
                                     
                                     selectInput(inputId = "ANO_ELEICAO",
                                                 label = "Escolha um ano",
                                                 choices = c("1998","2002","2006","2010","2014","2018"),
                                                 selected = "2018"),
                                     
                                     
                                     selectInput(inputId = "DESCRICAO_CARGO",
                                                 label = "Escolha um cargo",
                                                 choices = c("Presidente", "Governador","Senador", "Deputado Federal", "Deputado Estadual"),
                                                 selected = "Presidente"),
                                     
                                     textInput(inputId = "CANDIDATO",
                                               value = "Digite o nome do candidato"),
                                     
                                     
                                     actionButton(inputId = "BCALC1",
                                                  label = strong("Carregar"),
                                                  width = "95%")
                                     
                        ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100)))),
                        
                        tabPanel("Sobre")
             
             
             ))
                     
            
 

# 3. Server ---------------------------------------------------------------

server <- function(input, output)
{}



# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)

            