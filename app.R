
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
             
             
             tabPanel("Sobre"),
             
             tabPanel("Caracterização do candidato"),
             
             mainPanel()))
            
 

# 3. Server ---------------------------------------------------------------

server <- function(input, output)
{}



# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)

            