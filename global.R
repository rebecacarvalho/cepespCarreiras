# Titulo: Shiny do aplicativo de Carreiras
# Autor: Rebeca Carvalho


# Pacotes utilizados


library(tidyverse)
library(shiny)
library(DT)
library(R.utils)
library(shinyjs)
library(shinybusy)
library(shinyWidgets)



# Objetivos

#'         - Carregar os pacotes necessários e os dados pré-processados 
#'         - que serão utilizados no aplicativo de carreiras.


# 1. Data ----------------------------------------------------------------

 
df <- readRDS("data/input/df.rds")


candidatos <- readRDS("data/input/candidatos.rds")

