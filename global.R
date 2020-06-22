# Titulo: Shiny do aplicativo de Carreiras
# Autor: Rebeca Carvalho


# Pacotes utilizados


library(knitr)
library(plyr)
library(tidyverse)
library(ggplot2)
library(shiny)
library(readr)
library(shinythemes)
library(magrittr)
library(DT)
library(R.utils)
library(shinyjs)
library(shinybusy)
library(shinyWidgets)
library(dqshiny)


# Objetivos

#'         - Carregar os pacotes necessários e os dados pré-processados 
#'         - que serão utilizados no aplicativo de carreiras.


# 1. Data ----------------------------------------------------------------

df <- readRDS("data/input/df.rds")

candidatos <- readRDS("data/output/candidatos.rds")

candidatos <- candidatos$`Nome de urna`
