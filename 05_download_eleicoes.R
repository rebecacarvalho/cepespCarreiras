## APP CARREIRAS - Cepespdata / FGV
## Montando o banco de eleições nacionais

# Carregando pacotes ----
library(cepespR)
library(dplyr)
library(ggplot2)
library(httr)
library(readr)
library(utils)
library(jsonlite)
library(tidyverse)

# Download dos bancos de dados ----
      # Vamos usar a função get_elections do cepespR com os parametros political_aggregation="Candidate" 
      #e regional_aggregation="Brazil" porque assim obtemos o número de votos total do candidato 
      #na UE em que ele disputou.

# selecionando colunas:
columns <- list("ANO_ELEICAO","NUM_TURNO","NUM_TITULO_ELEITORAL_CANDIDATO",
                "DESCRICAO_CARGO","SIGLA_UE","DESCRICAO_UE","DES_SITUACAO_CANDIDATURA",
                "SIGLA_PARTIDO","SIGLA_COLIGACAO","COMPOSICAO_COLIGACAO",
                "QTDE_VOTOS","DESC_SIT_TOT_TURNO")
cargos_nacionais <- c("Presidente", "Governador", "Senador", "Deputado Federal", "Deputado Estadual")

# 1.1. Eleições nacionais - Download ----
## 1998
elections_1998 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_elections(year = 1998, position = i, 
                     regional_aggregation="Brazil", political_aggregation="Candidate", 
                     columns_list = columns)
  elections_1998 <- rbind(elections_1998,x)
}

## 2002
elections_2002 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_elections(year = 2002, position = i, 
                     regional_aggregation="Brazil", political_aggregation="Candidate", 
                     columns_list = columns)
  elections_2002 <- rbind(elections_2002,x)
}

## 2006
elections_2006 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_elections(year = 2006, position = i, 
                     regional_aggregation="Brazil", political_aggregation="Candidate", 
                     columns_list = columns)
  elections_2006 <- rbind(elections_2006,x)
}

## 2010
elections_2010 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_elections(year = 2010, position = i, 
                     regional_aggregation="Brazil", political_aggregation="Candidate", 
                     columns_list = columns)
  elections_2010 <- rbind(elections_2010,x)
}

## 2014
elections_2014 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_elections(year = 2014, position = i, 
                     regional_aggregation="Brazil", political_aggregation="Candidate", 
                     columns_list = columns)
  elections_2014 <- rbind(elections_2014,x)
}

## 2018
elections_2018 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_elections(year = 2018, position = i, 
                     regional_aggregation="Brazil", political_aggregation="Candidate", 
                     columns_list = columns)
  elections_2018 <- rbind(elections_2018,x)
}

# Empilhando bancos -----
elections <- rbind(elections_1998, elections_2002, elections_2006, 
                   elections_2010, elections_2014, elections_2018)
# Salvando banco ----
save(elections, file = "/home/eliana/Documents/cepesp/app_carreiras/elections.RData")

# Removendo bancos individuais ----
rm(elections_1998,elections_2002,elections_2006,elections_2010,elections_2014,elections_2018)
rm(x, i, cargos_nacionais,columns)