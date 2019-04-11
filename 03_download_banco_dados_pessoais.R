## # APP CANDIDATOS - CEPESPDATA/FGV
# SCRIPT 03 - Download do banco de informações pessoais

# 0. Carregando pacotes ----
library(cepespR)
library(dplyr)
library(ggplot2)
library(httr)
library(readr)
library(utils)
library(jsonlite)
library(tidyverse)

# 1. Carregando bancos de candidatos ----

# selecionando colunas:
columns <- list("ANO_ELEICAO",
                "NUM_TURNO",
                "NOME_CANDIDATO",
                "CPF_CANDIDATO",
                "NUM_TITULO_ELEITORAL_CANDIDATO",
                "DESCRICAO_SEXO", 
                "DESCRICAO_COR_RACA",
                "DESCRICAO_GRAU_INSTRUCAO",
                "DESCRICAO_OCUPACAO",
                "DESCRICAO_ESTADO_CIVIL",
                "DESCRICAO_NACIONALIDADE",
                "SIGLA_UF_NASCIMENTO",
                "NOME_MUNICIPIO_NASCIMENTO",
                "DATA_NASCIMENTO",
                "IDADE_DATA_ELEICAO",
                "EMAIL_CANDIDATO")

cargos_nacionais <- c("Presidente", "Governador", "Senador", "Deputado Federal", "Deputado Estadual")

# 1.1. Eleições nacionais - Download ----
## 1998
personal_1998 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_candidates(year = 1998, position = i, columns_list = columns)
  personal_1998 <- rbind(personal_1998,x)
}
#       summary(as.numeric(personal_1998$NUM_TITULO_ELEITORAL_CANDIDATO)) # 50 NAs

## 2002
personal_2002 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_candidates(year = 2002, position = i, columns_list = columns)
  personal_2002 <- rbind(personal_2002,x)
}

#     summary(as.numeric(personal_2002$NUM_TITULO_ELEITORAL_CANDIDATO)) # 584 NAs

## 2006
personal_2006 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_candidates(year = 2006, position = i, columns_list = columns)
  personal_2006 <- rbind(personal_2006,x)
}

#     summary(as.numeric(personal_2006$NUM_TITULO_ELEITORAL_CANDIDATO)) # 0 NA

## 2010
personal_2010 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_candidates(year = 2010, position = i, columns_list = columns)
  personal_2010 <- rbind(personal_2010,x)
}

#     summary(as.numeric(personal_2010$NUM_TITULO_ELEITORAL_CANDIDATO)) # 1 NA

## 2014
personal_2014 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_candidates(year = 2014, position = i, columns_list = columns)
  personal_2014 <- rbind(personal_2014,x)
}

#     summary(as.numeric(personal_2014$NUM_TITULO_ELEITORAL_CANDIDATO)) # 0 NA

## 2018
personal_2018 <- NULL
x <- NULL
for(i in cargos_nacionais){
  x <- get_candidates(year = 2018, position = i, columns_list = columns)
  personal_2018 <- rbind(personal_2018,x)
}

#       summary(as.numeric(personal_2018$NUM_TITULO_ELEITORAL_CANDIDATO)) # 0 NA

## 2. Salvando bancos eleições nacionais ----
save(personal_1998, file = "/home/eliana/Documents/cepesp/app_carreiras/personal_1998.RData")
save(personal_2002, file = "/home/eliana/Documents/cepesp/app_carreiras/personal_2002.RData")
save(personal_2006, file = "/home/eliana/Documents/cepesp/app_carreiras/personal_2006.RData")
save(personal_2010, file = "/home/eliana/Documents/cepesp/app_carreiras/personal_2010.RData")
save(personal_2014, file = "/home/eliana/Documents/cepesp/app_carreiras/personal_2014.RData")
save(personal_2018, file = "/home/eliana/Documents/cepesp/app_carreiras/personal_2018.RData")

## Removendo
rm(columns, cargos_nacionais); rm(x, i)
