# APP CANDIDATOS - CEPESPDATA/FGV
# api p/ consulta candidatos

# Instalar cepespR ----
#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("Cepesp-Fgv/cepesp-r")

# Carregando pacotes ----
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
columns <- list("ANO_ELEICAO","NUM_TURNO","NOME_CANDIDATO","CPF_CANDIDATO","NUM_TITULO_ELEITORAL_CANDIDATO","DES_SITUACAO_CANDIDATURA","DESC_SIT_TOT_TURNO")
cargos_nacionais <- c("Presidente", "Governador", "Senador", "Deputado Federal", "Deputado Estadual")

# 1.1. Eleições nacionais - Download e teste de CPF e Título ----
    ## 1998
    api_1998 <- NULL
    x <- NULL
    for(i in cargos_nacionais){
      x <- get_candidates(year = 1998, position = i, columns_list = columns)
      api_1998 <- rbind(api_1998,x)
    }
            summary(as.numeric(api_1998$CPF_CANDIDATO)) # 367 NAs
            summary(as.numeric(api_1998$NUM_TITULO_ELEITORAL_CANDIDATO)) # 50 NAs
            api_1998$CPF_CANDIDATO[is.na(as.numeric(api_1998$NUM_TITULO_ELEITORAL_CANDIDATO))==T] # 11 obs. não têm titulo nem CPF.
      
    ## 2002
      api_2002 <- NULL
      x <- NULL
      for(i in cargos_nacionais){
        x <- get_candidates(year = 2002, position = i, columns_list = columns)
        api_2002 <- rbind(api_2002,x)
      }
      
            summary(as.numeric(api_2002$CPF_CANDIDATO)) # 642 NAs
            summary(as.numeric(api_2002$NUM_TITULO_ELEITORAL_CANDIDATO)) # 584 NAs
            sum(api_2002$CPF_CANDIDATO[is.na(as.numeric(api_2002$NUM_TITULO_ELEITORAL_CANDIDATO))==T] == "#NULO#") # 509 obs.
      
    ## 2006
      api_2006 <- NULL
      x <- NULL
      for(i in cargos_nacionais){
        x <- get_candidates(year = 2006, position = i, columns_list = columns)
        api_2006 <- rbind(api_2006,x)
      }
      
            summary(as.numeric(api_2006$CPF_CANDIDATO)) # nenhum NA
            summary(as.numeric(api_2006$NUM_TITULO_ELEITORAL_CANDIDATO)) # nenhum NA
            
            
      ## 2010
      api_2010 <- NULL
      x <- NULL
      for(i in cargos_nacionais){
        x <- get_candidates(year = 2010, position = i, columns_list = columns)
        api_2010 <- rbind(api_2010,x)
      }
      
          #  summary(as.numeric(api_2010$CPF_CANDIDATO)) # nenhum NA
          # summary(as.numeric(api_2010$NUM_TITULO_ELEITORAL_CANDIDATO)) # 1 NA
          #  api_2010$CPF_CANDIDATO[is.na(as.numeric(api_2010$NUM_TITULO_ELEITORAL_CANDIDATO))==T] # CPF do candidato sem título: 11702214320
      
      ## 2014
      api_2014 <- NULL
      x <- NULL
      for(i in cargos_nacionais){
        x <- get_candidates(year = 2014, position = i, columns_list = columns)
        api_2014 <- rbind(api_2014,x)
      }
      
      #summary(as.numeric(api_2014$CPF_CANDIDATO)) # nenhum NA
      #summary(as.numeric(api_2014$NUM_TITULO_ELEITORAL_CANDIDATO)) # nenhum NA
      
      ## 2018
      api_2018 <- NULL
      x <- NULL
      for(i in cargos_nacionais){
        x <- get_candidates(year = 2018, position = i, columns_list = columns)
        api_2018 <- rbind(api_2018,x)
      }
      
      #summary(as.numeric(api_2018$CPF_CANDIDATO)) # nenhum NA
      #summary(as.numeric(api_2018$NUM_TITULO_ELEITORAL_CANDIDATO)) # nenhum NA
      
      ## Salvando bancos eleições nacionais
      save(api_1998, file = "/home/eliana/Documents/cepesp/app_carreiras/api_1998.RData")
      save(api_2002, file = "/home/eliana/Documents/cepesp/app_carreiras/api_2002.RData")
      save(api_2006, file = "/home/eliana/Documents/cepesp/app_carreiras/api_2006.RData")
      save(api_2010, file = "/home/eliana/Documents/cepesp/app_carreiras/api_2010.RData")
      save(api_2014, file = "/home/eliana/Documents/cepesp/app_carreiras/api_2014.RData")
      save(api_2018, file = "/home/eliana/Documents/cepesp/app_carreiras/api_2018.RData")
      
      ## Removendo
      rm(columns); rm(x); rm(i)