# APP CANDIDATOS - CEPESPDATA/FGV
# Gerar a chave que me dará qual a última eleição concorrida

# Carregando pacotes ----
library(cepespR)
library(dplyr)
library(ggplot2)
library(httr)
library(readr)
library(utils)
library(jsonlite)
library(tidyverse)


# Loading dfs saved in the last script -----
load("/home/eliana/Documents/cepesp/app_carreiras/api_1998.RData")
load("/home/eliana/Documents/cepesp/app_carreiras/api_2002.RData")
load("/home/eliana/Documents/cepesp/app_carreiras/api_2006.RData")
load("/home/eliana/Documents/cepesp/app_carreiras/api_2010.RData")
load("/home/eliana/Documents/cepesp/app_carreiras/api_2014.RData")
load("/home/eliana/Documents/cepesp/app_carreiras/api_2018.RData")

## Empilhando as bases de candidatos a eleições nacionais ----
candidatos_nacionais <- rbind(api_1998,api_2002,api_2006,api_2010,api_2014,api_2018)

# Candidatos sem título e sem CPF simultanteamente serão eliminados da base -----
candidatos_nacionais %>%
  filter(CPF_CANDIDATO != "#NULO#" | NUM_TITULO_ELEITORAL_CANDIDATO != "00000000#NI#") -> candidatos_nacionais

# Localizando os multiplos CPFs p/ o mesmo Título e vice-versa ----
candidatos_nacionais %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(sdcpf = sd(as.numeric(CPF_CANDIDATO), na.rm = T)) %>%
  ungroup() %>%
  group_by(CPF_CANDIDATO) %>%
  mutate(sdtitulo = sd(as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO), na.rm = T)) %>%
  ungroup() %>%
  mutate(multiplos_titulos = if_else(sdtitulo>0,1,0)) %>%
  mutate(multiplos_cpfs = if_else(sdcpf>0,1,0)) %>%
  mutate(multiplos_titulos = if_else(is.na(multiplos_titulos)==T,0,multiplos_titulos)) %>%
  mutate(multiplos_cpfs = if_else(is.na(multiplos_cpfs)==T,0,multiplos_cpfs)) %>%
  mutate(ambos_multiplos = if_else(multiplos_titulos + multiplos_cpfs == 2, 1, 0))-> candidatos_nacionais

# Medindo o erro ----
sum(candidatos_nacionais$multiplos_cpfs) / nrow(candidatos_nacionais) # erro max de 0,45%
sum(candidatos_nacionais$multiplos_titulos) / nrow(candidatos_nacionais) #erro max de 0,85%
# Como a fração de CPFs multiplos é menor, vamos usar o título.

# Filtrando p/ os casos em que não há título de eleitor -----
candidatos_nacionais %>%
  filter(is.na(as.numeric(candidatos_nacionais$NUM_TITULO_ELEITORAL_CANDIDATO))==F) -> candidatos_nacionais # tirando títulos nulos

# Criando a chave c/ título e última eleição a que concorreu ----
candidatos_nacionais %>%
  select(ANO_ELEICAO,NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  distinct() %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(ULTIMA_ELEICAO=if_else(max(ANO_ELEICAO)==ANO_ELEICAO,1,0)) %>%
  ungroup() %>%
  filter(ULTIMA_ELEICAO==1) -> chave
chave[,3] <- NULL
colnames(chave)[1] <- "ULTIMA_ELEICAO"

# Testando se o mesmo título aparece mais de uma vez na chave   ----
chave %>% 
  select(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  mutate(NUM_TITULO_ELEITORAL_CANDIDATO=as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO)) %>%
  mutate(duplicado=duplicated(NUM_TITULO_ELEITORAL_CANDIDATO)) %>%
  filter(duplicado==T)-> x # 0 obs. duplicadas.
rm(x)

# Construindo variável chave ----
chave$chave <- paste0(chave$NUM_TITULO_ELEITORAL_CANDIDATO,"_",chave$ULTIMA_ELEICAO)

# Salvando chave ----
save(chave, file = "chave.RData")

# Removendo bancos que utilizamos p/ construir a chave -----
rm(api_1998, api_2002, api_2006, api_2010, api_2014, api_2018, candidatos_nacionais)
