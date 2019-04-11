# APP CANDIDATOS - CEPESPDATA/FGV
# Gerar a chave que me dará qual a última eleição concorrida

# Loading data -----
      ## Bancos de eleições nacionais
      load("Documents/cepesp/app_carreiras/api_1998.RData")
      load("Documents/cepesp/app_carreiras/api_2002.RData")
      load("Documents/cepesp/app_carreiras/api_2006.RData")
      load("Documents/cepesp/app_carreiras/api_2010.RData")
      load("Documents/cepesp/app_carreiras/api_2014.RData")
      load("Documents/cepesp/app_carreiras/api_2018.RData")

# Funções ----
# Função que calcula p/ os elementos string de um vetor a distancia dos demais elementos
str_dist <- function(str_vector){
  distance <- NULL
  for(i in 1:length(str_vector)){
    x <- mean(adist(str_vector)[i,])/length(str_vector)
    distance <- c(distance,x)
  }
  rm(x)
  return(distance)
}

# 2. Testando se cada CPF se combina apenas a um Título de eleitor e vice-versa ----
## Empilhando as bases de candidatos a eleições nacionais
candidatos_nacionais <- rbind(api_1998,api_2002,api_2006,api_2010,api_2014,api_2018)

# Filtrando CPFs "#NULO#" e Título "00000000#NI#"
candidatos_nacionais %>% 
  filter(CPF_CANDIDATO == "#NULO#") -> sem_cpf #1071 obs.

candidatos_nacionais %>%
  filter(NUM_TITULO_ELEITORAL_CANDIDATO == "00000000#NI#" | NUM_TITULO_ELEITORAL_CANDIDATO == "000000000000") -> sem_titulo # 634 sem título

candidatos_nacionais %>%
  filter(CPF_CANDIDATO == "#NULO#" & NUM_TITULO_ELEITORAL_CANDIDATO == "00000000#NI#") -> sem_titulo_cpf # 521 sem título e sem cpf todas em 1998 e 2002

# Esses 521 candidatos sem título ou CPF serão eliminados da base:
candidatos_nacionais %>%
  filter(CPF_CANDIDATO != "#NULO#" & NUM_TITULO_ELEITORAL_CANDIDATO != "00000000#NI#") -> candidatos_nacionais


candidatos_nacionais %>% select(CPF_CANDIDATO,NUM_TITULO_ELEITORAL_CANDIDATO) %>% distinct() %>% nrow() -> cpf_titulo # Contando combinações únicas de cpf e título
candidatos_nacionais %>% select(CPF_CANDIDATO) %>% distinct() %>% nrow() -> cpf # cpfs únicos
candidatos_nacionais %>% select(NUM_TITULO_ELEITORAL_CANDIDATO) %>% distinct() %>% nrow() -> titulo # títulos únicos
cpf_titulo - cpf # 200 registros de CPF c/ títulos não únicos
cpf_titulo - titulo # 163 registros de título de eleitor c/ CPFs não únicos

# String to numeric
candidatos_nacionais$CPF_CANDIDATO <- as.numeric(candidatos_nacionais$CPF_CANDIDATO)
candidatos_nacionais$NUM_TITULO_ELEITORAL_CANDIDATO <- as.numeric(candidatos_nacionais$NUM_TITULO_ELEITORAL_CANDIDATO)

# criando flags p/ os multiplos
candidatos_nacionais %>% 
  group_by(CPF_CANDIDATO) %>%
  mutate(sdtitulo = sd(NUM_TITULO_ELEITORAL_CANDIDATO, na.rm = T)) %>%
  ungroup() %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(sdcpf = sd(CPF_CANDIDATO, na.rm = T)) %>%
  ungroup() %>%
  mutate(multiplos_titulos = if_else(sdtitulo>0,1,0)) %>%
  mutate(multiplos_cpfs = if_else(sdcpf>0,1,0)) %>%
  mutate(multiplos_titulos = if_else(is.na(multiplos_titulos)==T,0,multiplos_titulos)) %>%
  mutate(multiplos_cpfs = if_else(is.na(multiplos_cpfs)==T,0,multiplos_cpfs)) %>%
  mutate(ambos_multiplos = if_else(multiplos_titulos + multiplos_cpfs == 2, 1, 0))-> candidatos_nacionais

# Filtrando os multiplos:
candidatos_nacionais %>%
  filter(multiplos_cpfs==1) -> multicpf # Banco com os CPFs multiplos p/ o mesmo título
candidatos_nacionais %>%
  filter(multiplos_titulos==1) -> multi_tit # Banco com os Títulos múltiplos p/ o mesmo CPF
candidatos_nacionais %>%
  filter(multiplos_cpfs == 1 | multiplos_titulos ==1 ) -> multiplicados # 1084 obs. em todos os anos. Decidir o que fazer com estes.


# Alguns são erros de digitação, strings muito semelhantes; outros são muito diferentes e podem ser
#pessoas diferentes.
# Vamos calcular a distancia dos CPFs multiplos:
multicpf %>%
  mutate(CPF_CANDIDATO=as.character(CPF_CANDIDATO)) %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(distancia=str_dist(CPF_CANDIDATO)) %>%
  ungroup()-> multicpf
# Vamos calcular a distancia dos Títulos múltiplos
multi_tit %>%
  mutate(NUM_TITULO_ELEITORAL_CANDIDATO=as.character(NUM_TITULO_ELEITORAL_CANDIDATO)) %>%
  group_by(CPF_CANDIDATO) %>%
  mutate(distancia=str_dist(NUM_TITULO_ELEITORAL_CANDIDATO)) %>%
  ungroup() -> multi_tit

# Vamos ver se teremos mais erros graves a corrigir se utilizarmos o título (distancia nos CPFs)
# ou se utilizarmos os CPFs (distancia nos Títulos)
summary(multicpf$distancia)
summary(multi_tit$distancia) # usar o CPF será mais adequado


# Filtrando os multiplos novamente:
candidatos_nacionais %>%
  filter(multiplos_cpfs==1) -> multicpf # Banco com os CPFs multiplos p/ o mesmo título
candidatos_nacionais %>%
  filter(multiplos_titulos==1) -> multi_tit # Banco com os Títulos múltiplos p/ o mesmo CPF
candidatos_nacionais %>%
  filter(multiplos_cpfs == 1 | multiplos_titulos ==1 ) -> multiplicados

nrow(multicpf)/nrow(candidatos_nacionais) # 0,36% de erro max.
nrow(multi_tit) / nrow(candidatos_nacionais) # 0,47% de erro max.

summary(as.factor(multiplicados$DES_SITUACAO_CANDIDATURA))
summary(as.factor(multiplicados$DESC_SIT_TOT_TURNO))

# 3. Substituicoes no banco -----
# Se a distância for menor que 1 no titulo, subtituir pelo titulo cuja distancia é mínima,
#se a distância for menor que 1 no CPF, subtituir pelo CPF cuja distancia é mínima.
candidatos_nacionais %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(dist_cpf=str_dist(CPF_CANDIDATO)) %>%
  mutate(mindist_cpf=if_else(min(dist_cpf)==dist_cpf,1,0)) %>% # CPF com a distancia min dos demais
  mutate(CPF_CANDIDATO=if_else(dist_cpf<1 & mindist_cpf==0, mean(as.numeric(CPF_CANDIDATO[mindist_cpf==1])), as.numeric(CPF_CANDIDATO)))%>%
  mutate(sdcpf = sd(as.numeric(CPF_CANDIDATO), na.rm = T)) %>%
  ungroup() %>%
  group_by(CPF_CANDIDATO) %>%
  mutate(dist_tit=str_dist(NUM_TITULO_ELEITORAL_CANDIDATO)) %>%
  mutate(mindist_tit=if_else(min(dist_tit)==dist_tit,1,0)) %>%
  mutate(NUM_TITULO_ELEITORAL_CANDIDATO=if_else(dist_tit<1 & mindist_tit==0, mean(as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO[mindist_tit==1])), as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO))) %>%
  mutate(sdtitulo = sd(as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO), na.rm = T)) %>%
  ungroup() %>%
  mutate(multiplos_titulos = if_else(sdtitulo>0,1,0)) %>%
  mutate(multiplos_cpfs = if_else(sdcpf>0,1,0)) %>%
  mutate(multiplos_titulos = if_else(is.na(multiplos_titulos)==T,0,multiplos_titulos)) %>%
  mutate(multiplos_cpfs = if_else(is.na(multiplos_cpfs)==T,0,multiplos_cpfs)) %>%
  mutate(ambos_multiplos = if_else(multiplos_titulos + multiplos_cpfs == 2, 1, 0))-> candidatos_nacionais


# Criando variáveis com a distância entre os nomes dos candidatos
candidatos_nacionais %>%
  group_by(CPF_CANDIDATO) %>%
  mutate(distnome_tit=str_dist(NOME_CANDIDATO)) %>% # com o mesmo CPF e titulos diferentes
  ungroup() %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(distnome_cpf=str_dist(NOME_CANDIDATO)) %>% # com o mesmo título e CPFs diferentes.
  ungroup() -> candidatos_nacionais 

## Se dist_cpf>1 (CPFs multiplos) p/ o mesmo título e mesmo nome, substituir o CPF 
#com o CPF da observação que tem a dist_cpf mínima p/ aquele titulo.
## Se dist_tit>1, (Títulos multiplos) p/ o mesmo CPF e mesmo nome, substituir o titulo
#com o Título da observação que tem a dist_tit mínima p/ aquele CPF.
candidatos_nacionais %>%
  group_by(CPF_CANDIDATO) %>%
  mutate(NUM_TITULO_ELEITORAL_CANDIDATO=if_else(dist_tit>=1 & distnome_tit==0, mean(as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO[mindist_tit==1])), NUM_TITULO_ELEITORAL_CANDIDATO)) %>%
  ungroup() %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(CPF_CANDIDATO=if_else(dist_cpf>=1 & distnome_cpf==0, mean(as.numeric(CPF_CANDIDATO[mindist_cpf==1])),CPF_CANDIDATO)) -> candidatos_nacionais


## DUPLAS ## -------------------
# problemas: quando tenho só duas obs repetidas com CPF ou titulo minimamente diferente, 
#o minimo (no pipe gigante acima) será as duas obs. -> ele não substitui.
# Vamos descobrir quem são as duplas:
multiplicados %>%
  group_by(CPF_CANDIDATO) %>%
  mutate(dupladeCPF=if_else(sum(multiplos_titulos)==2,1,0)) %>%
  ungroup() %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(dupladeTitulo=if_else(sum(multiplos_cpfs)==2,1,0)) %>%
  ungroup() %>%
  filter(dupladeCPF==1 | dupladeTitulo==1)-> duplas

# De toda forma, temos pouquíssimo erro:
sum(candidatos_nacionais$multiplos_cpfs)/nrow(candidatos_nacionais) # 0.002634036 temos 0,27% de erro
sum(candidatos_nacionais$multiplos_titulos/nrow(candidatos_nacionais)) # 0.002694763 temos 0,27% de erro


# Depois de todas as substituições (feitas no script 02) - Recalculando observações múltiplas ----------------
candidatos_nacionais %>%
  group_by(CPF_CANDIDATO) %>%
  mutate(sdtitulo = sd(as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO), na.rm = T)) %>%
  ungroup() %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(sdcpf = sd(as.numeric(CPF_CANDIDATO), na.rm = T)) %>%
  ungroup() %>%
  mutate(multiplos_titulos = if_else(sdtitulo>0,1,0)) %>%
  mutate(multiplos_cpfs = if_else(sdcpf>0,1,0)) %>%
  mutate(multiplos_titulos = if_else(is.na(multiplos_titulos)==T,0,multiplos_titulos)) %>%
  mutate(multiplos_cpfs = if_else(is.na(multiplos_cpfs)==T,0,multiplos_cpfs)) %>%
  mutate(ambos_multiplos = if_else(multiplos_titulos + multiplos_cpfs == 2, 1, 0))-> candidatos_nacionais

candidatos_nacionais %>%
  filter(multiplos_titulos==1 | multiplos_cpfs==1) -> multiplicados

sum(candidatos_nacionais$multiplos_cpfs) / nrow(candidatos_nacionais) # conseguimos chegar a um erro de 0.1548% no máximo, se usarmos o titulo
sum(candidatos_nacionais$multiplos_titulos) / nrow(candidatos_nacionais) # e a 0.159% usando CPF


## Checar se usar a média dos documentos com a min distancia p/ substituir os casos em que a distância é maior pode dar algum problema -----
summary(as.numeric(candidatos_nacionais$NUM_TITULO_ELEITORAL_CANDIDATO))
sum(is.integer(as.numeric(candidatos_nacionais$NUM_TITULO_ELEITORAL_CANDIDATO))==F)
candidatos_nacionais$nao_inteiro <- is.integer(as.numeric(candidatos_nacionais$NUM_TITULO_ELEITORAL_CANDIDATO)) # Parece que não há problemas.
## De forma mais sistemática/generalizada:
x <- rbind(api_1998,api_2002,api_2006,api_2010,api_2014,api_2018)
x %>%
  filter(CPF_CANDIDATO != "#NULO#" & NUM_TITULO_ELEITORAL_CANDIDATO != "00000000#NI#") -> x
x %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(dist_cpf=str_dist(CPF_CANDIDATO)) %>%
  mutate(mindist_cpf=if_else(min(dist_cpf)==dist_cpf,1,0)) %>%
  mutate(sdcpf = sd(as.numeric(CPF_CANDIDATO), na.rm = T)) %>%
  ungroup() %>%
  group_by(CPF_CANDIDATO) %>%
  mutate(dist_tit=str_dist(NUM_TITULO_ELEITORAL_CANDIDATO)) %>%
  mutate(mindist_tit=if_else(min(dist_tit)==dist_tit,1,0)) %>%
  mutate(sdtitulo = sd(as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO), na.rm = T)) %>%
  ungroup() %>%
  mutate(multiplos_titulos = if_else(sdtitulo>0,1,0)) %>%
  mutate(multiplos_cpfs = if_else(sdcpf>0,1,0)) %>%
  mutate(multiplos_titulos = if_else(is.na(multiplos_titulos)==T,0,multiplos_titulos)) %>%
  mutate(multiplos_cpfs = if_else(is.na(multiplos_cpfs)==T,0,multiplos_cpfs)) %>%
  mutate(ambos_multiplos = if_else(multiplos_titulos + multiplos_cpfs == 2, 1, 0))-> x # note que este pipe inteiro cria as medidas, mas não executa substituições.
# Situações que podem ser erro: 
# ERRO 1: ter mindist_cpf==1 p/ mais de um CPF por título, quando tenho CPFs diferentes p/ um mesmo título (multiplos_cpfs == 1):
x %>%
  filter(mindist_cpf==1) %>% # ao filtrar apenas os CPFs mais similares aos demais das obs. com o mesmo título que ele
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% # no conjunto de obs daquele titulo
  mutate(dist_cpf=str_dist(CPF_CANDIDATO)) %>% # a distancia recalculada dos demais CPFs deste mesmo título deveria ser igual a zero (quer dizer que todos são iguais)
  ungroup() %>%
  filter(dist_cpf>0) -> erros1 # 170 observações
# Analogamente, ERRO 2: ter mindist_tit==1 p/ mais de um Titulo por CPF, quando tenho títulos diferentes p/ um mesmo CPF (multiplos_titulos == 1):
x %>%
  filter(mindist_tit==1) %>%
  group_by(CPF_CANDIDATO) %>%
  mutate(dist_tit=str_dist(NUM_TITULO_ELEITORAL_CANDIDATO)) %>%
  ungroup() %>%
  filter(dist_tit>0) -> erros2 # 202 observações.
# Vou ter que inserir um comando p/ garantir que só faço substituirei pela média quando o CPF min ou Titulo min for único.


# Removendo objetos --------
rm(duplas, erros1, erros2, multi_tit, multicpf, multiplicados, sem_cpf, sem_titulo, sem_titulo_cpf)
rm(str_dist)
rm(cpf, cpf_titulo, titulo)