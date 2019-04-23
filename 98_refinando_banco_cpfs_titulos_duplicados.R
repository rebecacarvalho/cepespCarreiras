# APP CANDIDATOS - CEPESPDATA/FGV
# Refinando o banco

# Carregando pacotes ----
library(stringdist)
library(tidyverse)

# Loading dfs saved in the 1st script -----
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

# Checo qual é o número de CPFs múltiplos por título de eleitor ----
candidatos_nacionais %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  summarize(num_cpf=n_distinct(CPF_CANDIDATO)) -> test

# Filtramos CPFs multiplos p/ o mesmo titulo -----
test <- candidatos_nacionais %>%
            select(ANO_ELEICAO,NUM_TITULO_ELEITORAL_CANDIDATO,CPF_CANDIDATO) %>%
            group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
            mutate(num_cpf=n_distinct(CPF_CANDIDATO)) %>%
            filter(num_cpf>1)


test %>% arrange(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(NUM_TITULO_ELEITORAL_CANDIDATO!="00000000#NI#") -> test

test <- test %>% filter(num_cpf<3) %>%  
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO,CPF_CANDIDATO) %>%
  summarize(freq=n()) %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(pct_freq=freq/sum(freq))

test<- test %>% mutate(CPF_ID=c('one','two')) %>% 
  select(NUM_TITULO_ELEITORAL_CANDIDATO,CPF_CANDIDATO, CPF_ID) %>% 
  spread(key='CPF_ID', value='CPF_CANDIDATO') %>%
  mutate(dist_string=stringdist(one,two, method="lv"))

test <- candidatos_nacionais %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO, CPF_CANDIDATO) %>%
  count() %>%
  filter(n>1) %>%
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO)

test %>%
  group_by(num_cpf) %>%
  count()

test %>% filter(n==2) %>% arrange(NUM_TITULO_ELEITORAL_CANDIDATO,CPF_CANDIDATO)

#desenvolver:
# - CPF nulo em 1998 podemos substituir pelo cpf que aparece em outro momento.
# - Mesmo Titulo concorrendo a diferentes cargos em um mesmo ano.


#### Outras notas #### -------------------------------------------------------

# Começar a salvar em RDS e não em RData;

# Ter todos os nomes de urna que o candidato já utilizou na vida incorporado ao banco de dados;

# função que substitui o loop for: map. Exemplo:

args <- expand.grid(position = c(1,3,5,6,7),
                    year=c("1998","2002","2006","2010","2014","2018"))

candidatos_ls <- pmap(args,
                      cepespR::get_candidates,
                      columns_list = list("ANO_ELEICAO",
                                          "NUM_TURNO",
                                          'SIGLA_UF',
                                          'CODIGO_CARGO',
                                          'SIGLA_PARTIDO',
                                          'NOME_CANDIDATO',
                                          'NOME_URNA_CANDIDATO',
                                          'NUMERO_CANDIDATO',
                                          'NUM_TITULO_ELEITORAL_CANDIDATO',
                                          'DATA_NASCIMENTO',
                                          'DES_SITUACAO_CANDIDATURA',
                                          'DESC_SIT_TOT_TURNO'))

candidatos_df <- bind_rows(candidatos_ls)