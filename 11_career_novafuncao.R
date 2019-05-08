## Criando a funação de consulta a partir da nova API ##
rm(list=ls())
library(dplyr)
library(cepespR)

# Função carrer. Indicar o string com o nome no argumento 'nome =' e indicar se TRUE or FALSE em 
# 'nome_urna =' .TRUE se o nome buscado for o (ou parte do) nome de urna.

career <- function(nome, nome_urna = TRUE) {
  if(nome_urna){
  consulta <- cepespR::get_careers(NOME_URNA_CANDIDATO = nome)
  } else {
  consulta <- cepespR::get_careers(NOME_CANDIDATO = nome)
  }
  
  args <- expand.grid(ID_DIM_CANDIDATO = consulta$ID)
  all_options <- purrr::pmap(args, cepespR::get_careers_elections)
  all_options <- purrr::map(all_options, dplyr::select, NOME_CANDIDATO, NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO, NUM_TURNO, SIGLA_PARTIDO, SIGLA_UE)
  
  x<-NA
  for (i in all_options) {
  x <- rbind(x, as.data.frame(i))
  }
  x <- x %>% filter(is.na(NOME_CANDIDATO)==F)
  all_options <- x
  # Filtro p/ manter apenas o último ano de eleição
  all_options <- all_options %>% 
    filter(NUM_TURNO==1) %>%
    group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
    mutate(ultimaeleicao=max(ANO_ELEICAO)) %>%
    ungroup() %>%
    filter(ANO_ELEICAO==ultimaeleicao)
  # Crio a variável com o texto do dropdown
  all_options$dropdown <- paste0(all_options$NOME_CANDIDATO," - ",all_options$SIGLA_UE," - ",all_options$SIGLA_PARTIDO)
  all_options <- all_options[,c("dropdown","NUM_TITULO_ELEITORAL_CANDIDATO")]
  rm(consulta,x,args,i)
  all_options
}
