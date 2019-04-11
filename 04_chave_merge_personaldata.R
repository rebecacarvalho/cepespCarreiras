## # APP CANDIDATOS - CEPESPDATA/FGV
# SCRIPT 03 - Merge das informações pessoais com a chave

# Load saved data personaldata ----
load("/home/eliana/Documents/cepesp/app_carreiras/personal_1998.RData")
load("/home/eliana/Documents/cepesp/app_carreiras/personal_2002.RData")
load("/home/eliana/Documents/cepesp/app_carreiras/personal_2006.RData")
load("/home/eliana/Documents/cepesp/app_carreiras/personal_2010.RData")
load("/home/eliana/Documents/cepesp/app_carreiras/personal_2014.RData")
load("/home/eliana/Documents/cepesp/app_carreiras/personal_2018.RData")
load("chave.RData")

# Empilhando bancos ----
personaldata <- rbind(personal_1998, personal_2002, personal_2006, personal_2010, personal_2014, personal_2018)

# Eliminando títulos eleitorais indisponíveis e selecionando apenas 1º turno p/ evitar duplicatas ----
x<-nrow(personaldata)

personaldata %>% 
  filter(NUM_TURNO==1) %>%
  filter(is.na(as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO))==F) -> personaldata

y<-nrow(personaldata)
x-y # perdemos 795 obs.
rm(x, y)

# Criando chave -----
personaldata$chave <- paste0(personaldata$NUM_TITULO_ELEITORAL_CANDIDATO,"_",personaldata$ANO_ELEICAO)

# Verificando chaves duplicadas ----
personaldata %>%
  filter(duplicated(chave)==T) -> chavesduplicadas
chavesduplicadas %>% distinct() -> obs_unicas_chavesduplicadas
nrow(chavesduplicadas) - nrow(obs_unicas_chavesduplicadas) # Verificamos que todas as chaves duplicadas 
#são observações únicas entre si nas variáveis que selecionamos. Talvez a diferença esteja na candidatura, 
#mas esta informação não é relevante p/ nós neste momento.

# Mantendo apenas as chaves únicas nos bancos -----
x <- nrow(personaldata)

personaldata %>%
  distinct(chave, .keep_all = T) -> personaldata

y <- nrow(personaldata)
x - y # mesmo tendo filtrado para o primeiro turno anteriormente, 
#perdemos 366 observações quando mantivemos apenas chaves únicas.
# Provavelmente as variáveis duplicadas se diferenciam nas variáveis que 
#não foram baixadas.
rm(x,y,chavesduplicadas,obs_unicas_chavesduplicadas)
rm(personal_1998,personal_2002,personal_2006,personal_2010,personal_2014,personal_2018)

# Merge com o banco "chave"
personaldata <- left_join(chave, personaldata, by = "chave")

nrow(personaldata) - nrow(chave) # Vemos que não houve nenhuma duplicação no join.
sum(personaldata$NUM_TITULO_ELEITORAL_CANDIDATO.x != personaldata$NUM_TITULO_ELEITORAL_CANDIDATO.y) # vemos que essas duas variáveis são identicas.
colnames(personaldata)[2] <- "NUM_TITULO_ELEITORAL_CANDIDATO" # Arrumando o nome da coluna que foi alterado com o merge
personaldata$NUM_TITULO_ELEITORAL_CANDIDATO.y <- NULL # E eliminamos a outra variavel de titulo de eleitor repetida.
personaldata$ANO_ELEICAO <- NULL # Apagando a variável ano eleição, que virá do outro banco
personaldata$NUM_TURNO <- NULL # Apagando a variável numero turno, que virá do outro banco
personaldata %>% filter(duplicated(NUM_TITULO_ELEITORAL_CANDIDATO)) -> teste # checando se há títulos de eleitor duplicados na base de perfil (não pode haver).

rm(chave, teste) #removando chave e teste



# Salvando banco com os dados pessoais
save(personaldata, file = "/home/eliana/Documents/cepesp/app_carreiras/personaldata.RData")
