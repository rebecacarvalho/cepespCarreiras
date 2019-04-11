## APP Carreiras - Cepespdata/FGV
## Arrumando os detlahes do banco

# Pacotes ----
library(tidyverse)
library(stringr)

# Carregando banco -------
load("df_carreiras.RData")

# Funções ----

## I. P/ Deixar primeiras letras das palavras maiúsculas:
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}
# (Adaptado de: Andrie, Stackoverflow - 
# https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string ).

## II. P/ ajustar o formato do CPF:
CPFformat <- function(x) {
  x <- as.character(x)
  x <- str_pad(x, 11, pad = "0")
  part1<-substring(x, 1,3)
  part2 <- substring(x,4,6)
  part3 <- substring(x,7,9)
  part4 <- substring(x,10)
  paste0(part1,".",part2,".",part3,"-",part4)
}

# 01. Substituir #NE# p/ "Indisponivel"----
  ##1.0. Mas antes... Passar e-mail p/ caixa baixa
  df_carreiras$EMAIL_CANDIDATO <- tolower(df_carreiras$EMAIL_CANDIDATO)
## 1.1. no e-mail dos candidatos
df_carreiras$EMAIL_CANDIDATO[df_carreiras$EMAIL_CANDIDATO=="#ne#"] <- "Indisponivel"
df_carreiras$EMAIL_CANDIDATO[df_carreiras$EMAIL_CANDIDATO=="#nulo#"] <- "Indisponivel"

## 1.2. Na sigla e composição da coligacao
df_carreiras$SIGLA_COLIGACAO[df_carreiras$SIGLA_COLIGACAO=="#NE#"] <- "Indisponivel"
df_carreiras$SIGLA_COLIGACAO[df_carreiras$SIGLA_COLIGACAO=="#NULO#"] <- "Indisponivel"
df_carreiras$COMPOSICAO_COLIGACAO[df_carreiras$COMPOSICAO_COLIGACAO=="#NE#"] <- "Indisponivel"
df_carreiras$COMPOSICAO_COLIGACAO[df_carreiras$COMPOSICAO_COLIGACAO=="#NULO#"] <- "Indisponivel"
## 1.3. Na descrição COR/RAÇA
df_carreiras$DESCRICAO_COR_RACA[df_carreiras$DESCRICAO_COR_RACA=="#NE#"] <- "Indisponivel"
## 1.4. Idade
df_carreiras$IDADE_DATA_ELEICAO[df_carreiras$IDADE_DATA_ELEICAO<0] <- "Indisponivel"

# 02. Deixar a primeira letra do nome maiúscula e o resto minúscula ----
## 2.1. nos nomes dos candidatos
df_carreiras$NOME_CANDIDATO <- lapply(df_carreiras$NOME_CANDIDATO, simpleCap)
## 2.2. no nome dos cargos
df_carreiras$DESCRICAO_CARGO <- lapply(df_carreiras$DESCRICAO_CARGO, simpleCap)
## 2.3. descrição UE
df_carreiras$DESCRICAO_UE <- lapply(df_carreiras$DESCRICAO_UE, simpleCap)
## 2.4. descrição situação candidatura
df_carreiras$DES_SITUACAO_CANDIDATURA <- lapply(df_carreiras$DES_SITUACAO_CANDIDATURA, simpleCap)
## 2.5. descrição COR/RAÇA
df_carreiras$DESCRICAO_COR_RACA <-lapply(df_carreiras$DESCRICAO_COR_RACA, simpleCap)
## 2.5. Grau de instrução
df_carreiras$DESCRICAO_GRAU_INSTRUCAO <- lapply(df_carreiras$DESCRICAO_GRAU_INSTRUCAO, simpleCap)
df_carreiras$DESCRICAO_GRAU_INSTRUCAO[df_carreiras$DESCRICAO_GRAU_INSTRUCAO=="Lê E Escreve"] <- "Lê e Escreve"
## 2.6. Ocupação
df_carreiras$DESCRICAO_OCUPACAO <- lapply(df_carreiras$DESCRICAO_OCUPACAO, simpleCap)
## 2.7. Nome municipio de nascimento
df_carreiras$NOME_MUNICIPIO_NASCIMENTO <- lapply(df_carreiras$NOME_MUNICIPIO_NASCIMENTO, simpleCap)
## 2.8. Nacionalidade
df_carreiras$DESCRICAO_NACIONALIDADE <- lapply(df_carreiras$DESCRICAO_NACIONALIDADE,simpleCap)
## 2.9. Sexo e estado civil
df_carreiras$DESCRICAO_SEXO <- lapply(df_carreiras$DESCRICAO_SEXO,simpleCap)
df_carreiras$DESCRICAO_ESTADO_CIVIL <- lapply(df_carreiras$DESCRICAO_ESTADO_CIVIL, simpleCap)
      # Obs.: df_carreiras$DESC_SIT_TOT_TURNO não dá para usar esta função p/ alterar o texto 
      #porque tem "/" ao inves de espaço.

# 03. Colocar dígitos e pontinhos no CPF -----
df_carreiras$CPF_CANDIDATO <- lapply(df_carreiras$CPF_CANDIDATO, CPFformat)

# 04. Criamos a variável Voto com ponto entre os milhares (mas esta variável é character) -----
df_carreiras$Votos <- format(df_carreiras$QTDE_VOTOS,big.mark = ".",decimal.mark = ",")

# 05. Data de nascimento -----
# Vamos tentar identificar padroes:
datas1998 <- df_carreiras$DATA_NASCIMENTO[df_carreiras$ANO_ELEICAO==1998]
datas1998
    # Formatos encontrados:
    # A. "08-APR-52" 
    # B. "25091952"
    # C. "29/05/1963"
datas2002 <- df_carreiras$DATA_NASCIMENTO[df_carreiras$ANO_ELEICAO==2002]
datas2002 # Idem anterior e NA
datas2018 <- df_carreiras$DATA_NASCIMENTO[df_carreiras$ANO_ELEICAO==2018]
datas2018 # Já está tudo padronizada para dd/mm/aaaa (Formato C acima) -> é este o padrão que vamos adotar.
rm(datas1998,datas2002,datas2018)

# Arrumando o formato A
# Vamos separar o banco em dois: Data tipo A e o restante p/ fazer as alterações.
df_carreiras %>%
  mutate(tipoA = if_else(str_count(DATA_NASCIMENTO, "-") == 2,1,0)) -> df_carreiras

df_carreiras %>%
  filter(tipoA==1)-> tipoA # Filtrando observacoes com datas tipo A

df_carreiras <- df_carreiras[df_carreiras$tipoA != 1,] # Tirando Tipo A da base grande

tipoA$DATA_NASCIMENTO <- str_pad(tipoA$DATA_NASCIMENTO, 9, pad = "0") # Insere um 0 antes 
#dos strings que tem menos de 9 digitos, por exemplo 1-JAN-87 (1º de janeiro de 1987)
tipoA$mes <- substring(tipoA$DATA_NASCIMENTO,4,6) # criando variável com o mes de nascimento
# Arrumando meses:
tipoA$mes[tipoA$mes=="JAN"] <- "01"
tipoA$mes[tipoA$mes=="FEB"] <- "02"
tipoA$mes[tipoA$mes=="MAR"] <- "03"
tipoA$mes[tipoA$mes=="APR"] <- "04"
tipoA$mes[tipoA$mes=="MAY"] <- "05"
tipoA$mes[tipoA$mes=="JUN"] <- "06"
tipoA$mes[tipoA$mes=="JUL"] <- "07"
tipoA$mes[tipoA$mes=="AUG"] <- "08"
tipoA$mes[tipoA$mes=="SEP"] <- "09"
tipoA$mes[tipoA$mes=="OCT"] <- "10"
tipoA$mes[tipoA$mes=="NOV"] <- "11"
tipoA$mes[tipoA$mes=="DEC"] <- "12"

tipoA$DATA_NASCIMENTO <- paste0(substring(tipoA$DATA_NASCIMENTO,1,2),"/",
                                tipoA$mes,"/19",
                                (substring(tipoA$DATA_NASCIMENTO,8, 9)))
tipoA$mes <- NULL


# Arrumando formato B:
df_carreiras$DATA_NASCIMENTO <- str_pad(df_carreiras$DATA_NASCIMENTO, 8, pad = "0") # Insere um 0 antes 
#dos strings que tem menos de 8 digitos, por exemplo 1011987 (1º de janeiro de 1987)

df_carreiras$DATA_NASCIMENTO <- str_replace(string = df_carreiras$DATA_NASCIMENTO,
                                            pattern = "([0-9]{2})([0-9]{2})([0-9]{4})",
                                            replacement = "\\1/\\2/\\3")

df_carreiras <- rbind(df_carreiras,tipoA) # juntando as bases novamente
df_carreiras$DATA_NASCIMENTO<-as.Date(df_carreiras$DATA_NASCIMENTO, format="%d/%m/%Y") # ajustando formato
rm(tipoA) # removendo banco
df_carreiras$tipoA <- NULL # Apagando variável auxiliar.

# deixar nome das colunas bonitos -----
colnames(df_carreiras) <- c("Número do Título Eleitoral",
                            "Ano da Eleição",
                            "Nº do Turno",
                            "Cargo",
                            "Sigla da Unidade Eleitoral",
                            "Unidade Eleitoral",
                            "Situação da Candidatura",
                            "Sigla do Partido",
                            "Sigla da Coligação",
                            "Composição da Coligação",
                            "Quantidade de Votos",
                            "Situação de Totalização do Turno",
                            "Última Eleição Concorrida",
                            "Chave",
                            "Nome","CPF","Sexo","Cor ou Raça",
                            "Grau de Instrução",
                            "Ocupação",
                            "Estado Civil","Nacionalidade",
                            "Estado de Nascimento","Município de Nascimento",
                            "Data de Nascimento","Idade na data da eleição",
                            "E-mail","Votos")
df <-df_carreiras
rm(df_carreiras)
# salvar banco final pronto ! -----
save(df, file = "df.RData")

# removendo todos os objetos ----
rm(list = ls())
