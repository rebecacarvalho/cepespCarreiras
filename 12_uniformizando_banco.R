library(stringr)

x <- career(nome = "Haddad", nome_urna = T)
x <- x %>% distinct(NUM_TITULO_ELEITORAL_CANDIDATO, .keep_all = T)

args <- expand.grid(NUM_TITULO_ELEITORAL_CANDIDATO = x$NUM_TITULO_ELEITORAL_CANDIDATO)
df_carreiras <- purrr::pmap(args, cepespR::get_careers_elections)

x<- NA
for (i in df_carreiras) {
        x <- rbind(x,as.data.frame(i))
}
x[1,] <- NULL
df_carreiras <- as.data.frame(x)



# 01. Substituir #NE# p/ "Indisponivel"----
##1.0. Mas antes... Passar e-mail p/ caixa baixa
df_carreiras$EMAIL_CANDIDATO <- tolower(df_carreiras$EMAIL_CANDIDATO)
## 1.1. no e-mail dos candidatos
df_carreiras$EMAIL_CANDIDATO[df_carreiras$EMAIL_CANDIDATO=="#ne#"] <- "Indisponivel"
df_carreiras$EMAIL_CANDIDATO[df_carreiras$EMAIL_CANDIDATO=="#nulo#"] <- "Indisponivel"

## 1.2. Na sigla e composição da coligacao
df_carreiras$SIGLA_LEGENDA[df_carreiras$SIGLA_LEGENDA=="#NE#"] <- "Indisponivel"
df_carreiras$SIGLA_LEGENDA[df_carreiras$SIGLA_COLIGACAO=="#NULO#"] <- "Indisponivel"
df_carreiras$COMPOSICAO_LEGENDA[df_carreiras$COMPOSICAO_LEGENDA=="#NE#"] <- "Indisponivel"
df_carreiras$COMPOSICAO_LEGENDA[df_carreiras$COMPOSICAO_LEGENDA=="#NULO#"] <- "Indisponivel"
## 1.3. Na descrição COR/RAÇA
df_carreiras$DESCRICAO_COR_RACA[df_carreiras$DESCRICAO_COR_RACA=="#NE#"] <- "Indisponivel"
## 1.4. Idade
df_carreiras$IDADE_DATA_ELEICAO[df_carreiras$IDADE_DATA_ELEICAO<0] <- "Indisponivel"

# 02. Deixar a primeira letra do nome maiúscula e o resto minúscula ----
## 2.1. nos nomes dos candidatos
df_carreiras$NOME_CANDIDATO <- str_to_title(df_carreiras$NOME_CANDIDATO)
## 2.2. no nome dos cargos
df_carreiras$DESCRICAO_CARGO <- str_to_title(df_carreiras$DESCRICAO_CARGO)
## 2.3. descrição UE
df_carreiras$DESCRICAO_UE <- str_to_title(df_carreiras$DESCRICAO_UE)
## 2.4. descrição situação candidatura
df_carreiras$DES_SITUACAO_CANDIDATURA <- str_to_title(df_carreiras$DES_SITUACAO_CANDIDATURA)
## 2.5. descrição COR/RAÇA
df_carreiras$DESCRICAO_COR_RACA <-str_to_title(df_carreiras$DESCRICAO_COR_RACA)
## 2.5. Grau de instrução
df_carreiras$DESCRICAO_GRAU_INSTRUCAO <- str_to_title(df_carreiras$DESCRICAO_GRAU_INSTRUCAO)
df_carreiras$DESCRICAO_GRAU_INSTRUCAO[df_carreiras$DESCRICAO_GRAU_INSTRUCAO=="Lê E Escreve"] <- "Lê e Escreve"
## 2.6. Ocupação
df_carreiras$DESCRICAO_OCUPACAO <- str_to_title(df_carreiras$DESCRICAO_OCUPACAO)
## 2.7. Nome municipio de nascimento
df_carreiras$NOME_MUNICIPIO_NASCIMENTO <- str_to_title(df_carreiras$NOME_MUNICIPIO_NASCIMENTO)
## 2.8. Nacionalidade
df_carreiras$DESCRICAO_NACIONALIDADE <- str_to_title(df_carreiras$DESCRICAO_NACIONALIDADE)
## 2.9. Sexo e estado civil
df_carreiras$DESCRICAO_SEXO <- str_to_title(df_carreiras$DESCRICAO_SEXO)
df_carreiras$DESCRICAO_ESTADO_CIVIL <- str_to_lower(df_carreiras$DESCRICAO_ESTADO_CIVIL)
df_carreiras$DESCRICAO_ESTADO_CIVIL <- paste0(str_to_upper(substring(df_carreiras$DESCRICAO_ESTADO_CIVIL,1,1)), 
                                              substring(df_carreiras$DESCRICAO_ESTADO_CIVIL,2))
# Obs.: df_carreiras$DESC_SIT_TOT_TURNO não dá para usar esta função p/ alterar o texto 
#porque tem "/" ao inves de espaço.

# 03. Colocar dígitos e pontinhos no CPF -----
df_carreiras$CPF_CANDIDATO <- as.character(df_carreiras$CPF_CANDIDATO)
df_carreiras$CPF_CANDIDATO  <- str_pad(df_carreiras$CPF_CANDIDATO, 11, pad = "0")
df_carreiras$CPF_CANDIDATO <- paste0(substring(df_carreiras$CPF_CANDIDATO, 1,3),".",
                                     substring(df_carreiras$CPF_CANDIDATO, 4,6),".",
                                     substring(df_carreiras$CPF_CANDIDATO, 7,9),"-",
                                     substring(df_carreiras$CPF_CANDIDATO, 10))


# 04. Criamos a variável Voto com ponto entre os milhares (mas esta variável é character) -----
df_carreiras$TOTAL_VOTACAO <- as.numeric(df_carreiras$TOTAL_VOTACAO)
df_carreiras$Votos <- format(df_carreiras$TOTAL_VOTACAO,big.mark = ".",decimal.mark = ",")
# O mesmo p/ despesa máxima de campanha ----
df_carreiras$DESPESA_MAX_CAMPANHA <- as.numeric(df_carreiras$DESPESA_MAX_CAMPANHA)
df_carreiras$DESPESA_MAX_CAMPANHA <- format(df_carreiras$DESPESA_MAX_CAMPANHA,
                                            big.mark = ".",decimal.mark = ",")
df_carreiras$DESPESA_MAX_CAMPANHA[df_carreiras$DESPESA_MAX_CAMPANHA=="        -1"] <- "Indisponivel"


# 05. Data de nascimento -----
# Formatos:
# A. "08-APR-52" 
# B. "25091952"
# C. "29/05/1963"
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

# Apagando coluna 3 (Descrição Eleição) ----
df_carreiras$DESCRICAO_ELEICAO <- NULL

# 06. deixar nome das colunas bonitos -----
colnames(df_carreiras) <- c("Número do Título Eleitoral",
                            "Ano da Eleição",
                            "Nº do Turno",
                            "Cargo",
                            "Sigla da Unidade Eleitoral",
                            "Unidade Eleitoral",
                            "Situação da Candidatura",
                            "Número de urna",
                            "Nome de urna da eleição",
                            "Sigla do Partido",
                            "Sigla da Coligação",
                            "Composição da Coligação",
                            "Quantidade de Votos",
                            "Situação de Totalização do Turno",
                            "Última Eleição Concorrida",
                            "Chave",
                            "Nome","Nome de urna (última eleição)","CPF","Sexo","Cor ou Raça",
                            "Grau de Instrução",
                            "Ocupação",
                            "Estado Civil","Nacionalidade",
                            "Estado de Nascimento","Município de Nascimento",
                            "Data de Nascimento","Idade na data da eleição",
                            "E-mail","Votos")

# 07. Inserir sigla atual dos partidos que mudaram de nome (Créditos: Mauricio) -----
df_carreiras$`Sigla Atual do Partido` <- recode(df$`Sigla do Partido`,
                                      "DEM" = "PFL>DEM",
                                      "GOV" = "GOV",
                                      "LID" = "LID",
                                      "MINORI" = "MINORI",
                                      "PAN" = "PAN",
                                      "PCB" = "PCB>PPS",
                                      "PCdoB" = "PCdoB",
                                      "PCN" = "PCN",
                                      "PDC" = "PDC",
                                      "pDIR" = "pDIR",
                                      "PDS" = "PDS>PP",
                                      "PDT" = "PDT",
                                      "PEN" = "PEN",
                                      "pESQ" = "pESQ",
                                      "PFL" = "PFL>DEM",
                                      "PHS" = "PHS",
                                      "PJ" = "PJ>PTC",
                                      "PL" = "PL>PR",
                                      "PLENAR" = "PLENAR",
                                      "PLP" = "PLP",
                                      "PMB" = "PMB",
                                      "PMB_1" = "PMB_1",
                                      "PMDB" = "PMDB",
                                      "PMN" = "PMN",
                                      "PMR" = "PMR>PRB",
                                      "PP" = "PDS>PP",
                                      "PP_1" = "PP_1",
                                      "PP_2" = "PP_2",
                                      "PPB" = "PDS>PP",
                                      "PPB_1" = "PPB_1",
                                      "PPL" = "PPL",
                                      "PPR" = "PDS>PP",
                                      "PPS" = "PCB>PPS",
                                      "PR" = "PL>PR",
                                      "PRB" = "PMR>PRB",
                                      "PRN" = "PJ>PTC",
                                      "PRONA" = "PRONA",
                                      "PROS" = "PROS",
                                      "PRP" = "PRP",
                                      "PRS" = "PRS",
                                      "PRTB" = "PRTB",
                                      "PSB" = "PSB",
                                      "PSC" = "PSC",
                                      "PSD" = "PSD",
                                      "PSDB" = "PSDB",
                                      "PSDC" = "PSDC",
                                      "PSL" = "PSL",
                                      "PSOL" = "PSOL",
                                      "PST" = "PST",
                                      "PSTU" = "PSTU",
                                      "PT" = "PT",
                                      "PTB" = "PTB",
                                      "PTC" = "PJ>PTC",
                                      "PTdoB" = "PTdoB",
                                      "PTN" = "PTN",
                                      "PTR" = "PTR",
                                      "PV1" = "PV",
                                      "REDE" = "REDE",
                                      "S/PART" = "S/PART",
                                      "SDD" = "SDD")
