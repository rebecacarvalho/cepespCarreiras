###########################################################################
##### Abrindo bases baixadas do Repositório de Dados Eleitorais do TSE ####
###########################################################################

library(readr)
library(data.table)

anos<-seq(1994,2018,2)

#Anos de 1996 a 2012:

for(g in 2:10){
  
  cand<-NULL
  
  files<-list.files(paste("consulta_cand_",anos[g],sep=""),pattern = ".txt")
  
  for (i in 1:length(files)) {
    
    dir<-paste("consulta_cand_",anos[g],"/",files[i],sep = "")
    
    temp<-read_delim(dir, 
                     ";", escape_double = FALSE, col_names = FALSE, 
                     locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)
    
    cand<-rbind(cand,temp[!is.na(temp$X2),])
    
  }
  
  assign(paste("cand",anos[g],sep = "_"),cand)
  
}

#Ano de 1994:

g<-1

cand<-NULL

files<-list.files(paste("consulta_cand_",anos[g],sep=""),pattern = ".txt")

for (i in 1:length(files)) {
  
  if(i==6){
    
    dir<-paste("consulta_cand_",anos[g],"/",files[i],sep = "")
    
    temp<-read_delim(dir, 
                     ";", escape_double = FALSE, col_names = FALSE, 
                     locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)
    
    temp$X22<-NULL
    
    colnames(temp)<-paste(rep("X",43),1:43,sep = "")
    
    cand<-rbind(cand,temp[!is.na(temp$X2),])}else{
      
      dir<-paste("consulta_cand_",anos[g],"/",files[i],sep = "")
      
      temp<-read_delim(dir, 
                       ";", escape_double = FALSE, col_names = FALSE, 
                       locale = locale(encoding = "ISO-8859-1"), 
                       trim_ws = TRUE)
      
      cand<-rbind(cand,temp[!is.na(temp$X2),])
      
    }
  
}

assign(paste("cand",anos[g],sep = "_"),cand)

rm(cand)
rm(temp)

#Anos de 2014 a 2018


for(g in 11:13){
  
  dir<-paste("consulta_cand_",anos[g],"/consulta_cand_",anos[g],"_BRASIL.csv",sep="")
  
  assign(paste("cand",anos[g],sep = "_"),read_delim(dir, 
                                                    ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                                    trim_ws = TRUE))
}

#save.image(file="bases_cand_TSE.RData")  

#Juntar bases e ajustar nome das variáveis:

temp1<-rbind.data.frame(cand_1994, cand_1996, cand_1998, cand_2000, cand_2002, cand_2004, cand_2006,
                        cand_2008, cand_2010)  

colnames(temp1)<-c('DATA_GERACAO',
                   'HORA_GERACAO',
                   'ANO_ELEICAO',
                   'NUM_TURNO' ,
                   'DESCRICAO_ELEICAO' ,
                   'SIGLA_UF' ,
                   'SIGLA_UE' ,
                   'DESCRICAO_UE' ,
                   'CODIGO_CARGO' ,
                   'DESCRICAO_CARGO',
                   'NOME_CANDIDATO' ,
                   'SEQUENCIAL_CANDIDATO' ,
                   'NUMERO_CANDIDATO' ,
                   'CPF_CANDIDATO' ,
                   'NOME_URNA_CANDIDATO' ,
                   'COD_SITUACAO_CANDIDATURA' ,
                   'DES_SITUACAO_CANDIDATURA' ,
                   'NUMERO_PARTIDO' ,
                   'SIGLA_PARTIDO' ,
                   'NOME_PARTIDO' ,
                   'CODIGO_LEGENDA' ,
                   'SIGLA_LEGENDA' ,
                   'COMPOSICAO_LEGENDA' ,
                   'NOME_LEGENDA' ,
                   'CODIGO_OCUPACAO' ,
                   'DESCRICAO_OCUPACAO' ,
                   'DATA_NASCIMENTO',
                   'NUM_TITULO_ELEITORAL_CANDIDATO' ,
                   'IDADE_DATA_ELEICAO' ,
                   'CODIGO_SEXO' ,
                   'DESCRICAO_SEXO' ,
                   'COD_GRAU_INSTRUCAO' ,
                   'DESCRICAO_GRAU_INSTRUCAO',
                   'CODIGO_ESTADO_CIVIL' ,
                   'DESCRICAO_ESTADO_CIVIL',
                   'CODIGO_NACIONALIDADE' ,
                   'DESCRICAO_NACIONALIDADE' ,
                   'SIGLA_UF_NASCIMENTO',
                   'CODIGO_MUNICIPIO_NASCIMENTO' ,
                   'NOME_MUNICIPIO_NASCIMENTO',
                   'DESPESA_MAX_CAMPANHA' ,
                   'COD_SIT_TOT_TURNO' ,
                   'DESC_SIT_TOT_TURNO')  

colnames(cand_2012)<-c(colnames(temp1),'NM_EMAIL')

nm<-data.frame(c(colnames(cand_2012),rep(0,14)),colnames(cand_2018))

write.table(nm,file="nomes.csv",sep=";",row.names = F) #Organizar manualmente a comparação entre os nomes mais atigos e mais recentes das variáveis

nm<-fread("nomes_at.csv",sep=";")
nm$novo_at<-nm$novo
nm[!nm$novo_at=="",]$novo_at<-nm[!nm$novo_at=="",]$antigo
nm[nm$novo_at=="",]$novo_at<-nm[nm$novo_at=="",]$novo


temp2<-rbind.data.frame(cand_2014,cand_2016,cand_2018)
colnames(temp2)<-nm[!nm$novo_at=="",]$novo_at

base_cand<-bind_rows(temp1,cand_2012,temp2)

saveRDS(base_cand,file = "base_cand.rds")

##############################################################################################
######################## Criação do ID_cepesp para candidatos únicos  ########################
##############################################################################################

rm(list=ls())
library(tidyverse)
library(cepespR)
library(stringr)
library(stringi)
library(data.table)

####### 1 - Abrindo e organizando os dados #######

cand_national <- readRDS("base_cand.rds")

# Remove cargos TRUE e Plebiscitos:
cand_national <- subset(cand_national, cand_national$DESCRICAO_CARGO != "TRUE")
cand_national <- cand_national[!(cand_national$DESCRICAO_ELEICAO == "PLEBISCITO"),]


# Excluindo segundo turno (nao precisa dele)
df <- cand_national %>% 
  filter(NUM_TURNO==1)

df$NUM_TURNO<-NULL


# Chamando o distrital de estadual:
df<-df %>% 
  mutate(DESCRICAO_CARGO = if_else(DESCRICAO_CARGO == "DEPUTADO DISTRITAL",
                                   "DEPUTADO ESTADUAL",
                                   DESCRICAO_CARGO)) 


saveRDS(df,'df.rds')

# df <- readRDS("df.rds")

####### 2 - Padronizando data de nascimento, CPF e titulo ####### 
######### 2.1 DATA ######### 

# Formatos:
# A. "08-APR-52"   (2010)
# B. "08041952"    (1998,2002,2004)
# C. "08/04/1952"  (2006,2008,2012,2014,2016,2018)
# D. "08/04/52" (1996,1994)

# Vamos colocar todas as datas no formato C

## Formato D ##

df[df$ANO_ELEICAO%in%c(1996,1994),]$DATA_NASCIMENTO<-paste(substring(df[df$ANO_ELEICAO%in%c(1996,1994),]$DATA_NASCIMENTO,1,6),"19",substring(df[df$ANO_ELEICAO%in%c(1996,1994),]$DATA_NASCIMENTO,7,8),sep="")


## Formato A ##

# Vamos separar o df em duas partes: com formato A e as demais
df<-df %>% # dummy que identifica o formato A
  mutate(tipoA = if_else(str_count(DATA_NASCIMENTO, "-") == 2,1,0)) 
# Verificando os tipos (21503 tipo A e 586 NA (585 no ano de 2002))
table(df$tipoA,df$ANO_ELEICAO, exclude = NULL)
# Gerando a base so com tipo A
tipoA <- filter(df, tipoA==1) 
# Retirando o tipo A da base principal (deixo NA e diferentes de tipo A)
df <- df %>% 
  filter(is.na(tipoA)|tipoA != 1) 
# Padronizando
tipoA$DATA_NASCIMENTO <- str_pad(tipoA$DATA_NASCIMENTO, 9, pad = "0") # Inserts a 0 before strings with less than 9 digits, for instance 1-JAN-87 (Jan 1st, 1987).
tipoA$mes <- substring(tipoA$DATA_NASCIMENTO,4,6) # Creating new variable: month of birth.
# Replacing month abbreviation with month number:
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

## Formato B ##
df$DATA_NASCIMENTO <- str_pad(df$DATA_NASCIMENTO, 8, pad = "0") # Inserts 0 in the 
#beginning of a string when it has less than 8 digits. For instance, 1011987 (Jan 1st, 1987).

df$DATA_NASCIMENTO <- str_replace(string = df$DATA_NASCIMENTO,
                                  pattern = "([0-9]{2})([0-9]{2})([0-9]{4})",
                                  replacement = "\\1/\\2/\\3")
# Retornando a base de tipo A
df <- rbind(df,tipoA)

df$DATA_NASCIMENTO<-as.Date(df$DATA_NASCIMENTO, format="%d/%m/%Y") # adjusting date format.
rm(tipoA); df$tipoA <- NULL # removendo auxiliar data.frame and dropping dummy variable.
# Criar ano de nascimento para remover os valores estranhos (<17 e >105)
df$ano_nasc <- as.numeric(format(df$DATA_NASCIMENTO,'%Y')) # creating a birth year variable.

# Dummy que identifica quem nao tem data de nascimento
df <- df %>% 
  mutate(semniver = if_else(ANO_ELEICAO - ano_nasc < 17 | ANO_ELEICAO - ano_nasc > 105 | 
                              is.na(ano_nasc)==T,1,0)) # Candidates with non-sence ages 


saveRDS(df,'df.rds')
# df <- readRDS("df.rds")

######### 2.2 CPF ######### 

# Tem varios CPF's 0 e 99999, vamos considera-lo NA
df <- df %>% 
  mutate(CPF_CANDIDATO = na_if(CPF_CANDIDATO,"00000000000")) %>% 
  mutate(CPF_CANDIDATO = na_if(CPF_CANDIDATO,"99999999999"))

# verificando os problemas
  # nrow(df[as.numeric(df$CPF_CANDIDATO)==0,]) 
  # summary(as.numeric(df$CPF_CANDIDATO)) 
  # sum(str_detect(df$CPF_CANDIDATO,"#N")) 
  # table(str_detect(df$CPF_CANDIDATO,"#N"), is.na(as.numeric(df$CPF_CANDIDATO))) 

# Retirando os espacos
df$CPF_CANDIDATO<-stringr::str_replace_all(df$CPF_CANDIDATO, ' ', '')

# Vamos chamar de NA esses casos (ao inves de #NULO#):
df$CPF_CANDIDATO<-as.numeric(df$CPF_CANDIDATO)
# Vejamos o numero de observacoes com menos de 11 digitos:
sum(nchar(grep("^[0-9]+$", df$CPF_CANDIDATO, value=TRUE)) <11) #Como na linha acima colocamos as.numeric, o 
# zero a esquerda nao aparece, de modo que devemos preencher
# Vamos uniformizar o CPF, colocando todos com 11 digitos. Para tanto, incluimos 0 a esquerda:
df$CPF_CANDIDATO <- stringr::str_pad(string=df$CPF_CANDIDATO,width=11,side='left',pad='0')
# salvando
saveRDS(df,'df.rds')
# df <- readRDS("df.rds")

######### 2.3 TITULO ######### 

# Tem varios titulos 0, vamos considera-lo NA
df <- df %>% 
  mutate(NUM_TITULO_ELEITORAL_CANDIDATO = na_if(NUM_TITULO_ELEITORAL_CANDIDATO,"000000000000"))

# summary(as.numeric(df$NUM_TITULO_ELEITORAL_CANDIDATO)) 

# Verificando o numero de NA
sum(str_detect(df$NUM_TITULO_ELEITORAL_CANDIDATO,"#N")==T, na.rm = T) 

# Arrumando os espacos
df$NUM_TITULO_ELEITORAL_CANDIDATO<-stringr::str_replace_all(df$NUM_TITULO_ELEITORAL_CANDIDATO, ' ', '') 

# Arrumando o -1 no titulo
df[grep("-1", df$NUM_TITULO_ELEITORAL_CANDIDATO), "NUM_TITULO_ELEITORAL_CANDIDATO"] <- NA
df[grep("-4", df$NUM_TITULO_ELEITORAL_CANDIDATO), "NUM_TITULO_ELEITORAL_CANDIDATO"] <- NA


# Colocando como NA os casos de 00000000#NI#:
df$NUM_TITULO_ELEITORAL_CANDIDATO<-as.numeric(df$NUM_TITULO_ELEITORAL_CANDIDATO)

# Colocando 12 digitos:
#summary(nchar(df$NUM_TITULO_ELEITORAL_CANDIDATO)) # maximo de 12 digitos
df$NUM_TITULO_ELEITORAL_CANDIDATO <- stringr::str_pad(string=df$NUM_TITULO_ELEITORAL_CANDIDATO,
                                                      width=12,side='left',pad='0')
# Dummy que identifica quem tem titulo
df <- df %>% 
  mutate(temtitulo = ifelse(is.na(NUM_TITULO_ELEITORAL_CANDIDATO)==T,0,1))


# salvando
saveRDS(df,'df.rds')
# df <- readRDS("df.rds")

####### 3 - Conferindo Titulo e CPF ####### 
####### 3.1 - CPF ####### 

# Usando a funcao de conferir o CPF
source("function_cpf.R")

# Vou chamar o NA de um numero que ira identifica-los como sem CPF pela funcao
df$CPF_CANDIDATO <- ifelse(is.na(df$CPF_CANDIDATO) == T, "99999999919", df$CPF_CANDIDATO)

# Verificando os CPF's
df$check_cpf <- cpp_check_cpf(df$CPF_CANDIDATO)
table(df$check_cpf)

# Vamos chamar de NA os CPF's invalidos
df$CPF_CANDIDATO <- ifelse(df$check_cpf== F, NA, df$CPF_CANDIDATO)
summary(as.numeric(df$CPF_CANDIDATO)) 

df <- df %>%
  mutate(temcpf = ifelse(is.na(CPF_CANDIDATO)==T,0,1)) 

saveRDS(df,'df_check.rds')
# df <- readRDS("df_check.rds")


####### 4 - Criando ID ?nico ####### 

######### 4.0 - Limpando a base ######### 

# Tirando quem tem NA em CPF e TIT (nao consigo criar identificador)
temp <- which(is.na(df$CPF_CANDIDATO) == T & is.na(df$NUM_TITULO_ELEITORAL_CANDIDATO) == T)
full_df <- df[-temp,]
# Tirando os duplicados nos tres identificadores:
full_df<-full_df %>% 
  select(CPF_CANDIDATO,NUM_TITULO_ELEITORAL_CANDIDATO,DATA_NASCIMENTO,NOME_CANDIDATO) %>%
  distinct(CPF_CANDIDATO,NUM_TITULO_ELEITORAL_CANDIDATO,DATA_NASCIMENTO, .keep_all = T) #Manter as variaveis
# Verificando os duplicados em dois (NAO PRECISO RODAR ISSO, VOU SO PRA VER)
#full_df$flagTIT <- duplicated(full_df$NUM_TITULO_ELEITORAL_CANDIDATO)
#full_df$flagCPF <- duplicated(full_df$CPF_CANDIDATO)
#table(full_df$flagTIT,full_df$flagCPF)
#full_df$flagTIT <- NULL
#full_df$flagCPF <- NULL

######### 4.1 Base auxiliar 1: CPF e TIT ######### 
full_cpf_tit <- subset(full_df, is.na(full_df$CPF_CANDIDATO) == F) # Tira quem nao tem CPF
full_cpf_tit <- subset(full_cpf_tit, is.na(full_cpf_tit$NUM_TITULO_ELEITORAL_CANDIDATO) == F) # Tira quem nao tem TIT

full_cpf_tit <- full_cpf_tit %>%
  select(CPF_CANDIDATO,NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  distinct()
full_cpf_tit$id_cpf_tit <- c(1:nrow(full_cpf_tit))

final <- full_df %>% 
  left_join(full_cpf_tit, by = c("CPF_CANDIDATO","NUM_TITULO_ELEITORAL_CANDIDATO"))


######### 4.2 Base auxiliar 2: CPF e DATA ######### 
full_cpf_dat <- subset(full_df, is.na(full_df$CPF_CANDIDATO) == F)
full_cpf_dat <- subset(full_cpf_dat, is.na(full_cpf_dat$DATA_NASCIMENTO) == F)

full_cpf_dat <- full_cpf_dat %>%
  select(CPF_CANDIDATO,DATA_NASCIMENTO) %>% 
  distinct()
full_cpf_dat$id_cpf_dat <- c(1:nrow(full_cpf_dat))

final2 <- final %>% 
  left_join(full_cpf_dat, by = c("CPF_CANDIDATO","DATA_NASCIMENTO"))

######### 4.3 Base auxiliar 3: TIT e DATA ######### 
full_tit_dat <- subset(full_df, is.na(full_df$NUM_TITULO_ELEITORAL_CANDIDATO) == F)
full_tit_dat <- subset(full_tit_dat, is.na(full_tit_dat$DATA_NASCIMENTO) == F)

full_tit_dat <- full_tit_dat %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO,DATA_NASCIMENTO) %>% 
  select(NUM_TITULO_ELEITORAL_CANDIDATO,DATA_NASCIMENTO) %>% 
  distinct()
full_tit_dat$id_tit_dat <- c(1:nrow(full_tit_dat))

final3 <- final2 %>% 
  left_join(full_tit_dat, by = c("NUM_TITULO_ELEITORAL_CANDIDATO","DATA_NASCIMENTO"))

# Criamos 3 IDS, proximo passo e criar o ID final assegurando de que nao tem os erros

######### 4.4 Identificando erros ######### 
######### PROBLEMA I - ID's auxiliares inconsistentes ######### 

# a) Inconsistencia no CPF

final3$I<-1

temp1<-aggregate(final3$I,by=list(final3$CPF_CANDIDATO),FUN=sum)
colnames(temp1)<-c("CPF_CANDIDATO","n.cpf")

temp2<-unique(final3[,c("CPF_CANDIDATO","id_cpf_tit","I")])
temp2<-aggregate(temp2$I,by=list(temp2$CPF_CANDIDATO),FUN=sum)
colnames(temp2)<-c("CPF_CANDIDATO","n.cpf_tit")

temp3<-unique(final3[,c("CPF_CANDIDATO","id_cpf_dat","I")])
temp3<-aggregate(temp3$I,by=list(temp3$CPF_CANDIDATO),FUN=sum)  
colnames(temp3)<-c("CPF_CANDIDATO","n.cpf_dat")

temp4<-unique(final3[,c("CPF_CANDIDATO","id_tit_dat","I")])
temp4<-aggregate(temp4$I,by=list(temp4$CPF_CANDIDATO),FUN=sum)  
colnames(temp4)<-c("CPF_CANDIDATO","n.tit_dat")

teste.cpf<-left_join(temp1,temp2)
teste.cpf<-left_join(teste.cpf,temp3)
teste.cpf<-left_join(teste.cpf,temp4)
teste.cpf<-left_join(final3,teste.cpf)

# table(teste.cpf$n.cpf)
# 
# # Identificando os possiveis casos de problema 1 (tres ou mais cpfs repetidos)
# View(subset(teste.cpf, teste.cpf$n.cpf > 2)) 
# View(subset(teste.cpf, teste.cpf$CPF_CANDIDATO == "29722195034"|teste.cpf$CPF_CANDIDATO == "10769402771"|teste.cpf$CPF_CANDIDATO == "16117115920")) # Caso da foto no pdf
# # Esse caso so e um problema se nenhum ID identificar um unico individuo para os tres CPFS:
# View(subset(teste.cpf, (teste.cpf$n.cpf > 2 & teste.cpf$n.cpf_tit > 1) &    # Vendo os casos que deu problema 1
#               (teste.cpf$n.cpf > 2 & teste.cpf$n.cpf_dat > 1)))
# 
# table(subset(teste.cpf$n.cpf, (teste.cpf$n.cpf > 2 & teste.cpf$n.cpf_tit > 1) &
#                (teste.cpf$n.cpf > 2 & teste.cpf$n.cpf_dat > 1)))

# b) Inconsist?ncia no TITULO

final3$I<-1

temp1<-aggregate(final3$I,by=list(final3$NUM_TITULO_ELEITORAL_CANDIDATO),FUN=sum)
colnames(temp1)<-c("NUM_TITULO_ELEITORAL_CANDIDATO","n.tit")

temp2<-unique(final3[,c("NUM_TITULO_ELEITORAL_CANDIDATO","id_cpf_tit","I")])
temp2<-aggregate(temp2$I,by=list(temp2$NUM_TITULO_ELEITORAL_CANDIDATO),FUN=sum)
colnames(temp2)<-c("NUM_TITULO_ELEITORAL_CANDIDATO","n.cpf_tit")

temp3<-unique(final3[,c("NUM_TITULO_ELEITORAL_CANDIDATO","id_cpf_dat","I")])
temp3<-aggregate(temp3$I,by=list(temp3$NUM_TITULO_ELEITORAL_CANDIDATO),FUN=sum)  
colnames(temp3)<-c("NUM_TITULO_ELEITORAL_CANDIDATO","n.cpf_dat")

temp4<-unique(final3[,c("NUM_TITULO_ELEITORAL_CANDIDATO","id_tit_dat","I")])
temp4<-aggregate(temp4$I,by=list(temp4$NUM_TITULO_ELEITORAL_CANDIDATO),FUN=sum)  
colnames(temp4)<-c("NUM_TITULO_ELEITORAL_CANDIDATO","n.tit_dat")

teste.tit<-left_join(temp1,temp2)
teste.tit<-left_join(teste.tit,temp3)
teste.tit<-left_join(teste.tit,temp4)
teste.tit<-left_join(final3,teste.tit)


# table(teste.tit$n.tit)
# 
# View(subset(teste.tit, teste.tit$n.tit > 2)) 
# 
# View(subset(teste.tit, (teste.tit$n.tit > 2 & teste.tit$n.cpf_tit > 1) &    # Vendo os casos que deu problema 1
#               (teste.tit$n.tit > 2 & teste.tit$n.tit_dat > 1)))
# 
# table(subset(teste.tit$n.tit, (teste.tit$n.tit > 2 & teste.tit$n.cpf_tit > 1) &
#                (teste.tit$n.tit > 2 & teste.tit$n.tit_dat > 1)))

#View(subset(teste.tit, teste.tit$n.tit == 2))

######### 4.5 Separando as bases completas dos erros ######### 

# a) Criando o banco de inconsistencias no CPF

inconsist_cpf <- subset(teste.cpf, (teste.cpf$n.cpf > 2 & teste.cpf$n.cpf_tit > 1) &
                          (teste.cpf$n.cpf > 2 & teste.cpf$n.cpf_dat > 1))

# b) Criando o banco de inconsistencias no TIT

inconsist_tit<- subset(teste.tit, (teste.tit$n.tit > 2 & teste.tit$n.cpf_tit > 1) &
                         (teste.tit$n.tit > 2 & teste.tit$n.tit_dat > 1))

# c) Filtrando as inconsistencias da base completa

consist <- subset(final3,!(final3$CPF_CANDIDATO%in%unique(inconsist_cpf$CPF_CANDIDATO))) # Tira da base completa os cpfs inconsistentes
consist <- subset(consist,!(consist$NUM_TITULO_ELEITORAL_CANDIDATO%in%unique(inconsist_tit$NUM_TITULO_ELEITORAL_CANDIDATO))) # Tira da base completa os titulos inconsistentes

# OBSERVA??O ANTES DE CRIAR O ID


inconsist<-bind_rows(inconsist_cpf,inconsist_tit)

teste<-inconsist[inconsist$CPF_CANDIDATO=="04218639434",]
  teste<-teste[!is.na(teste$NOME_CANDIDATO),]

unique(teste[,1:7])



# Problema: NA no ID ? considerado como um ?nico indiv?duo
# Solu??o: inserir ID's nos NA's
# Dica: os n?meros n?o devem ter sido utilizados anteriormente para que n?o se misture com outros indiv?duos.
# Ent?o o n?mero do ID dever? come?ar a partir do ?ltimo ID+1 at? o ?ltimo ID + n?mero total de NA's

#Cria um vetor com os ID a partir do ?ltimo ID + 1 e termina no ?ltimo ID + n?mero total de NA's (1759) -> sum(is.na(consist$id_cpf_tit))
#O ultimo id e 94191 e tem 1759 NA. Logo, preenchemos os NA com o id 94191+1 (94192) ate 94191+738(94929)
inicio <- max(consist$id_cpf_tit, na.rm = T)+1
final <- max(consist$id_cpf_tit, na.rm = T) + sum(is.na(consist$id_cpf_tit))
consist[is.na(consist$id_cpf_tit),"id_cpf_tit"] <- c(inicio:final)

#Cria um vetor com os ID a partir do ?ltimo ID + 1 e termina no ?ltimo ID + n?mero total de NA's (700)
inicio <- max(consist$id_cpf_dat, na.rm = T)+1
final <- max(consist$id_cpf_dat, na.rm = T) + sum(is.na(consist$id_cpf_dat))
consist[is.na(consist$id_cpf_dat),"id_cpf_dat"] <- c(inicio:final)

#Cria um vetor com os ID a partir do ?ltimo ID + 1 e termina no ?ltimo ID + n?mero total de NA's (212)
inicio <- max(consist$id_tit_dat, na.rm = T)+1
final <- max(consist$id_tit_dat, na.rm = T) + sum(is.na(consist$id_tit_dat))
consist[is.na(consist$id_tit_dat),"id_tit_dat"] <- c(inicio:final)

######### 4.6 Criando o ID para as bases criadas ######### 

# a) Base sem inconsistencia

# Contando as repeticoes de cada ID
# Contando as vezes que o ID CPFTIT se repete
temp <- consist %>%
  group_by(id_cpf_tit) %>%
  summarize(n.cpf_tit = n())
consist <- consist %>%
  left_join(temp, by = "id_cpf_tit")
# Contando as vezes que o ID CPFDAT se repete
temp <- consist %>%
  group_by(id_cpf_dat) %>%
  summarize(n.cpf_dat = n())
consist <- consist %>%
  left_join(temp, by = "id_cpf_dat")
# Contando as vezes que o ID TITDAT se repete
temp <- consist %>%
  group_by(id_tit_dat) %>%
  summarize(n.tit_dat = n())
consist <- consist %>%
  left_join(temp, by = "id_tit_dat")

#Apos preencher os NA's, podemos criar o ID
consist$ID <- ifelse(consist$n.tit_dat > 1 & 
                       consist$n.cpf_dat == 1 & 
                       consist$n.cpf_tit == 1, paste0("tit_dat",consist$id_tit_dat),
                     ifelse(consist$n.cpf_dat > 1 &
                              consist$n.tit_dat == 1 &
                              consist$n.cpf_tit == 1, paste0("cpf_dat",consist$id_cpf_dat),
                            paste0("cpf_tit",consist$id_cpf_tit)))

#saveRDS(consist,'consist.rds')


# b) Base com inconsistencia no CPF
inconsist_cpf$nome <- tolower(inconsist_cpf$NOME_CANDIDATO) 
inconsist_cpf$nome <- gsub(" .*", "", inconsist_cpf$nome)
inconsist_cpf$nome <- iconv(inconsist_cpf$nome, from = "UTF-8", to = "ASCII//TRANSLIT")
inconsist_cpf$ID <- paste0(inconsist_cpf$CPF_CANDIDATO,inconsist_cpf$nome)
#saveRDS(inconsist_cpf,'inconsist_cpf.rds')



# c) Base com inconsistencia no TIT
inconsist_tit$nome <- tolower(inconsist_tit$NOME_CANDIDATO) 
inconsist_tit$nome <- gsub(" .*", "", inconsist_tit$nome)
inconsist_tit$nome <- iconv(inconsist_tit$nome, from = "UTF-8", to = "ASCII//TRANSLIT")
inconsist_tit$ID <- paste0(inconsist_tit$NUM_TITULO_ELEITORAL_CANDIDATO,inconsist_tit$nome)
#saveRDS(inconsist_tit,'inconsist_tit.rds')


# d) Unindo as bases

colunas <- c("CPF_CANDIDATO","NUM_TITULO_ELEITORAL_CANDIDATO","DATA_NASCIMENTO","ID")

base_completa <- inconsist_cpf[,colunas]
base_completa <- rbind(inconsist_tit[,colunas],base_completa)
base_completa <- rbind(base_completa,consist[,colunas]) 
#Nao sei pq essa base tem 4 obs a mais (soma das inconsiste com a base completa da 4 a mais)

# Id final
temp <- unique(base_completa$ID)
dic <- data.frame("ID" = temp, "id_cepesp" = seq(1:length(temp)))

base_completa <- base_completa %>%
  left_join(dic, by = "ID")

base_completa$ID <- NULL

# Base com ID
saveRDS(base_completa,'base_ID.rds')

######### 5 - Unindo o ID com a base original do CEPESP ######### 

teste <- df %>% 
  left_join(base_completa, by = c("CPF_CANDIDATO","NUM_TITULO_ELEITORAL_CANDIDATO","DATA_NASCIMENTO"))
#nrow(teste)==nrow(df) 

#View(subset(teste,teste$NUM_TITULO_ELEITORAL_CANDIDATO=="013665710353"))

# Base original com ID
  saveRDS(teste,'df_ID.rds')

  rm(list=ls())

######## 6 - Correções finais:
  
  library(stringi)
  library(stringdist)
  library(dplyr)
  
  base_cand_ID<-readRDS('df_ID.rds') 
  
  base_cand_ID$NOME_CANDIDATO<-stri_trans_general(base_cand_ID$NOME_CANDIDATO,"Latin-ASCII")
  
  ##Remover linhas duplicadas (idênticas, originárias dos dados do TSE):
  
  col<-c("ANO_ELEICAO","SIGLA_UE" ,                     
  "NUMERO_CANDIDATO","CPF_CANDIDATO",                 
  "NUM_TITULO_ELEITORAL_CANDIDATO","DATA_NASCIMENTO" ,              
  "NOME_CANDIDATO","SIGLA_UF",                      
  "DESCRICAO_CARGO","DES_SITUACAO_CANDIDATURA",      
  "DESCRICAO_ELEICAO","SIGLA_PARTIDO",                 
  "DESC_SIT_TOT_TURNO","id_cepesp")
  
  temp1<-base_cand_ID[base_cand_ID$ANO_ELEICAO<=2006,]
    temp1<-temp1[!duplicated(temp1[,col]),]
  temp2<-base_cand_ID[base_cand_ID$ANO_ELEICAO>2006,]
    temp2<-temp2[!duplicated(temp2[,col]),]
  
  base_cand_ID<-bind_rows(temp1,temp2)
  
  rm(list = ls(pattern = "temp"))
  
  ##Encontrar problemas de indivíduos idênticos com códigos diferentes (originárias do algoritmo do cepesp):
  
  base_cand_ID$D<-1*duplicated(base_cand_ID[,col[1:13]])

  base_cand_ID$id<-paste(base_cand_ID$ANO_ELEICAO,base_cand_ID$SIGLA_UE,base_cand_ID$NUMERO_CANDIDATO,
                         base_cand_ID$DESCRICAO_CARGO,base_cand_ID$DESCRICAO_ELEICAO,
                         base_cand_ID$DES_SITUACAO_CANDIDATURA,base_cand_ID$DESC_SIT_TOT_TURNO,sep = "_")
  
  teste<-base_cand_ID[base_cand_ID$id%in%base_cand_ID[base_cand_ID$D==1,]$id,]
  
  #View(teste) #já está organizado em ordem decrescente do id, 88 casos
  
  base_cand_ID<-base_cand_ID[base_cand_ID$D==0,]
  
  #saveRDS(base_cand_ID,'base_cand_ID_at.rds')  
  
  ##Procurar outras possíveis duplicações de candidatos (oiginárias do TSE):
  
  #Mesmos eleição-ue-número-nome-cargo-situações:
    
    teste<-base_cand_ID[base_cand_ID$id%in%base_cand_ID[duplicated(base_cand_ID$id),]$id,]
  
    teste$nom<-stri_trans_general(tolower(teste$NOME_CANDIDATO),"Latin-ASCII")
    teste<-teste[teste$nom%in%teste[duplicated(paste(teste$nom,teste$id)),]$nom,]
    
    temp<-anti_join(base_cand_ID,teste)
    
    #View(teste[,col]) #Checar dois casos estranhos: pares de ids "47089"/1474559 , 318338/"289418", "392756"/NA
    
    #View(base_cand_ID[base_cand_ID$id_cepesp=="289418",])
    
      #Remover manualmente esses três casos:
    
      teste<-teste[!teste$id_cepesp=="1474559",]
      teste<-teste[!teste$id_cepesp=="318338",]
      teste<-teste[!is.na(teste$id_cepesp),]
      
      teste<-teste[!duplicated(paste(teste$nom,teste$id)),]
      
      base_cand_ID<-bind_rows(temp,teste[,1:67])
    
  #Mesmos eleição-ue-número-id_cepesp-cargo-situações:
      
      teste<-base_cand_ID[base_cand_ID$id%in%base_cand_ID[duplicated(base_cand_ID$id),]$id,]
      
      teste$nom<-stri_trans_general(tolower(teste$NOME_CANDIDATO),"Latin-ASCII")
      teste<-teste[teste$id_cepesp%in%teste[duplicated(paste(teste$id_cepesp,teste$id)),]$id_cepesp,]
      teste<-teste[!is.na(teste$id_cepesp),]
      
      temp<-anti_join(base_cand_ID,teste)
      
      #View(teste) 
      
      teste<-teste[!duplicated(paste(teste$id_cepesp,teste$id)),]
      
      base_cand_ID<-bind_rows(temp,teste[,1:67])
      
      
  #Mesmos eleição-ue-número-cargo-situações e deferido:
      
      teste<-base_cand_ID[base_cand_ID$id%in%base_cand_ID[duplicated(base_cand_ID$id),]$id,]
      
      # unique(teste$DES_SITUACAO_CANDIDATURA)
      # 
      # [1] "RENÚNCIA"                   "INDEFERIDO"                
      # [3] "HOMOLOGAÇÃO DE RENÚNCIA"    "DEFERIDO"                  
      # [5] "CANCELADO"                  "INAPTO"                    
      # [7] "NÃO CONHECIMENTO DO PEDIDO"
      
      teste<-teste[teste$DES_SITUACAO_CANDIDATURA=="DEFERIDO",]
      
      temp<-anti_join(base_cand_ID,teste)
      
      #View(teste) #Problema específico da eleição de 2002 na UE 90514
      #Suplentes duplicados sem CPF e Título (serão removidos)
      
      teste<-teste[!is.na(teste$CPF_CANDIDATO),]
      
      base_cand_ID<-bind_rows(temp,teste)
      
      #Salvar base final de candidatos com ID:
      
      saveRDS(base_cand_ID,'base_cand_ID_at.rds')  
      