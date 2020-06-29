####################################################
#### Baixar dados de votação pela API do Cepesp ####
####################################################  

  library(cepespR)
  
  posi_G<-c("Deputado Estadual", "Deputado Federal", "Senador", "Governador","Presidente")
  posi_M<-c("Vereador", "Prefeito")
  
  votes_G<-NULL
  
  for (i in 1:4) {
  
  temp<-get_votes(year="1998, 2002, 2006, 2010, 2014, 2018", position = posi_G[i] ,regional_aggregation = "Estado" )    
  
  votes_G<-bind_rows(votes_G,temp)
  
  }
  
  temp<-get_votes(year="1998, 2002, 2006, 2010, 2014, 2018", position = posi_G[5] ,regional_aggregation = "Brazil" )    
  
  temp$SIGLA_UE<-"BR"
  
  votes_G<-bind_rows(votes_G,temp)
  
  for (i in 1:2) {
    
    temp<-get_votes(year="2000, 2004, 2008, 2012, 2016", position = posi_M[i] ,regional_aggregation = "Municipio" )    
    
    votes_G<-bind_rows(votes_G,temp)
    
  }

  base_votos<-votes_G
  rm(temp)
  rm(votes_G)
  
  saveRDS(base_votos,file = "base_votos.rds")
  
##########################################################
#### Unir base de candidatos com ID com base de votos ####
##########################################################  
  
  library(dplyr)
  
  rm(list = ls())
  
  col<-c("ANO_ELEICAO","SIGLA_UE" ,                     
         "NUMERO_CANDIDATO","CPF_CANDIDATO",                 
         "NUM_TITULO_ELEITORAL_CANDIDATO","DATA_NASCIMENTO" ,              
         "NOME_CANDIDATO","SIGLA_UF",                      
         "DESCRICAO_CARGO","DES_SITUACAO_CANDIDATURA",      
         "DESCRICAO_ELEICAO","SIGLA_PARTIDO",                 
         "DESC_SIT_TOT_TURNO","id_cepesp","id")
  
  df_ID <- readRDS("~/Projetos_R/cepesp_app_carreiras/base_cand_ID_at.rds")
    df_ID<-df_ID[,col]
    df_ID$ID<-1:nrow(df_ID)
    
  base_votos <- readRDS("~/Projetos_R/cepesp_app_carreiras/base_votos.rds")[,c(1:13,19)]
    base_votos[base_votos$DESCRICAO_CARGO=="DEPUTADO DISTRITAL"]$DESCRICAO_CARGO<-"DEPUTADO ESTADUAL"
    
  
  #Indicador de Eleição Suplementar:

  df_ID$I_supl<-0  
  
    df_ID[grep("supl",df_ID$DESCRICAO_ELEICAO,ignore.case=T,invert = F),]$I_supl<-1  
    df_ID[grep("nov",df_ID$DESCRICAO_ELEICAO,ignore.case=T,invert = F),]$I_supl<-1 
    df_ID[grep("elei",df_ID$DESCRICAO_ELEICAO,ignore.case=T,invert = T),]$I_supl<-1 
    df_ID[grep("[0-9]",df_ID$DESCRICAO_ELEICAO,ignore.case=T,invert = T),]$I_supl<-1  
  
  base_votos$I_supl<-0
  
    base_votos[grep("sup",base_votos$DESCRICAO_ELEICAO,ignore.case=T,invert = F),]$I_supl<-1
  
    
  #Unir bases:
    
  base_votos$teste<-1*duplicated(base_votos[,c("ANO_ELEICAO","SIGLA_UE","NUMERO_CANDIDATO",
                                              "DESCRICAO_CARGO","I_supl","NUM_TURNO")])
   
      #Um único duplicado: duas eleições suplementares no mesmo ano (2008) no mesmo município (09156)
  
      teste<-base_votos[base_votos$teste==1,c("ANO_ELEICAO","SIGLA_UE","NUMERO_CANDIDATO",
                           "DESCRICAO_CARGO","I_supl","NUM_TURNO")]
      teste$I<-1
      
      base_votos<-left_join(base_votos,teste)
      
        base_votos[is.na(base_votos$I),]$I<-0
        
        base_votos[base_votos$I==1&base_votos$DESCRICAO_ELEICAO=="ELEIÇÕES SUPLEMENTARES",]$I<-0
      
      df_ID<-left_join(df_ID,teste)
      
        df_ID[is.na(df_ID$I),]$I<-0
          
        df_ID[df_ID$I==1&df_ID$DESCRICAO_ELEICAO=="Eleições Suplementares",]$I<-0
      
        df_ID$NUM_TURNO<-NULL
  
        
  #Unir bases:      
  
  temp1<-left_join(df_ID,base_votos[base_votos$NUM_TURNO==1,],by=c("ANO_ELEICAO","SIGLA_UE","NUMERO_CANDIDATO",
                                                                   "DESCRICAO_CARGO","I_supl","I"))
  temp2<-left_join(df_ID,base_votos[base_votos$NUM_TURNO==2,],by=c("ANO_ELEICAO","SIGLA_UE","NUMERO_CANDIDATO",
                                                                   "DESCRICAO_CARGO","I_supl","I"), ) 
    temp2<-temp2[!is.na(temp2$NUM_TURNO),]
    
  df_ID_votos<-bind_rows(temp1,temp2)
  
  saveRDS(df_ID_votos,file = "df_ID_votos.rds")
       
  
  #Analisar a qualidade da junção e corrigir alguns problemas:
  
  temp<-unique(base_votos$DESCRICAO_CARGO)
  
  # [1] "DEPUTADO ESTADUAL" "DEPUTADO FEDERAL"  "SENADOR"           "GOVERNADOR"       
  # [5] "VEREADOR"          "PREFEITO"          "PRESIDENTE"   
  
  temp<-data.frame(temp,c(5,4,3,2,5,2,2),rep(1,7))
  
  colnames(temp)<-c("DESCRICAO_CARGO","num","ind_ok")
  
  base_votos$num<-nchar(base_votos$NUMERO_CANDIDATO)
  
  base_votos<-left_join(base_votos,temp)
  
  #nrow(base_votos[!is.na(base_votos$ind_ok),]) #2006425 Candidatos com votação nominal
  
  teste<-df_ID_votos[!is.na(df_ID_votos$NUM_TURNO),]
    teste$teste<-1*duplicated(teste[,c("ANO_ELEICAO","SIGLA_UE","NUMERO_CANDIDATO",
                                            "DESCRICAO_CARGO","I_supl","NUM_TURNO")]) 
  
    teste$id<-paste(teste$ANO_ELEICAO,teste$SIGLA_UE,teste$NUMERO_CANDIDATO,
                    teste$DESCRICAO_CARGO,teste$I_supl,teste$NUM_TURNO,sep = "_")
    
   #length(unique(teste$id)) #2006454 Candidatos com votação nominal
    
    teste<-teste[teste$id%in%teste[teste$teste==1,]$id,]
    
    
    temp<-anti_join(df_ID_votos,teste[,c(1:14,16:28)])
    
    #unique(teste$DES_SITUACAO_CANDIDATURA)
      
    #nrow(teste[teste$DES_SITUACAO_CANDIDATURA%in%c("APTO","DEFERIDO"),]) #6010 casos
    
    temp1<-teste[teste$id%in%teste[teste$DES_SITUACAO_CANDIDATURA%in%c("APTO","DEFERIDO"),]$id,]
    temp2<-anti_join(teste,temp1)
    
    #Casos em que consigo identificar que recebeu os votos:
    temp1[!temp1$DES_SITUACAO_CANDIDATURA%in%c("APTO","DEFERIDO"),]$QTDE_VOTOS<-NA
    
    #Casos em que não consigo identificar quem estava apto/deferido ou é o mesmo indivíduo mas com
      #situações de candidatura e de totalização diferentes ao longo da eleição:
      
      #View(temp2) #Nenhuma alteração foi feita
     
    #Atualizar base principal: 
      
    df_ID_votos<-bind_rows(temp,temp1,temp2)
     
    saveRDS(df_ID_votos,file = "df_ID_votos.rds")
      
 
    
##########################################
#### Ajustar a base para o aplicativo ####
########################################## 
         
    rm(list = ls())
    
    col<-c("ANO_ELEICAO","SIGLA_UE" ,                     
           "NUMERO_CANDIDATO","CPF_CANDIDATO",                 
           "NUM_TITULO_ELEITORAL_CANDIDATO","DATA_NASCIMENTO" ,              
           "NOME_CANDIDATO","SIGLA_UF",                      
           "DESCRICAO_CARGO","DES_SITUACAO_CANDIDATURA",      
           "DESCRICAO_ELEICAO","SIGLA_PARTIDO",                 
           "DESC_SIT_TOT_TURNO","id_cepesp","id")
    
    df_ID_votos<-readRDS("~/Projetos_R/cepesp_app_carreiras/df_ID_votos.rds")
    
    df_ID <- readRDS("~/Projetos_R/cepesp_app_carreiras/base_cand_ID_at.rds")
      df_ID<-df_ID[,!colnames(df_ID)%in%col]
      df_ID$ID<-1:nrow(df_ID)
    
    df_ID_votos<-left_join(df_ID_votos,df_ID[,c(3,6,11,12,18,15,22,30,41,24,20,25,27,16,53)])
    
    
    rm(df_ID)
    
      # colnames(df_ID)
      # [1] "DATA_GERACAO"                "HORA_GERACAO"               
      # [3] "DESCRICAO_UE"                "CODIGO_CARGO"               
      # [5] "SEQUENCIAL_CANDIDATO"        "NOME_URNA_CANDIDATO"        
      # [7] "COD_SITUACAO_CANDIDATURA"    "NUMERO_PARTIDO"             
      # [9] "NOME_PARTIDO"                "CODIGO_LEGENDA"             
      # [11] "SIGLA_LEGENDA"               "COMPOSICAO_LEGENDA"         
      # [13] "NOME_LEGENDA"                "CODIGO_OCUPACAO"            
      # [15] "DESCRICAO_OCUPACAO"          "IDADE_DATA_ELEICAO"         
      # [17] "CODIGO_SEXO"                 "DESCRICAO_SEXO"             
      # [19] "COD_GRAU_INSTRUCAO"          "DESCRICAO_GRAU_INSTRUCAO"   
      # [21] "CODIGO_ESTADO_CIVIL"         "DESCRICAO_ESTADO_CIVIL"     
      # [23] "CODIGO_NACIONALIDADE"        "DESCRICAO_NACIONALIDADE"    
      # [25] "SIGLA_UF_NASCIMENTO"         "CODIGO_MUNICIPIO_NASCIMENTO"
      # [27] "NOME_MUNICIPIO_NASCIMENTO"   "DESPESA_MAX_CAMPANHA"       
      # [29] "COD_SIT_TOT_TURNO"           "NM_EMAIL"                   
      # [31] "CD_TIPO_ELEICAO"             "NM_TIPO_ELEICAO"            
      # [33] "CD_ELEICAO"                  "DT_ELEICAO"                 
      # [35] "TP_ABRANGENCIA"              "NM_SOCIAL_CANDIDATO"        
      # [37] "CD_DETALHE_SITUACAO_CAND"    "DS_DETALHE_SITUACAO_CAND"   
      # [39] "TP_AGREMIACAO"               "NR_IDADE_DATA_POSSE"        
      # [41] "CD_COR_RACA"                 "DS_COR_RACA"                
      # [43] "ST_REELEICAO"                "ST_DECLARAR_BENS"           
      # [45] "NR_PROTOCOLO_CANDIDATURA"    "\\NR_PROCESSO\\\"\"\"\""    
      # [47] "ano_nasc"                    "semniver"                   
      # [49] "temtitulo"                   "check_cpf"                  
      # [51] "temcpf"                      "D"                          
      # [53] "ID"     
     
     df_ID_votos<-df_ID_votos[,c(1:14,19,26,30:42)]
     
     # colnames(df_ID_votos)
     # [1] "ANO_ELEICAO"                    "SIGLA_UE"                      
     # [3] "NUMERO_CANDIDATO"               "CPF_CANDIDATO"                 
     # [5] "NUM_TITULO_ELEITORAL_CANDIDATO" "DATA_NASCIMENTO"               
     # [7] "NOME_CANDIDATO"                 "SIGLA_UF"                      
     # [9] "DESCRICAO_CARGO"                "DES_SITUACAO_CANDIDATURA"      
     # [11] "DESCRICAO_ELEICAO.x"            "SIGLA_PARTIDO"                 
     # [13] "DESC_SIT_TOT_TURNO"             "id_cepesp"                     
     # [15] "NUM_TURNO"                      "QTDE_VOTOS"                    
     # [17] "DESCRICAO_UE"                   "NOME_URNA_CANDIDATO"           
     # [19] "SIGLA_LEGENDA"                  "COMPOSICAO_LEGENDA"            
     # [21] "DESCRICAO_SEXO"                 "DESCRICAO_OCUPACAO"            
     # [23] "DESCRICAO_ESTADO_CIVIL"         "NM_EMAIL"                      
     # [25] "CD_COR_RACA"                    "DESCRICAO_NACIONALIDADE"       
     # [27] "DESCRICAO_GRAU_INSTRUCAO"       "SIGLA_UF_NASCIMENTO"           
     # [29] "NOME_MUNICIPIO_NASCIMENTO"        
     
     colnames(df_ID_votos)<-c("Ano da Eleição",
                              "Sigla da Unidade Eleitoral",
                              "Número de urna",
                              "CPF",
                              "Número do Título Eleitoral",
                              "Data de Nascimento",
                              "Nome",
                              "Sigla da UF",
                              "Cargo",
                              "Situação da Candidatura",
                              "Eleição",
                              "Sigla do Partido",
                              "Situação de Totalização do Turno",
                              "id_cepesp",
                              "Nº do Turno",
                              "Quantidade de Votos",
                              "Unidade Eleitoral",
                              "Nome de urna",
                              "Sigla da Coligação",
                              "Composição da Coligação",
                              "Sexo",
                              "Ocupação",
                              "Estado Civil",
                              "E-mail",
                              "Cor ou Raça",
                              "Nacionalidade",
                              "Grau de Instrução",
                              "Estado de Nascimento","Município de Nascimento")
     
     df_ID_votos$`Sigla Atual do Partido` <- recode(df_ID_votos$`Sigla do Partido`,
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
     
     
    #Adicionar os nomes dos municípios na Sigla da UE
        
    df_ID_votos$'Código do Município (TSE)'<-as.numeric(df_ID_votos$'Sigla da Unidade Eleitoral')
     
    df_ID_votos[!is.na(df_ID_votos$'Código do Município (TSE)'),]$'Sigla da Unidade Eleitoral'<-df_ID_votos[!is.na(df_ID_votos$'Código do Município (TSE)'),]$'Unidade Eleitoral'
    
    
    #Salvar:
    
    temp<-df_ID_votos[is.na(df_ID_votos$id_cepesp),] #Base sem id (candidatos sem CPF e Título)
    
    df_ID_votos<-df_ID_votos[!is.na(df_ID_votos$id_cepesp),]
    
    
     saveRDS(df_ID_votos,file = "df_carreiras.rds")
     
     saveRDS(temp,file = "df_sem_id.rds")
     

  # Dividir a base em arquivos menores para facilitar a leitura:

    df_carreiras <- readRDS("~/Projetos_R/cepesp_app_carreiras/df_carreiras.rds")
     
    setwd("~/Projetos_R/cepesp_app_carreiras/bases")
    
    teste<-list.files("~/Projetos_R/cepesp_app_carreiras/bases",pattern = ".rds")
    
      teste<-as.numeric(gsub("[A-Z]|[a-z]|[[:punct:]]","",teste))
    
    id_cepesp<-unique(df_carreiras$id_cepesp)
    
      faltantes<-id_cepesp[!id_cepesp%in%teste]

    for (i in 1:length(faltantes)) {
      saveRDS(df_carreiras[df_carreiras$id_cepesp==faltantes[i],],paste("id_",faltantes[i],".rds",sep = ""))
      }

     
     
     
     
     
     
     
     
