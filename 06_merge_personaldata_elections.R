## APP CARREIRAS - Cepespdata / FGV
## Merge entre o banco de dados pessoais e o banco de eleições nacionais

# Carregando bancos -----
load("elections.RData")
load("personaldata.RData")

# Tirando títulos NAs do banco de eleições----
elections %>%
  filter(is.na(as.numeric(elections$NUM_TITULO_ELEITORAL_CANDIDATO))==F) -> elections 
# perdemos 3446 obs. retirando os NAs no título

# Merge dos bancos, mantendo todas as observações do banco de eleições
df_carreiras <- merge(x = elections, y = personaldata, by = "NUM_TITULO_ELEITORAL_CANDIDATO", 
                      all.x = T, all.y = F) 

# Perdas do merge ----
perdasmerge <- df_carreiras[is.na(df_carreiras$CPF_CANDIDATO)==T,] # 0 perdas. Usei o CPF porque ele vem do df personaldata.
rm(perdasmerge)

# Salvando o banco ----
save(df_carreiras, file = "df_carreiras.RData")

rm(elections,personaldata) # removendo bancos
