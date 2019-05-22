library(tidyverse)

df <- readRDS("~/Documents/cepesp/app_carreiras/df.rds") # importando df.

ultima_eleicao <- df %>%
  group_by(`Número do Título Eleitoral`) %>%
  mutate(ultima_eleicao = if_else(`Ano da Eleição` == max(`Ano da Eleição`),1,0)) %>%
  ungroup() %>%
  filter(`Nº do Turno` == 1) %>%
  filter(ultima_eleicao==1) %>%
  select(`Número do Título Eleitoral`,`Sigla da Unidade Eleitoral`,`Sigla do Partido`,`Sigla Atual do Partido`)

colnames(ultima_eleicao) <- c('Número do Título Eleitoral', 'Última UE','Último Partido','Sigla Atual do Último Partido')

df <- left_join(x = df, y = ultima_eleicao, by = 'Número do Título Eleitoral')
rm(ultima_eleicao)

saveRDS(df, "df.rds")
write_delim(df, path = 'df_UTC8.csv', delim = ';')
