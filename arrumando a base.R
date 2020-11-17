# carregar os pacotes ----
library(tidyverse)
library(geobr)
#genero
devtools::install_github("meirelesff/genderBR")
library(genderBR)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#subir as bases ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cand_2016 <- read.csv2('consulta_cand_2016_BRASIL.csv')

cand_2020 <- read.csv2('consulta_cand_2020_BRASIL.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# selecionar as colunas de interesse ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cand_2016_filter <- cand_2016 %>%
  filter(DS_SITUACAO_CANDIDATURA == 'APTO') %>%
  select(ANO_ELEICAO,SG_UF, NM_UE,NM_MUNICIPIO_NASCIMENTO, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO,
         NM_SOCIAL_CANDIDATO, NR_CPF_CANDIDATO,SG_PARTIDO, SG_UF_NASCIMENTO, NR_IDADE_DATA_POSSE,
         DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, ST_REELEICAO,
         DS_SIT_TOT_TURNO, DS_OCUPACAO,VR_DESPESA_MAX_CAMPANHA)


cand_2020_filter <- cand_2020 %>%
  filter(DS_SITUACAO_CANDIDATURA == 'APTO') %>%
  select(ANO_ELEICAO,SG_UF, NM_UE,NM_MUNICIPIO_NASCIMENTO, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO,
         NM_SOCIAL_CANDIDATO,NR_CPF_CANDIDATO ,SG_PARTIDO, SG_UF_NASCIMENTO, NR_IDADE_DATA_POSSE,
         DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, ST_REELEICAO,
         DS_SIT_TOT_TURNO, DS_OCUPACAO, VR_DESPESA_MAX_CAMPANHA)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#juntar as bases ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cand_junto <- rbind(cand_2016_filter, cand_2020_filter)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# arrumando para análise -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cand_junto <- cand_junto %>%
  mutate(NM_URNA_CANDIDATO = as.character(NM_URNA_CANDIDATO),
         NM_CANDIDATO = as.character(NM_CANDIDATO),
         NM_SOCIAL_CANDIDATO = as.character(NM_SOCIAL_CANDIDATO),
         NM_SUF = NM_URNA_CANDIDATO,
         NM_POL = str_detect(NM_URNA_CANDIDATO, "DELEGADO|BOMBEIRO|
                             POLICIAL|MILITAR|CAPITAO|CAPITÃO|MAJOR|
                             CORONEL|DELEGADA|CAPITA|CAPITÃ")) %>%
  separate(NM_SUF, into = c("nome_urna", "sufixo_urna"), sep ="DO |DA ") %>%
  select(-nome_urna)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ARRUMAR DADOS STR ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cand_junto <- cand_junto %>%
  mutate (sufixo_urna = str_replace(sufixo_urna, "FARMACIA", "FARMÁCIA"),
          sufixo_urna = str_replace(sufixo_urna, "SAUDE", "SAÚDE"),
          sufixo_urna = str_replace(sufixo_urna, "ONIBUS", "ÔNIBUS"),
          sufixo_urna = str_replace(sufixo_urna, "RADIO", "RÁDIO"),
          sufixo_urna = str_replace(sufixo_urna, "TÁXI", "TAXI"))



write_rds(cand_junto, "cand_juntoBR.rds")

library(writexl)

write_xlsx(cand_junto, "cand_juntoBR.xlsx")
