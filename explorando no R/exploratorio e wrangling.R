# carregar os pacotes
library(tidyverse)
library(geobr)

#subir as bases ----
cand_2016 <- read.csv2('consulta_cand_2016_BRASIL.csv')

states <- read_state(year = 2018)

mun <- read_municipality(year = 2018)

cand_2020 <- read.csv2('consulta_cand_2020_BRASIL.csv')

# selecionar as colunas de interesse ----

cand_2016_filter <- cand_2016 %>%
  filter(DS_SITUACAO_CANDIDATURA == 'APTO') %>%
  select(ANO_ELEICAO,SG_UF, NM_UE, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO,
         NM_SOCIAL_CANDIDATO, SG_PARTIDO, SG_UF_NASCIMENTO, NR_IDADE_DATA_POSSE,
         DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, ST_REELEICAO,
         DS_SIT_TOT_TURNO)


cand_2020_filter <- cand_2020 %>%
  filter(DS_SITUACAO_CANDIDATURA == 'APTO') %>%
  select(ANO_ELEICAO,SG_UF, NM_UE, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO,
         NM_SOCIAL_CANDIDATO, SG_PARTIDO, SG_UF_NASCIMENTO, NR_IDADE_DATA_POSSE,
         DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, ST_REELEICAO,
         DS_SIT_TOT_TURNO)


#juntar as bases ----

cand_junto <- rbind(cand_2016_filter, cand_2020_filter)

# salvando as bases

write_csv2(cand_junto, 'candidatos_BR1620.csv')

write_rds(cand_junto, 'cand_BR1620.rds')
write_rds(cand_2016_filter, 'cand_BR16.rds')
write_rds(cand_2020_filter, 'cand_BR20.rds')

# analise descritiva 2020 ----

### nome dos candidatos
library(stringr)
library(dplyr)
library(tidyr)

cand_BR20 <- cand_BR20 %>%
  mutate(NM_URNA_CANDIDATO = as.character(NM_URNA_CANDIDATO),
         NM_CANDIDATO = as.character(NM_CANDIDATO),
         NM_SOCIAL_CANDIDATO = as.character(NM_SOCIAL_CANDIDATO))


cand_2020_filter_names <- cand_BR20 %>%
  separate(NM_URNA_CANDIDATO, into = c("nome urna", "sufixo urna"), sep ="DO |DA ")


#genero
devtools::install_github("meirelesff/genderBR")
library(genderBR)

cand_2020_filter_names$generosocial <- get_gender(cand_2020_filter_names$NM_SOCIAL_CANDIDATO)
cand_2020_filter_names$generonomeoficial <- get_gender(cand_2020_filter_names$NM_CANDIDATO)


#salvando a base final de analise
write_rds(cand_2020_filter_names, "candidatos_tratrada2020.rds")

cand_1620_tratadas <- rbind(cand_2016_filter_names, cand_2020_filter_names)

write_rds(cand_1620_tratadas, "cand_1620BR_tratadas.rds")


# ANALISES -----------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analisando 2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# candidatos trans? ----

#### geral
cand_2020_filter_names %>%
  filter(generosocial != is.na(generosocial)) %>%
  mutate(trans = case_when(generosocial != generonomeoficial ~ "Trans",
                           TRUE ~ "Cis ou não identificado")) %>%
  ggplot(aes(DS_CARGO, fill = trans)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Candidatos com nome social")

#### por partido
cand_2020_filter_names %>%
  filter(generosocial != is.na(generosocial)) %>%
  mutate(trans = case_when(generosocial != generonomeoficial ~ "Trans",
                           TRUE ~ "Cis ou não identificado")) %>%
  filter(trans == "Trans")%>%
  ggplot(aes(SG_PARTIDO)) +
  geom_bar() +
  coord_flip()

#### por estado
cand_2020_filter_names %>%
  filter(generosocial != is.na(generosocial)) %>%
  mutate(trans = case_when(generosocial != generonomeoficial ~ "Trans",
                           TRUE ~ "Cis ou não identificado")) %>%
  filter(trans == "Trans")%>%
  ggplot(aes(SG_UF)) +
  geom_bar() +
  coord_flip()


#### nome dos candidatos ----

library(stringr)
cand_2020_filter_names2 <- cand_2020_filter_names %>%
  mutate (sufix = str_replace(`sufixo urna`, "FARMACIA", "FARMÁCIA")) %>%
  mutate(sufix = str_replace(sufix, "SAUDE", "SAÚDE"))

cand_2020_filter_names2 %>%
  filter(DS_CARGO == "VEREADOR") %>%
  filter(sufix != is.na(sufix),
         sufix != "",
         sufix != "SILVA")%>%
  count(sufix, sort = TRUE) %>%
  filter(n > 300) %>%
  ggplot(aes(sufix, n)) +
  geom_col() +
  coord_flip()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analise 2020 e 2016 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# aumento de candidaturas jovens ----
cand_1620BR_tratadas %>%
  filter(NR_IDADE_DATA_POSSE > 0,
         NR_IDADE_DATA_POSSE < 100) %>%
  ggplot(aes(NR_IDADE_DATA_POSSE, as_factor(ANO_ELEICAO))) +
  geom_boxplot()


cand_1620BR_tratadas %>%
  filter(NR_IDADE_DATA_POSSE > 0,
         NR_IDADE_DATA_POSSE < 100) %>%
  mutate(idade = case_when(NR_IDADE_DATA_POSSE < 30 ~ "Jovem",
                           NR_IDADE_DATA_POSSE >= 60 ~ "Idoso",
                           TRUE ~ "Adulto")) %>%
  filter(idade == "Jovem") %>%
  ggplot(aes(as_factor(ANO_ELEICAO))) +
  geom_bar()


# aumento de candidaturas trans ----
#SOMENTE DADOS DE 2020, EM 2016 ISSO NÃO ERA MEDIDO?
#cand_1620BR_tratadas %>%
#  filter(generosocial != is.na(generosocial)) %>%
#  mutate(trans = case_when(generosocial != generonomeoficial ~ "Trans",
TRUE ~ "Cis ou não identificado")) %>%
  # ggplot(aes(as_factor(ANO_ELEICAO), fill = trans)) +
  #geom_bar() +
  #coord_flip() +
  #labs(title = "Candidatos com nome social")
  
  
  
  # aumento de candidaturas de negros ----

cand_1620BR_tratadas %>%
  mutate(cor = case_when(DS_COR_RACA == "BRANCA" ~ "BRANCA",
                         TRUE ~  "NÃO BRANCA")) %>%
  ggplot(aes(as_factor(ANO_ELEICAO), fill = cor)) +
  geom_bar(position = "fill") +
  coord_flip()


# aumento de candidaturas de mulheres ----

cand_1620BR_tratadas %>%
  filter(DS_GENERO == "FEMININO") %>%
  ggplot(aes(as_factor(ANO_ELEICAO))) +
  geom_bar()+
  labs(y="",x = "", title = "Candidatas")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2016
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ELEITOS
# DISTRIBUIÇÃO COR E GENERO NO BRASIL ----

cand_BR16 %>%
  filter(DS_SIT_TOT_TURNO != "NÃO ELEITO",
         DS_GENERO != "NÃO DIVULGÁVEL") %>%
  ggplot(aes(DS_CARGO, (..count..)/sum(..count..), fill = DS_GENERO)) +
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent) +
  labs(y="",x = "", title = "Distribuição de gênero - Eleitos")


cand_BR16 %>%
  filter(DS_SIT_TOT_TURNO != "NÃO ELEITO") %>%
  mutate(cor = case_when(DS_COR_RACA == "BRANCA" ~ "BRANCA",
                         TRUE ~  "NÃO BRANCA")) %>%
  ggplot(aes(DS_CARGO, fill = cor)) +
  geom_bar(position = "fill") +
  coord_flip()


cand_BR16 %>%
  filter(DS_SIT_TOT_TURNO != "NÃO ELEITO") %>%
  filter(NR_IDADE_DATA_POSSE > 0,
         NR_IDADE_DATA_POSSE < 100) %>%
  ggplot(aes(NR_IDADE_DATA_POSSE, fill = DS_GENERO)) +
  geom_boxplot()

cand_BR16 %>%
  filter(DS_SIT_TOT_TURNO != "NÃO ELEITO") %>%
  filter(NR_IDADE_DATA_POSSE > 0,
         NR_IDADE_DATA_POSSE < 100) %>%
  ggplot(aes(NR_IDADE_DATA_POSSE, fill = DS_COR_RACA)) +
  geom_boxplot()

#efeito reeleição ----

cand_BR16 %>%
  filter(DS_SIT_TOT_TURNO != "NÃO ELEITO") %>%
  ggplot(aes(DS_SIT_TOT_TURNO, fill = ST_REELEICAO)) +
  geom_bar() +
  coord_flip()

cand_BR16 %>%
  filter(DS_SIT_TOT_TURNO != "NÃO ELEITO") %>%
  ggplot(aes(DS_CARGO, fill = ST_REELEICAO)) +
  geom_bar(position = 'fill') +
  coord_flip()



