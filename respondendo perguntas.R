library(dplyr)
library(tidyr)
library(purrr)
library(janitor)
library(ggplot2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dados gerais 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GERAL: aumentou ou diminui o numero e candidaturas? ----
cand_junto %>% 
  tabyl(ANO_ELEICAO, DS_CARGO) # queda de canditaturas GERAL


## COR: aumentou ou diminui o numero e candidaturas? ----
cand_junto %>% 
  tabyl(ANO_ELEICAO, DS_COR_RACA) #mais candidaturas pretas, menos pardas,
#menos cand brancas, mais indigenas, menos amarelas

cand_junto %>% 
  filter(DS_CARGO == "VEREADOR") %>%
  tabyl(ANO_ELEICAO, DS_COR_RACA) #Aumentaram indigenas e preta

cand_junto %>% 
  filter(DS_CARGO == "PREFEITO") %>%
  tabyl(ANO_ELEICAO, DS_COR_RACA) #Aumentaram indigenas, preta


cand_junto %>%
  filter(DS_CARGO == "VEREADOR") %>%
  ggplot(aes(as.factor(ANO_ELEICAO), fill = DS_COR_RACA))+
  geom_bar(position = "fill") #Pardo aumentou relativo a 2020 geral, preta tbm


cand_junto %>%
  filter(DS_CARGO == "PREFEITO") %>%
  ggplot(aes(as.factor(ANO_ELEICAO), fill = DS_COR_RACA))+
  geom_bar(position = "fill") #Pardo aumentou relativo a 2020 geral, preta tbm
#Para pref tambem! aumento de pardos e pretos na concorrencia do executivo


## GENERO: aumentou ou diminui o numero e candidaturas? ----
cand_junto %>% 
  tabyl(ANO_ELEICAO, DS_GENERO) # menos mulheres e menos homens


cand_junto %>%
  ggplot(aes(as.factor(ANO_ELEICAO), fill = DS_GENERO))+
  geom_bar(position = "fill") #aumento proporcional de mulheres, ainda que baixo

cand_junto %>%
filter(DS_CARGO == "VEREADOR") %>%
  ggplot(aes(as.factor(ANO_ELEICAO), fill = DS_GENERO))+
  geom_bar(position = "fill") # aumentou o numero de mulheres para vereadoras


cand_junto %>%
  filter(DS_CARGO == "PREFEITO") %>%
  ggplot(aes(as.factor(ANO_ELEICAO), fill = DS_GENERO))+
  geom_bar(position = "fill")

## GRAU DE INSTRUÇÃO -----------
cand_junto %>%
  tabyl(DS_GRAU_INSTRUCAO, ANO_ELEICAO)

cand_junto %>%
  filter(DS_CARGO == "PREFEITO") %>%
  ggplot(aes(as.factor(ANO_ELEICAO), fill = DS_GRAU_INSTRUCAO))+
  geom_bar(position = "fill") #superior completo aumentou


cand_junto %>%
  filter(DS_CARGO == "VEREADOR") %>%
  ggplot(aes(as.factor(ANO_ELEICAO), fill = DS_GRAU_INSTRUCAO))+
  geom_bar(position = "fill") -> p #aumentou superior completo


# OCUPAÇÕES PRINCIPAIS ----
cand_junto %>%
  tabyl(DS_OCUPACAO, ANO_ELEICAO) #são muitas, não deu para entender

## OCUPAÇÕES QUE CRESCERAM ----
cand_junto %>%
  filter(DS_CARGO == "VEREADOR",
         DS_OCUPACAO != "OUTROS")) %>%
  count(DS_OCUPACAO, ANO_ELEICAO, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(as.factor(ANO_ELEICAO), n, fill = as.factor(DS_OCUPACAO))) +
  geom_bar(stat = "identity", position = "fill") -> p #esse aqui traz infos!!
  
plotly::ggplotly(p)


  cand_junto %>%
    filter(DS_CARGO == "PREFEITO",
           DS_OCUPACAO != "OUTROS") %>%
  count(DS_OCUPACAO, ANO_ELEICAO, sort = TRUE) %>%
    top_n(20) %>%
    ggplot(aes(as.factor(ANO_ELEICAO), n, fill = DS_OCUPACAO)) +
    geom_bar(stat = "identity", position = "fill") -> p #esse aqui traz infos!!

## IDADE 2016 X 2020 ----
cand_junto %>%
  filter(NR_IDADE_DATA_POSSE > 0,
         NR_IDADE_DATA_POSSE < 100) %>%
  mutate(idade_categoria = case_when(NR_IDADE_DATA_POSSE < 30 ~ "Jovem",
                                     NR_IDADE_DATA_POSSE >= 60 ~ "Idoso",
                                     TRUE ~ "Adulto")) %>%
  count(idade_categoria, ANO_ELEICAO) %>%
  ggplot(aes(as.factor(ANO_ELEICAO), n, colour = idade_categoria, group = idade_categoria)) +
  geom_line() +
  geom_point()


cand_junto %>%
  filter(NR_IDADE_DATA_POSSE > 0,
         NR_IDADE_DATA_POSSE < 100) %>%
  mutate(idade_categoria = case_when(NR_IDADE_DATA_POSSE < 30 ~ "Jovem",
                                     NR_IDADE_DATA_POSSE >= 60 ~ "Idoso",
                                     TRUE ~ "Adulto")) %>%
  tabyl(idade_categoria, ANO_ELEICAO)


cand_junto %>%
  filter(NR_IDADE_DATA_POSSE > 0,
         NR_IDADE_DATA_POSSE < 100,
         DS_CARGO == "VEREADOR") %>%
  mutate(idade_categoria = case_when(NR_IDADE_DATA_POSSE < 30 ~ "Jovem",
                                     NR_IDADE_DATA_POSSE >= 60 ~ "Idoso",
                                     TRUE ~ "Adulto")) %>%
  count(idade_categoria, ANO_ELEICAO) %>%
  ggplot(aes(as.factor(ANO_ELEICAO), n, fill = idade_categoria)) +
  geom_bar(stat = "identity", position = "fill") -> p

plotly::ggplotly(p) #queda no numero de jovens candidatos, pequeno aumento de idosos





## SUFIXO ----
cand_junto %>%
  filter(DS_CARGO == "VEREADOR") %>%
  filter(sufixo_urna != is.na(sufixo_urna),
         sufixo_urna != "",
         sufixo_urna != "SILVA", 
         ANO_ELEICAO == 2020)%>%
  count(sufixo_urna, sort = TRUE) %>%
  filter(n > 300) %>%
  mutate(sufixo_urna = fct_reorder(sufixo_urna, n)) %>%
  ggplot(aes(sufixo_urna, n)) +
  geom_col() +
  coord_flip()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PERGUNTAS ESPECÍFICAS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## MULHERES Quais municipios há candidaturas p/executivo? ----
prefmulhercand <- cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_GENERO == "FEMININO",
         DS_CARGO == "PREFEITO") %>%
  count(NM_UE, sort = TRUE)

## MULHERES Qual mais possui p/executivo? ----
cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_GENERO == "FEMININO",
         DS_CARGO == "PREFEITO") %>%
  ggplot(aes(SG_PARTIDO)) +
  geom_bar() +
  coord_flip()


## COR Quais municipios  há candidaturas p/executivo? ----
cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_COR_RACA == "PRETA",
         DS_CARGO == "PREFEITO") %>%
  count(NM_UE, sort = TRUE) %>%
  summarize(sum(n)) #643 pretos


cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_COR_RACA == "PARDA",
         DS_CARGO == "PREFEITO") %>%
  count(NM_UE, sort = TRUE) %>%
  summarize(sum(n)) #4722 pardos


prefpretopardo <- cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_COR_RACA == "PARDA" |
          DS_COR_RACA == "PRETA",
         DS_CARGO == "PREFEITO") %>%
  count(NM_UE, sort = TRUE)


lesgpretopardo <- cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_COR_RACA == "PARDA" |
           DS_COR_RACA == "PRETA",
         DS_CARGO == "VEREADOR") %>%
  count(NM_UE, sort = TRUE)


prefindigena <- cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_COR_RACA == "INDÍGENA",
         DS_CARGO == "PREFEITO") %>%
  count(NM_UE, sort = TRUE) %>%
  summarize(sum(n)) #33 indígenas a prefeitura


cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_COR_RACA == "INDÍGENA",
         DS_CARGO == "PREFEITO") %>%
  ggplot(aes(NM_UE)) +
  geom_bar() +
  coord_flip()


## COR Qual mais possui p/executivo? ----

cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_COR_RACA == "INDÍGENA",
         DS_CARGO == "PREFEITO") %>%
  ggplot(aes(SG_PARTIDO)) +
  geom_bar() +
  coord_flip()


## MULHERES Quais municipios não há candidaturas p/legislativo? ----
cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_CARGO == "VEREADOR") %>% 
  tabyl(NM_UE, DS_GENERO) %>%
  filter(FEMININO == 0)-> semmulherleg

## MULHERES Qual mais possui p/legislativo? ----

cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_GENERO == "FEMININO",
         DS_CARGO == "VEREADOR") %>%
  ggplot(aes(SG_PARTIDO)) +
  geom_bar() +
  coord_flip()

## COR Quais municipios não há candidaturas p/legislativo? ----
cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_CARGO == "VEREADOR") %>% 
  tabyl(NM_UE, DS_COR_RACA) %>%
  filter(PARDA == 0)


cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_CARGO == "VEREADOR") %>% 
  tabyl(NM_UE, DS_COR_RACA) %>%
  filter( PRETA == 0)

## indigenas no legislativo ----
lesgindigena <- cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_COR_RACA == "INDÍGENA",
         DS_CARGO == "VEREADOR") %>%
  count(NM_UE, sort = TRUE) 

## COR Qual mais possui p/legislativo n brancos? ----

cand_junto %>%
  filter(ANO_ELEICAO == 2020,
         DS_COR_RACA != "BRANCA",
         DS_COR_RACA != "SEM INFORMAÇÃO",
         DS_CARGO == "VEREADOR") %>%
  ggplot(aes(SG_PARTIDO)) +
  geom_bar() +
  coord_flip()


## INDIGENAS - Aumento? ----
cand_junto %>%
  filter(DS_COR_RACA == "INDÍGENA") %>%
  count(ANO_ELEICAO,sort = TRUE) %>%
  ggplot(aes(ANO_ELEICAO, n)) +
  geom_point() +
  geom_line() +
  ylim(0, 1800)


cand_junto %>%
  filter(DS_COR_RACA == "INDÍGENA") %>%
  count(ANO_ELEICAO,DS_CARGO,sort = TRUE) %>%
  ggplot(aes(ANO_ELEICAO, n, color = DS_CARGO)) +
  geom_point() +
  geom_line() +
  ylim(0, 1800)

# COR - candidatos mudaram de cor? ----
#pior soluçao possivel - spread e pivot_wider n funcionavam
write_rds(cand_cor, "cand_cor.rds")

cand_junto %>%
  select(NR_CPF_CANDIDATO, ANO_ELEICAO, DS_COR_RACA, DS_CARGO, NM_UE, SG_UF) %>%
  filter(ANO_ELEICAO == 2016) -> cor16

cand_junto %>%
  select(NR_CPF_CANDIDATO, ANO_ELEICAO, DS_COR_RACA, DS_CARGO, NM_UE, SG_UF) %>%
  filter(ANO_ELEICAO == 2020) -> cor20

cand_cor <- inner_join(cor20, cor16, by = "NR_CPF_CANDIDATO")

cand_cor <- cand_cor %>%
  mutate(mudou = case_when(DS_COR_RACA.x != DS_COR_RACA.y ~ "alterou",
                           TRUE ~ "não alterou")) %>%
  filter(mudou == "alterou",
         DS_COR_RACA.x != "BRANCA")


cand_cor %>%
  ggplot(aes(DS_COR_RACA.x)) +
  geom_bar() #vale fazer um sankey de mudou de qual cor para qual cor!


# MILITARES ----
cand_junto %>%
  filter(NM_POL == TRUE,
         ANO_ELEICAO == 2020,
         DS_CARGO == "VEREADOR") %>%
  ggplot(aes(SG_PARTIDO)) +
  geom_bar() +
  coord_flip()


cand_junto %>%
  filter(NM_POL == TRUE,
         DS_CARGO == "PREFEITO") %>%
  ggplot(aes(SG_PARTIDO)) +
  geom_bar() +
  coord_flip() +
  facet_grid( .~ ANO_ELEICAO)

cand_junto %>%
  filter(NM_POL == TRUE,
         DS_CARGO == "PREFEITO") %>% 
  count(ANO_ELEICAO,sort = TRUE) %>%
  mutate(perc = n/sum(n)) %>%
  ggplot(aes(ANO_ELEICAO, perc)) +
  geom_point() +
  geom_line() +
  ylim(0,1)


cand_junto %>%
  filter(NM_POL == TRUE,
         DS_CARGO == "VEREADOR") %>% 
  count(ANO_ELEICAO,sort = TRUE) %>%
  mutate(perc = n/sum(n)) %>%
  ggplot(aes(ANO_ELEICAO, perc)) +
  geom_point() +
  geom_line() +
  ylim(0,1)


cand_junto %>%
  filter(NM_POL == TRUE,
         DS_CARGO == "VEREADOR") %>%
  ggplot(aes(SG_PARTIDO)) +
  geom_bar() +
  coord_flip() +
  facet_grid( .~ ANO_ELEICAO)
