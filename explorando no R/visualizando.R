#DADOS DO SCRIPT "respondendo perguntas.R"

###### COR ######

#QUANTOS ALTERARAM A COR DE 2016 --> 2020 -----
sankey <- cand_cor %>%
  count(DS_COR_RACA.y,DS_COR_RACA.x, sort = TRUE) %>%
  mutate(perc = n/sum(n)*100)

library(flipPlots)

SankeyDiagram(sankey[,-3:-4],
              link.color = "Source",
              label.show.varname = FALSE,
              weights = sankey$n)


# Onde estao as candidaturas de negros na prefeitura? ----
library(geobr)
library(leaflet)

mun <- read_municipality(year = 2018)
mun <- mun %>%
  mutate(name_muni = str_to_upper(name_muni))

mun_negros <- left_join(mun, prefpretopardo, by = c("name_muni" = "NM_UE"))
mun_negros <- mun_negros %>% filter(n != is.na(n))

write_rds(mun_negros, "mapa_pref_negros.rds")

mapa = leaflet(mun_negros) %>% addTiles()

pal <- colorNumeric(
  palette = "Blues",
  domain = mun_negros$n)

mapa %>% addPolygons(stroke = FALSE,
                     smoothFactor = 0.2, fillOpacity = 1,
                     color = ~pal(n),
                     label = ~paste(mun_negros$name_muni, mun_negros$n),
                     labelOptions = labelOptions(direction = "auto"))

# Onde estao as candidaturas de indigenas para prefeitura? ----
library(geobr)
library(leaflet)

mun <- read_municipality(year = 2018)
mun <- mun %>%
  mutate(name_muni = str_to_upper(name_muni))

mun_indigena <- left_join(mun, prefindigena, by = c("name_muni" = "NM_UE"))
mun_indigena <- mun_indigena %>% filter(n != is.na(n))

mapa = leaflet(mun_indigena) %>% addTiles()

pal <- colorNumeric(
  palette = "Blues",
  domain = mun_indigena$n)

mapa %>% addPolygons(stroke = FALSE,
                     smoothFactor = 0.2, fillOpacity = 1)

write_rds(mun_indigena, "mapa_pref_indigena.rds")


# candiaturas indigenas a vereador -----
mun_indigena_lesg <- left_join(mun, lesgindigena, by = c("name_muni" = "NM_UE"))
mun_indigena_lesg <- mun_indigena_lesg %>% filter(n != is.na(n))

write_rds(mun_indigena_lesg, "mapa_lesg_indigena.rds")

mapa = leaflet(mun_indigena_lesg) %>% addTiles()

pal <- colorNumeric(
  palette = "Blues",
  domain = mun_indigena_lesg$n)

mapa %>% addPolygons(stroke = FALSE,
                     smoothFactor = 0.2, fillOpacity = 1,
                     color = ~pal(n),
                     label = ~paste(mun_indigena_lesg$name_muni, mun_indigena_lesg$n),
                     labelOptions = labelOptions(direction = "auto"))



###### GENERO ######

# Onde estao as candidaturas de mulheres? -----
library(geobr)
library(leaflet)

mun <- read_municipality(year = 2018)
mun <- mun %>%
  mutate(name_muni = str_to_upper(name_muni))

mun_genero <- left_join(mun, prefmulhercand, by = c("name_muni" = "NM_UE"))
mun_genero <- mun_genero %>% filter(n != is.na(n))

write_rds(mun_genero, "mapa_pref_mulheres.rds")

mapa = leaflet(mun_genero) %>% addTiles()


mapa %>% addPolygons(stroke = FALSE,
                     smoothFactor = 0.2, fillOpacity = 1)



###### IDADE ######