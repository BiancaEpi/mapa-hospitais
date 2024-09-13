# pacotes
library(rio)
library(here)
library(shiny)
library(readxl)
library(dplyr)
library(rio)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(geobr)
library(stringi)
library(stringr)
library(jsonlite)
library(rjson)
library(RCurl)
library(stringi)
library(readr)
library(readxl)


# # import

#mapa_hospitais <- read_excel("mapa_hospitais.xlsx")
mapa_hospitais <- read_excel("mapa_hospitais2.xlsx")

mapa_hospitais$municipio <- mapa_hospitais$municipio %>% stri_trans_general("Latin-ASCII") #acentos

# macroregiao
macro <- read_excel("municipios e macros.xlsx")
macro$Municípios <- macro$Municípios %>% stri_trans_general("Latin-ASCII") #acentos
macro <- macro %>%mutate(Municípios = toupper(Municípios)) #Maiuscula

# regiao (17)
regiao <- read_delim("regionais.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
regiao$NM_MUNICIP <- regiao$NM_MUNICIP %>% stri_trans_general("Latin-ASCII") #acentos
macro_regiao <- left_join(regiao, macro, by = c("NM_MUNICIP" = "Municípios")) #Join
macro_regiao$NM_MUNICIP <- replace(macro_regiao$NM_MUNICIP, macro_regiao$NM_MUNICIP == "SAO MIGUEL d'OESTE", "SAO MIGUEL DO OESTE")


mapa_hospitais <- left_join(mapa_hospitais, macro_regiao, by = c("municipio" = "NM_MUNICIP")) #Join

#shapefile municipios
sc <- read_municipality(code_muni = 'SC', year=2018)
sc <- sc %>%mutate(name_muni = toupper(name_muni))
sc$name_muni <- sc$name_muni %>% stri_trans_general("Latin-ASCII") #acentos
macro_regiao$NM_MUNICIP <- replace(macro_regiao$NM_MUNICIP, macro_regiao$NM_MUNICIP == "HERVAL d'OESTE", "HERVAL D'OESTE")

sc <- left_join(sc, macro_regiao, by = c("name_muni" = "NM_MUNICIP")) #Join



# Agregar municípios por região de saúde
sc_regioes <- sc %>%
  group_by(reg_saude) %>%
  summarize(geometry = st_union(geom))



# Criação das novas colunas ajustadas de latitude e longitude 
mapa_hospitais <- mapa_hospitais %>%
  mutate(
    latitude_adjusted = lat / 100000,
    longitude_adjusted = lon/ 100000
  )


# criar variável de estratificacao 

mapa_hospitais <- mapa_hospitais %>% 
  mutate(partos = case_when(
    NV <= 107 ~  "Até 107 partos ao ano",
    NV > 107 & NV <= 424 ~  "de 108 a 424 partos ao ano",
    NV > 424 & NV <= 1288  ~  "de 425 a 1288 partos ao ano",
    NV > 1288 ~  "de 1289 a 5528 partos ao ano"))


mapa_hospitais$partos <- as.factor(mapa_hospitais$partos)


# opcao numérica                              
mapa_hospitais <- mapa_hospitais %>% 
  mutate(partos_num = case_when(
    NV <= 107 ~  1,
    NV > 107 & NV <= 424 ~  2,
    NV > 424 & NV <= 1288  ~  3,
    NV > 1288 ~  4 ))


