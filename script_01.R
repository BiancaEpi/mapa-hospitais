
# pacotes
library(rio)
library(here)
library(shiny)
library(readxl)
library(dplyr)
library(rio)
library(leaflet)
library(jsonlite)
library(rjson)
library(RCurl)
library(stringi)

# import

mapa_hospitais  <- read_excel("mapa_hospitais.xlsx")
mapa_hospitais$municipio <- mapa_hospitais$municipio %>% stri_trans_general("Latin-ASCII") #acentos
regiao  <- import("regiao.csv")
mapa_hospitais <- left_join(mapa_hospitais, regiao, by = c("municipio" = "nom_municipio")) #Join


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
                        

# opcao numérica                              
mapa_hospitais <- mapa_hospitais %>% 
  mutate(partos_num = case_when(
    NV <= 107 ~  1,
    NV > 107 & NV <= 424 ~  2,
    NV > 424 & NV <= 1288  ~  3,
    NV > 1288 ~  4 ))


                             

# mapa de cluster por regiao ------------------------------------------------------------


# Mapa base de santa catarina

map <- leaflet() %>% 
  setView(lng = -51, lat = -27.50, zoom = 7) %>% # Coordenadas do mapa de SC
  addTiles() %>% 
  addProviderTiles(providers$CartoDB)

# Criar o popup com várias informações

mapa_hospitais$popup_info <- sprintf(
  "<strong>Estabelecimento:</strong> %s<br/>
  <strong>CEP:</strong> %s<br/>
  <strong>Município:</strong> %s<br/>
  <strong>Número de Partos:</strong> %s
  <strong>Região de Saúde:</strong> %s",
  mapa_hospitais$estabelecimento,
  mapa_hospitais$cep,
  mapa_hospitais$municipio,
  mapa_hospitais$NV,
  mapa_hospitais$dsc_regiao_saude)




# criando cores diferentes conforme a estratificação 

# criar os vetores da funcao
cor = c()
nrow(mapa_hospitais)

# criar uma funcao do tipo for para as cores

for (i in 1 : nrow (mapa_hospitais)){
  
  if(mapa_hospitais$partos_num[i] == "1"){
    cor[i] = "blue"} 
  
  else if (mapa_hospitais$partos_num[i] == "2"){
    cor[i] = "orange"}  
  
  else if (mapa_hospitais$partos_num[i] == "3"){
    cor[i] = "red"}  
  
  else if (mapa_hospitais$partos_num[i] == "4"){
    cor[i] = "purple"}  
  
}


cor # verificar a distribuicao de cores



# criando os icones 
icone = awesomeIcons(icon = "home", 
                     iconColor = "white", 
                     markerColor = cor) #marcadores da livraria




# Adicionar marcadores com agrupamento por região de saúde

unique_regions <- unique(mapa_hospitais$dsc_regiao_saude)

for (region in unique_regions) {
  region_data <- mapa_hospitais[mapa_hospitais$dsc_regiao_saude == region, ]
  
  map <- map %>% addAwesomeMarkers(
    lng = region_data$longitude_adjusted,
    lat = region_data$latitude_adjusted,
    #color = ~cores(partos), 
    icon = icone,
    popup = region_data$popup_info, # aparece no clique 
    # label = region_data$CNES,
    group = region,
    clusterOptions = markerClusterOptions()
  )
}


# Adicionar controle de camadas para as regiões
map <- map %>% addLayersControl(
  overlayGroups = unique_regions,
  options = layersControlOptions(collapsed = FALSE)
)

# Mostrar o mapa
map


###################### --  #################


# mapa sem cluster --------------------------------------------------------

# Mapa base de santa catarina

map2 <- leaflet() %>% 
  setView(lng = -51, lat = -27.50, zoom = 7) %>% # Coordenadas do mapa de SC
  addTiles() %>% 
  addProviderTiles(providers$CartoDB)



cores <- colorFactor(palette = c("orange", "red", "blue",  "purple"), domain = mapa_hospitais$partos)


# Adicionar marcadores coloridos no mapa
map2 <- map2 %>%
  addCircleMarkers(data = mapa_hospitais, 
                   lng = ~longitude_adjusted, 
                   lat = ~latitude_adjusted, 
                   color = ~cores(partos), 
                   popup = region_data$popup_info # aparece no clique 
                   #radius = ifelse(mapa_hospitais$NV == 1,10,6)
  )

# Exibir mapa
map2


########################### --- ###########################




# mapa3 - por raios conforme número de partos  ------------------------------------------------------------------

# Mapa base de Santa Catarina
map3 <- leaflet() %>% 
  setView(lng = -51, lat = -27.50, zoom = 7) %>% # Coordenadas do mapa de SC
  addTiles() %>% 
  addProviderTiles(providers$CartoDB)

# Criar o popup com várias informações
mapa_hospitais$popup_info <- sprintf(
  "<strong>Estabelecimento:</strong> %s<br/>
  <strong>CEP:</strong> %s<br/>
  <strong>Município:</strong> %s<br/>
  <strong>Número de Partos:</strong> %s",
  mapa_hospitais$estabelecimento,
  mapa_hospitais$cep,
  mapa_hospitais$municipio,
  mapa_hospitais$partos
)

# Definir cores com base no número de partos
cores <- colorFactor(palette = c("orange", "red", "blue", "purple"), domain = mapa_hospitais$partos)


# Ajustar o fator de escala para o tamanho dos marcadores
fator_escala <- 0.2 # Você pode ajustar este valor conforme necessário

# Calcular o tamanho dos marcadores com base no número de partos
mapa_hospitais$radius <- sqrt(mapa_hospitais$NV) * fator_escala


# Adicionar marcadores coloridos no mapa
map3 <- map3 %>%
  addCircleMarkers(data = mapa_hospitais, 
                   lng = ~longitude_adjusted, 
                   lat = ~latitude_adjusted, 
                   color = ~cores(partos), 
                   popup = ~popup_info, # aparece no clique 
                   radius = ~radius
  )

map3 #print




# mapa 4 cluster + radios


# Mapa base de santa catarina

map <- leaflet() %>% 
  setView(lng = -51, lat = -27.50, zoom = 7) %>% # Coordenadas do mapa de SC
  addTiles() %>% 
  addProviderTiles(providers$CartoDB)

# Criar o popup com várias informações

mapa_hospitais$popup_info <- sprintf(
  "<strong>Estabelecimento:</strong> %s<br/>
  <strong>CEP:</strong> %s<br/>
  <strong>Município:</strong> %s<br/>
  <strong>Número de Partos:</strong> %s
  <strong>Região de Saúde:</strong> %s",
  mapa_hospitais$estabelecimento,
  mapa_hospitais$cep,
  mapa_hospitais$municipio,
  mapa_hospitais$NV,
  mapa_hospitais$dsc_regiao_saude)





# Adicionar marcadores com agrupamento por região de saúde

unique_regions <- unique(mapa_hospitais$dsc_regiao_saude)

for (region in unique_regions) {
  region_data <- mapa_hospitais[mapa_hospitais$dsc_regiao_saude == region, ]
  
  
  
  
  # Definir cores com base no número de partos
  cores <- colorFactor(palette = c("orange", "red", "blue", "purple"), domain = mapa_hospitais$partos)
  
  
  # Ajustar o fator de escala para o tamanho dos marcadores
  fator_escala <- 0.2 # Você pode ajustar este valor conforme necessário
  
  # Calcular o tamanho dos marcadores com base no número de partos
  mapa_hospitais$radius <- sqrt(mapa_hospitais$NV) * fator_escala
  
  
  # Adicionar marcadores coloridos no mapa
  map4 <- map4 %>%
    addCircleMarkers(data = mapa_hospitais, 
                     lng = ~longitude_adjusted, 
                     lat = ~latitude_adjusted, 
                     color = ~cores(partos), 
                     popup = ~popup_info, # aparece no clique 
                     radius = ~radius,
                     group = region,
                     clusterOptions = markerClusterOptions()
    )
  
  
}


# Adicionar controle de camadas para as regiões
map4 <- map4 %>% addLayersControl(
  overlayGroups = unique_regions,
  options = layersControlOptions(collapsed = FALSE)
)

# Mostrar o mapa
map4


###################### --  #################


# Definir cores com base no número de partos
cores <- colorFactor(palette = c("orange", "red", "blue", "purple"), domain = mapa_hospitais$partos)

# Ajustar o fator de escala para o tamanho dos marcadores
fator_escala <- 0.6 # Você pode ajustar este valor conforme necessário

# Calcular o tamanho dos marcadores com base no número de partos
mapa_hospitais <- mapa_hospitais %>%
  mutate(radius = sqrt(NV) * fator_escala)

# Criar o popup com várias informações
mapa_hospitais <- mapa_hospitais %>%
  mutate(popup_info = sprintf(
    "<strong>Estabelecimento:</strong> %s<br/>
    <strong>CEP:</strong> %s<br/>
    <strong>Município:</strong> %s<br/>
    <strong>Número de Partos:</strong> %s<br/>
    <strong>Região de Saúde:</strong> %s",
    estabelecimento, cep, municipio, NV, dsc_regiao_saude))

# Mapa base de Santa Catarina
map4 <- leaflet() %>% 
  setView(lng = -51, lat = -27.50, zoom = 7) %>% # Coordenadas do mapa de SC
  addTiles() %>% 
  addProviderTiles(providers$CartoDB)

# Adicionar marcadores com agrupamento por região de saúde
unique_regions <- unique(mapa_hospitais$dsc_regiao_saude)

for (region in unique_regions) {
  region_data <- mapa_hospitais %>%
    filter(dsc_regiao_saude == region)
  
  map4 <- map4 %>%
    addCircleMarkers(data = region_data, 
                     lng = ~longitude_adjusted, 
                     lat = ~latitude_adjusted, 
                     color = ~cores(partos), 
                     popup = ~popup_info, # aparece no clique 
                     radius = ~radius,
                     group = region,
                     clusterOptions = markerClusterOptions()
    )
}

# Adicionar controle de camadas para as regiões
map4 <- map4 %>% addLayersControl(
  overlayGroups = unique_regions,
  options = layersControlOptions(collapsed = FALSE)
)

# Mostrar o mapa
map4



