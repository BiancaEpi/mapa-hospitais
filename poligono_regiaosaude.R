library(sf)
library(geobr)
library(stringi)
library(stringr)




# Suponha que 'municipios_sc' tenha uma coluna 'regiao_saude' que indica a região de saúde
# e 'geometry' que é a geometria do município.

sc <- read_municipality(code_muni = 'SC', year=2018)
sc <- sc %>%mutate(name_muni = toupper(name_muni))
sc$name_muni <- sc$name_muni %>% stri_trans_general("Latin-ASCII") #acentos
sc <- left_join(sc, regiao, by = c("name_muni" = "nom_municipio")) #Join


# Agregar municípios por região de saúde
   sc_regioes <- sc %>%
   group_by(dsc_regiao_saude) %>%
   summarize(geometry = st_union(geom))
   

   leaflet(sc_regioes) %>%
     addTiles() %>% addPolygons()
   
   
   
   # Adicionar os limites das regiões de saúde
   map4 <- map4 %>%
     addPolygons(
       data = regioes_saude, # Adiciona as regiões de saúde
       fillColor = "transparent", # Ou a cor desejada para preenchimento
       color = "black", # Cor das bordas
       weight = 2, # Espessura da linha
       opacity = 1, # Opacidade das bordas
       label = ~regiao_saude # Ajuste o rótulo conforme o nome das regiões (verifique o nome da coluna no seu shapefile)
     )
   