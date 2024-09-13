



# Define UI for application that draws a histogram
  ui <- fluidPage(
    # Adiciona o CSS customizado diretamente na UI
    tags$head(
      tags$style(HTML("
            .leaflet-control-layers {
                font-size: 10px; /* Ajuste o tamanho da fonte */
                width: 200px; /* Ajuste a largura do controle */
            }
        "))
    ),

    # Application title
    titlePanel("Geolocalização dos hospitais em Santa Catarina"),
       
     fluidPage(
      
        mainPanel(
         
             # mapa icon colorido
             h4("Mapa 1: Distribuição espacial dos hospitais por núm. de partos e região de saúde."),
             width= 12, leafletOutput("mapa_icon2", height = 600),
             
        
            #  mapa icone - cor unica filtro de hospital no leaflet
             # h3("Mapa 2: geral."),
             # leafletOutput("mapa_icon_filtro", height = 600),
             
             # mapa geral com radius 
             #h3("Mapa 3: geral."),
             #leafletOutput("mapa_geral", height = 600),
            
            h4("Mapa 2: Análise de cluster por Região de Saúde."),
             leafletOutput("mapa", height = 600),
            hr(),
            h4("Mapa 3: Análise de cluster por Macroregião de Saúde."),
           leafletOutput("mapa2", height = 600))
        
            #downloadButton("download_mapa", "Baixar Mapa")
            
            
        )
            
        )
    

