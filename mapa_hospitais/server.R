#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  

  mapa_hospitais <- mapa_hospitais %>%
    mutate(markerColor = case_when(
      partos == "Até 107 partos ao ano" ~ "green",
      partos == "de 108 a 424 partos ao ano" ~ "blue",
      partos == "de 425 a 1288 partos ao ano" ~ "orange",
      partos == "de 1289 a 5528 partos ao ano" ~ "red"
    ))

 
  
  output$mapa_icon2 <- renderLeaflet({

    # 1. Criar o popup com várias informações
    mapa_hospitais <- mapa_hospitais %>%
      mutate(popup_info = sprintf(
        "<strong>Estabelecimento:</strong> %s<br/>
             <strong>CEP:</strong> %s<br/>
             <strong>Município:</strong> %s<br/>
             <strong>Número de Partos:</strong> %s<br/>
             <strong>Estrato:</strong> %s<br/>
             
             <strong>Parto Normal:</strong> %s<br/>
             <strong>Parto Normal GAR:</strong> %s<br/>
              <strong>Cesárea:</strong> %s<br/>
             <strong>Cesárea GAR:</strong> %s<br/>
             <strong>Cesárea Laqueadura:</strong> %s<br/>

             
             <strong>Macroregião:</strong> %s<br/>
             <strong>Região de Saúde:</strong> %s",
        estabelecimento, cep, municipio, NV,  partos,
        parto_normal, parto_normal_GAR,cesarea, parto_cesaria_GAR, cesarea_laqueadura, 
        Macrorregião, reg_saude))
    
    # 2. Mapa base de Santa Catarina
    map4 <- leaflet() %>% 
      setView(lng = -51, lat = -27.50, zoom = 7) %>% # Coordenadas do mapa de SC
      addTiles() %>% 
       addProviderTiles(providers$OpenStreetMap)
    
    # Adicionar os limites das regiões de saúde
    map4 <- map4 %>%
      addPolygons(
        data = sc_regioes, # Adiciona as regiões de saúde
        fillColor = "white", # Ou a cor desejada para preenchimento
        fillOpacity = 0.2,
        color = "gray", # Cor das bordas
        weight = 1.5, # Espessura da linha
        opacity = 1, # Opacidade das bordas
        label = ~ reg_saude )
    
    # 3. Adicionar marcadores agrupados por região de saúde
    unique_regions <- unique(mapa_hospitais$reg_saude)
    
    for (region in unique_regions) {
      region_data <- mapa_hospitais %>%
        filter(reg_saude == region)
      
      
      map4 <- map4 %>%
        addAwesomeMarkers(
          data = region_data, 
          lng = ~longitude_adjusted, 
          lat = ~latitude_adjusted, 
          icon = ~makeAwesomeIcon(
            icon = 'home', 
            markerColor = markerColor, 
            iconColor = 'white', 
            library = 'glyphicon'
          ),
          popup = ~popup_info, # aparece no clique 
          group = region
        )
    }
    
   
    
    # 5. Adicionar controle de camadas para as regiões
    map4 <- map4 %>%
      addLayersControl(
        overlayGroups = unique_regions,
        options = layersControlOptions(collapsed = FALSE)
      )
    
    # 6. Adicionar a legenda para as cores
    map4 <- map4 %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "blue", "orange", "red"),
        labels = c("Até 107 partos ao ano", "de 108 a 424 partos ao ano", "de 425 a 1288 partos ao ano", "de 1289 a 5528 partos ao ano"),
        title = "Número de Partos"
      )
    
   
    
    # Mostrar o mapa
    map4
    
  })
# mapa icone - cor unica filtro de hospital no leaflet--------------------------------------------------

  
  
  output$mapa_icon_filtro <- renderLeaflet({
   
    # Definir ícones personalizados usando Leaflet.awesome-markers
    icone <- makeAwesomeIcon(
      icon = 'hospital',
      markerColor = 'blue',
      iconColor = 'white',
      library = 'fa'
    )
    
    # Criar o popup com várias informações
    mapa_hospitais <- mapa_hospitais %>%
      mutate(popup_info = sprintf(
        "<strong>Estabelecimento:</strong> %s<br/>
             <strong>CEP:</strong> %s<br/>
             <strong>Município:</strong> %s<br/>
             <strong>Número de Partos:</strong> %s<br/>
             <strong>Estrato:</strong> %s<br/>
             <strong>Macroregião:</strong> %s<br/>
             <strong>Região de Saúde:</strong> %s",
        estabelecimento, cep, municipio, NV, partos, dsc_macrorregiao_saude, dsc_regiao_saude))
    
    # Mapa base de Santa Catarina
    map4 <- leaflet() %>% 
      setView(lng = -51, lat = -27.50, zoom = 7) %>% # Coordenadas do mapa de SC
      addTiles() %>% 
      addProviderTiles(providers$CartoDB)
    
    # Adicionar marcadores com ícones por região de saúde
    unique_regions <- unique(mapa_hospitais$dsc_regiao_saude)
    
    for (region in unique_regions) {
      region_data <- mapa_hospitais %>%
        filter(dsc_regiao_saude == region)
      
      map4 <- map4 %>%
        addMarkers(data = region_data, 
                   lng = ~longitude_adjusted, 
                   lat = ~latitude_adjusted, 
                   icon = icone,
                   popup = ~popup_info, # aparece no clique 
                   group = region
        )
    }
    
    # Adicionar controle de camadas para as regiões
    map4 <- map4 %>%
      addLayersControl(
        overlayGroups = unique_regions,
        options = layersControlOptions(collapsed = TRUE, position = "topright") # Ajuste a posição conforme necessário
      )
    
    
    
    # Mostrar o mapa
    map4
  })
    
  

# mapa geral de bolhas definicao de raios ---------------------------------

  
    # 
    # output$mapa_geral <- renderLeaflet({
    #     # Definir cores com base no número de partos
    #     cores <- colorFactor(palette = c("orange", "red", "blue", "purple"), domain = mapa_hospitais$partos)
    #     
    #     # Ajustar o fator de escala para o tamanho dos marcadores
    #     fator_escala <- 0.2 # Você pode ajustar este valor conforme necessário
    #     
    #     # Calcular o tamanho dos marcadores com base no número de partos
    #     mapa_hospitais <- mapa_hospitais %>%
    #         mutate(radius = sqrt(NV) * fator_escala)
    #     
    #     # Criar o popup com várias informações
    #     mapa_hospitais <- mapa_hospitais %>%
    #         mutate(popup_info = sprintf(
    #             "<strong>Estabelecimento:</strong> %s<br/>
    #          <strong>CEP:</strong> %s<br/>
    #          <strong>Município:</strong> %s<br/>
    #          <strong>Número de Partos:</strong> %s<br/>
    #          <strong>Estrato:</strong> %s<br/>
    #          <strong>Macroregião:</strong> %s<br/>
    #          <strong>Região de Saúde:</strong> %s",
    #             estabelecimento, cep, municipio, NV, partos, dsc_macrorregiao_saude, dsc_regiao_saude))
    #     
    #     # Mapa base de Santa Catarina
    #     map4 <- leaflet() %>% 
    #         setView(lng = -51, lat = -27.50, zoom = 7) %>% # Coordenadas do mapa de SC
    #         addTiles() %>% 
    #         addProviderTiles(providers$CartoDB)
    #     
    #     # Adicionar marcadores com agrupamento por região de saúde
    #     unique_regions <- unique(mapa_hospitais$dsc_regiao_saude)
    #     
    #     for (region in unique_regions) {
    #         region_data <- mapa_hospitais %>%
    #             filter(dsc_regiao_saude == region)
    #         
    #         map4 <- map4 %>%
    #             addCircleMarkers(data = region_data, 
    #                              lng = ~longitude_adjusted, 
    #                              lat = ~latitude_adjusted, 
    #                              color = ~cores(partos), 
    #                              popup = ~popup_info, # aparece no clique 
    #                              radius = ~radius,
    #                              group = region
    #             )
    #     }
    #     
    #     # Adicionar controle de camadas para as regiões
    #     map4 <- map4 %>% addLayersControl(
    #         overlayGroups = unique_regions,
    #         options = layersControlOptions(collapsed = FALSE)
    #     )
    #     
    #     # Mostrar o mapa
    #     map4
    # })
    # 
    # 
    # 
    # 
    
    
    

# mapa cluster regiao -----------------------------------------------------

    
    

    output$mapa <- renderLeaflet({


        
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
             <strong>Estrato:</strong> %s<br/>
             <strong>Macroregião:</strong> %s<br/>
             <strong>Região de Saúde:</strong> %s",
            estabelecimento, cep, municipio, NV, partos, Macrorregião, reg_saude))
        
        # Mapa base de Santa Catarina
        map4 <- leaflet() %>% 
            setView(lng = -51, lat = -27.50, zoom = 7) %>% # Coordenadas do mapa de SC
            addTiles() %>% 
            addProviderTiles(providers$CartoDB)
        
        # Adicionar marcadores com agrupamento por região de saúde
        unique_regions <- unique(mapa_hospitais$reg_saude)
        
        for (region in unique_regions) {
            region_data <- mapa_hospitais %>%
                filter(reg_saude == region)
            
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
        
        
    })

    

# mapa cluster macro regiao -----------------------------------------------

    
    
        
output$mapa2 <- renderLeaflet({
            
    
    
    # Definir cores com base no número de partos
    cores <- colorFactor(palette = c("orange", "red", "blue", "purple"), domain = mapa_hospitais$partos)
    
    # Ajustar o fator de escala para o tamanho dos marcadores
    fator_escala <- 0.6 # Você pode ajustar este valor conforme necessário
    
    # Calcular o tamanho dos marcadores com base no número de partos
    mapa_hospitais <- mapa_hospitais %>%
        mutate(radius = sqrt(NV) * fator_escala)
    
    # 1. Criar o popup com várias informações
    mapa_hospitais <- mapa_hospitais %>%
      mutate(popup_info = sprintf(
        "<strong>Estabelecimento:</strong> %s<br/>
             <strong>CEP:</strong> %s<br/>
             <strong>Município:</strong> %s<br/>
             <strong>Número de Partos:</strong> %s<br/>
             <strong>Estrato:</strong> %s<br/>
             <strong>Macroregião:</strong> %s<br/>
             <strong>Região de Saúde:</strong> %s",
        estabelecimento, cep, municipio, NV, partos, Macrorregião, reg_saude))
    
    
    # Mapa base de Santa Catarina
    map4 <- leaflet() %>% 
        setView(lng = -51, lat = -27.50, zoom = 7) %>% # Coordenadas do mapa de SC
        addTiles() %>% 
        addProviderTiles(providers$CartoDB)
    
    # Adicionar marcadores com agrupamento por região de saúde
    unique_regions <- unique(mapa_hospitais$Macrorregião)
    
    for (region in unique_regions) {
        region_data <- mapa_hospitais %>%
            filter(Macrorregião == region)
        
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
    
    
})
    
} 





