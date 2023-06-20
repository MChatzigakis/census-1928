server <- function(input, output) {
  
  #Filter reset button
  observeEvent(input$reset_button, {
    reset("menu")
  })
  
  #Filter data 
  data_filtered <- reactive({
    req(input$input_variable)
    req(input$input_gender)
    
    data[data$variable == input$input_variable &
           data$strat == input$input_gender,]
  })
  
  #Create map
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapboxGL(style = "mapbox://styles/mchatzigakis/clen1kdwx007q01pnzmhfmw3a") %>% 
      addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light",
                                                               accessToken = Sys.getenv('pk.eyJ1IjoibWNoYXR6aWdha2lzIiwiYSI6ImNsZW4wempkajFhdzAzdnBnbjFpMnl2dHQifQ.LxP677Ldt2WHej2Wmf1kQA'),
                                                               attribution = "&copy; <a href=\"https://www.mapbox.com/about/maps/\" target='_blank' rel='noopener noreferrer' e.preventDefault()>Mapbox</a>
                                                                              &copy; <a href=\"https://www.openstreetmap.org/copyright\" target='_blank' rel='noopener noreferrer' e.preventDefault()>OpenStreetMap</a>
                                                                              &copy; <a href=\"https://carto.com/attributions\" target='_blank' rel='noopener noreferrer' e.preventDefault()>CARTO</a>")) %>%
      setView(lng = 23.8, lat = 38.2, zoom = 7) %>% 
      addMapPane(name = "polygons", zIndex = 410) %>% 
      addMapPane(name = "maplabels", zIndex = 420) %>% 
      addProviderTiles("CartoDB.PositronNoLabels",
                       options = providerTileOptions(attribution = FALSE)) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels",
                       options = leafletOptions(pane = "maplabels", attribution = FALSE),
                       group = "map labels") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to World Map",
        onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addEasyButton(
        button = easyButton(
          icon = 'fa-info-circle', title="Info",
          onClick = JS('function(btn, map){ alert("The icons above and below can help you adjust the view and find your province in the map."); }'))) %>%
      addResetMapButton()%>%
      addSearchOSM(options = searchOptions(autoCollapse = TRUE, 
                                           minLength = 2, 
                                           zoom = 12, 
                                           hideMarkerOnCollapse = TRUE))
  })
  
  #Make map reactive
  observeEvent(list(input$input_variable, input$input_gender), {
    
    req(data_filtered())
    
    #Merge .shp with data
    df_map<-merge(map,data_filtered(), by='geo_hrm')
    
    #Change variable in legend based on the selected variable
    if (is.null(input$input_variable)) return(leafletProxy("map") %>% removeControl("Legend")) 
    
    bin_tot_pop<-c(0,15000,30000,45000,60000,75000,90000,105000,375000,900000)
    bin_tot_mf_pop<-c(0,5000,10000,15000,20000,25000,30000,50000,100000,500000)
    bin_r_pop<-c(0,50,200,400,800,1600,3200,6400,12800,25600,51200,256000)
    bin_ra22_pop<-c(0,50,200,400,800,1600,3200,6400,12800,25600,51200,256000)
    bin_rb22_pop<-c(0,10,25,75,150,500,1500,3000,5000,150000,30000)
    bin_per_ra22_pop<-c(0,0.25,1,5,15,30,50,75,100)
    bin_per_rb22_pop<-c(0,0.05,0.1,0.25,0.5,1,2.5,5,10,50,100)
    bin_per_r_pop<-c(0,0.25,0.5,1,2.5,5,10,25,50,75,100)
    
    #Set color based on the selected variable
    if (input$input_variable == "Πληθυσμός") {
      df_map$legend_title <- "Πληθυσμός"
      palette <- colorBin(palette = "viridis",bins=bin_tot_pop,domain = df_map$value,na.color=rgb(0,0,0,0))
      df_map$color <- palette(df_map$value)
      
      if (input$input_gender %in% c("Άνδρες", "Γυναίκες")) {
        palette <- colorBin(palette = "viridis",bins=bin_tot_mf_pop,domain = df_map$value,na.color=rgb(0,0,0,0))
        df_map$color <- palette(df_map$value)
      }
    }
    
    if (input$input_variable == "Πληθυσμός Προσφύγων 1928") {
      df_map$legend_title <- "Πληθυσμός Προσφύγων 1928"
      palette <- colorBin(palette = "viridis",bins=bin_r_pop,domain = df_map$value,na.color=rgb(0,0,0,0))
      df_map$color <- palette(df_map$value)
      
      if (input$input_gender %in% c("Άνδρες", "Γυναίκες")) {
        
        showModal(
          modalDialog(
            "Unfortunately there is no available data for that combination. Please select another combination.",
            footer = modalButton("Close"),
            easyClose = TRUE
          )
        )
        
      } 
      
    }
    
    if(input$input_variable == "Ποσοστό Προσφύγων 1928") {
      df_map$legend_title <- HTML("Ποσοστό Προσφύγων 1928")
      palette <- colorBin(palette = "viridis",bins=bin_per_r_pop,domain = df_map$value,na.color=rgb(0,0,0,0))
      df_map$color <- palette(df_map$value)
      
      if (input$input_gender %in% c("Άνδρες", "Γυναίκες")) {
        
        showModal(
          modalDialog(
            "Unfortunately there is no available data for that combination. Please select another combination.",
            footer = modalButton("Close"),
            easyClose = TRUE
          )
        )
        
      } 
      
    }
    
    if(input$input_variable == "Πληθυσμός Προσφύγων μετά το 1922") {
      df_map$legend_title <- "Πληθυσμός Προσφύγων μετά το 1922"
      palette <- colorBin(palette = "viridis",bins=bin_ra22_pop,domain = df_map$value,na.color=rgb(0,0,0,0))
      df_map$color <- palette(df_map$value)
    }
    
    if(input$input_variable == "Ποσοστό Προσφύγων μετά το 1922") {
      palette <- colorBin(palette = "viridis",bins=bin_per_ra22_pop,domain = df_map$value,na.color=rgb(0,0,0,0))
      df_map$color <- palette(df_map$value)
      
      if (input$input_gender == "Σύνολο") {
        df_map$legend_title <- HTML("Ποσοστό Προσφύγων μετά το 1922 <br> στο Σύνολο του Γενικού Πληθυσμού")
      }
      else if (input$input_gender == "Άνδρες") {
        df_map$legend_title <- HTML("Ποσοστό Ανδρών Προσφύγων μετά το 1922 <br> στο Σύνολο του Προσφυγικού Πληθυσμού")
      } 
      else if (input$input_gender == "Γυναίκες") {
        df_map$legend_title <- HTML("Ποσοστό Γυναικών Προσφύγων μετά το 1922 <br> στο Σύνολο του Προσφυγικού Πληθυσμού")
      }
      
    }
    
    if(input$input_variable == "Πληθυσμός Προσφύγων πριν το 1922") {
      df_map$legend_title <- "Πληθυσμός Προσφύγων πριν το 1922"
      palette <- colorBin(palette = "viridis",bins=bin_rb22_pop,domain = df_map$value,na.color=rgb(0,0,0,0))
      df_map$color <- palette(df_map$value)
      
      if (input$input_gender %in% c("Άνδρες", "Γυναίκες")) {
        
        showModal(
          modalDialog(
            "Unfortunately there is no available data for that combination. Please select another combination.",
            footer = modalButton("Close"),
            easyClose = TRUE
          )
        )
        
      } 
      
    }
    
    if(input$input_variable == "Ποσοστό Προσφύγων πριν το 1922") {
      df_map$legend_title <- "Ποσοστό Προσφύγων πριν το 1922"
      palette <- colorBin(palette = "viridis",bins=bin_per_rb22_pop,domain = df_map$value,na.color=rgb(0,0,0,0))
      df_map$color <- palette(df_map$value)
      
      if (input$input_gender %in% c("Άνδρες", "Γυναίκες")) {
        
        showModal(
          modalDialog(
            "Unfortunately there is no available data for that combination. Please select another combination.",
            footer = modalButton("Close"),
            easyClose = TRUE
          )
        )
      } 
    }
    
    #Create reactive map
    leafletProxy("map") %>%
      addPolygons(data = df_map,
                  color = "#000066",
                  label=df_map$epar_gr,
                  weight=0.7,
                  opacity=0.5,
                  stroke=TRUE,
                  fillColor = df_map$color,
                  fillOpacity = 1,
                  smoothFactor =1,
                  popup = paste0("<center></br>","<font size='2px'><b>", df_map$epar_gr,"</b></font>","</center>",
                                 "</br><b>",df_map$legend_title,"</b>: ", df_map$value),
                  highlightOptions = highlightOptions(weight = 2,
                                                      fillOpacity = 1,
                                                      color = "#000066",
                                                      opacity = 1,
                                                      bringToFront = TRUE),
                  options = leafletOptions(pane = "polygons")) %>%
      
      addLegend(df_map,
                position = "bottomright",
                values = df_map$value,
                pal=palette,
                title = unique(df_map$legend_title),
                layerId = "Legend")
    
    delay(300,runjs(HTML('$("input.search-input")[0].placeholder = "Find your province!"')))
  })
  
}