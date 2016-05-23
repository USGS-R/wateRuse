output$mapData <- renderPlot({
  mapData()
})

mapData <- reactive({
  
  w.use <- w.use()
  
  norm.element <- df[["data.element.norm"]]
  
  if(norm.element == "None"){
    norm.element <- NA
  }
  
  if((df[["area.column"]] %in% c("Area","STATECOUNTYCODE"))){
    if(!("STATECOUNTYCODE" %in% names(w.use))){
      w.use$STATECOUNTYCODE <- paste0(stateCd$STATE[which(stateCd$STATE_NAME == input$stateToMap)],w.use[[df[["area.column"]]]])
    }
    
    mapData <- choropleth_plot(w.use, df[["data.element"]], year = input$yearToMap,
                               area.column = "STATE_TERR", area = input$stateToMap, norm.element = norm.element)
    
  } else {
    mapData <- ggplot(data = mtcars) +
      geom_text(x=0.5, y=0.5, label = "Choose new state or use County data")
  }
  
  mapData
  
})

output$hover_map <- renderPrint({
  txt <- ""
  
  hover=input$hover_map
  
  if(!is.null(hover)){
    
    data <- histCounties
    point.to.check <- SpatialPoints(data.frame(x = hover$x, y=hover$y), proj4string=CRS(proj4string(data)))
    
    dist=over(point.to.check, data)
    txt <- dist$FIPS
  }
  
  cat("Site: ", txt)
})

output$downloadMap <- downloadHandler(
  filename = function() { "map.png" },
  content = function(file) {
    ggsave(file, plot = mapData(), device = "png")
  }
)