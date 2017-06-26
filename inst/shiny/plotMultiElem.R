plotMultiElem <- reactive({
  
  validate(
    need(input$state, 'Choose a State'),
    need(input$area, 'Choose an Area')
  )
  
  w.use <- w.use()
  
  data.elements <- c(df[["data.element"]],df[["data.element.y"]])
  
  areas.p2e <- df[["area"]]
  
  if(all(df[["areas"]] %in% areas.p2e) | length(areas.p2e) > 3){
    areas.p2e <- areas.p2e[1:3]
  } 
  
  legend <- input$legendOn
  points <- input$points
  log <- input$log
  
  area.column <- df[["area.column"]]
  
  w.use <- filter(w.use, YEAR %in% input$whatYears)
  
  plotMultiElem <- multi_element_data(w.use, data.elements, area.column, log = log,
                                      areas=areas.p2e, legend=legend, plot.points = points)
  
  write.csv(x = plotMultiElem$data, file="plotMultiElem.csv", row.names = FALSE)
  
  plotMultiElem
  
})

output$plotMultiElem <- renderPlotly({
  plotMultiElem <- plotMultiElem()
  ggplotly(plotMultiElem, width = "100%")
})

output$downloadPlotmultiElem <- downloadHandler(
  filename = function() { "plotMultiElem.png" },
  content = function(file) {
    ggsave(file, plot = plotMultiElem(), device = "png")
  }
)

output$downloadPlotmultiElemPDF <- downloadHandler(
  filename = function() { "plotMultiElem.pdf" },
  content = function(file) {
    ggsave(file, plot = plotMultiElem(), device = "pdf")
  }
)

output$downloadPlotmultiElemData <- downloadHandler(
  filename = function() { "plotMultiElem.csv" },
  content = function(file) {
    file.copy("plotMultiElem.csv", file)
  }
)

output$plotMultiElemCode <- renderPrint({
  
  data.elements.x.y <- c(df[["data.element"]],df[["data.element.y"]])
  areas.pTC <- df[["area"]]
  
  areasOptions <-  df[["areas"]]
  
  if(all(areasOptions %in% areas.pTC) | length(areas.pTC) > 3){
    areas.pTC <- paste0('c("',paste(areas.pTC[1:3], collapse = '","'),'")')
  } else {
    areas.pTC <- paste0('c("',paste(areas.pTC, collapse = '","'),'")')
  }
  
  area.column <- df[["area.column"]]
  legend <- input$legendOn
  points <- input$points
  log <- input$log
  
  outText <- paste0(
    'data.elements <- c("',paste0(data.elements.x.y,collapse = '","'),'")\n',
    "areas <- ",areas.pTC, "\n",
    'area.column <- "', area.column, '"\n',
    'legend <- ',legend,"\n",
    'points <- ', points,"\n",
    'log <- ',log,"\n",
    "multi_element_data(w.use, data.elements, area.column = area.column,\n",
    "areas = areas,log=log, legend=legend, plot.points=points)"
    
  )
  
  HTML(outText)
  
})