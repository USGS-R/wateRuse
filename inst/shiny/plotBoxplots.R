plotBoxplots <- reactive({
  
  w.use <- w.use()
  
  data.elements <- c(df[["data.element"]],df[["data.element.y"]])
  
  validate(
    need(input$state, 'Choose a State'),
    need(input$area, 'Choose an Area')
  )
  
  areas.p2e <- df[["area"]]
  
  if(all(df[["areas"]] %in% areas.p2e)){
    areas.p2e <- NA
  }
  yrs <- NA # may want to code ability to select certain years
  log <- input$log
  
  area.column <- df[["area.column"]]

  w.use <- filter(w.use, YEAR %in% input$whatYears)
  
  sub.area <- ifelse("COUNTYNAME" %in% names(w.use), "COUNTYNAME", area.column)
  
  plotBoxplots <- boxplot_wu(w.use, data.elements, area.column, areas=areas.p2e, 
                             years=yrs, log=log, sub.area = sub.area)
  
  write.csv(x = plotBoxplots$data, file="plotBoxplots.csv", row.names = FALSE)
  
  return(plotBoxplots)
  
})

output$plotBoxplots <- renderPlotly({
  plotBoxplots <- plotBoxplots()+
    theme(plot.margin=unit(c(10,10,50, 50), "points")) 

  ggplotly(plotBoxplots, width = 700, height = 450)
})

output$downloadPlotBoxplots <- downloadHandler(
  filename = function() { "plotBoxplots.png" },
  content = function(file) {
    ggsave(file, plot = plotBoxplots(), device = "png")
  }
)

output$downloadPlotBoxplotsPDF <- downloadHandler(
  filename = function() { "plotBoxplots.pdf" },
  content = function(file) {
    ggsave(file, plot = plotBoxplots(), device = "pdf")
  }
)

output$downloadPlotBoxplotsData <- downloadHandler(
  filename = function() { "plotBoxplots.csv" },
  content = function(file) {
    file.copy("plotBoxplots.csv", file)
  }
)

output$plotBoxplotsCode <- renderPrint({
  
  data.elem <- c(df[["data.element"]],df[["data.element.y"]])
  areas.pTC <- df[["areas"]]

  areasOptions <-  df[["areas"]]
  
  if(all(areasOptions %in% areas.pTC)){
    areas.pTC <- NA
  } else {
    areas.pTC <- paste0('c("',paste(areas.pTC, collapse = '","'),'")')
  }
  
  area.column <- df[["area.column"]]
  yrs <- NA #to be changed when code added for year selection
  log <- input$log
  
  outText <- paste0(
    'data.elements <- c("',paste0(data.elem,collapse = '","'),'")\n',
    "areas <- ",areas.pTC, "\n",
    'area.column <- "', area.column, '"\n',
    'yrs <- ', yrs,"\n",
    'log <- ',log,"\n",
    "boxplot_wu(w.use, data.elements, area.column, areas=areas.p2e,\n", 
    "years=yrs, log=log)"
    
  )
  
  HTML(outText)
  
})