output$plotBarSums <- renderPlot({
  plotBarSums()
})

plotBarSums <- reactive({
  
  w.use <- w.use()
  
  data.elements <- as.character(df[["data.total.element"]])
  
  areas.p2e <- df[["area"]]
  
  if(all(df[["areas"]] %in% areas.p2e)){
    areas.p2e <- NA
  }
  
  plot.stack <- input$plot.stack
  
  area.column <- df[["area.column"]]
  
  plotBarSums <- barchart_sums(w.use, data.elements, 
                               area.column, areas = areas.p2e, plot.stack=plot.stack)
  
  write.csv(x = plotBarSums$data, file="plotBarSums.csv", row.names = FALSE)
  
  plotBarSums
  
})

output$downloadPlotBarSums <- downloadHandler(
  filename = function() { "plotBarSums.png" },
  content = function(file) {
    ggsave(file, plot = plotBarSums(), device = "png")
  }
)

output$downloadPlotBarSumsPDF <- downloadHandler(
  filename = function() { "plotBarSums.pdf" },
  content = function(file) {
    ggsave(file, plot = plotBarSums(), device = "pdf")
  }
)

output$downloadPlotBarSumsData <- downloadHandler(
  filename = function() { "plotBarSums.csv" },
  content = function(file) {
    file.copy("plotBarSums.csv", file)
  }
)

output$plotBarSumsCode <- renderPrint({
  
  data.elements <- df[["data.total.element"]]
  
  areas.pTC <- df[["area"]]
  
  areasOptions <-  df[["areas"]]
  
  if(all(areasOptions %in% areas.pTC)){
    areas.pTC <- NA
  } else {
    areas.pTC <- paste0('c("',paste(areas.pTC, collapse = '","'),'")')
  }
  
  area.column <- df[["area.column"]]
  stack <- input$plot.stack
  log <- input$log
  
  outText <- paste0(
    'data.elements <- c("',paste0(data.elements,collapse = '","'),'")\n',
    "areas <- ",areas.pTC, "\n",
    'area.column <- "', area.column, '"\n',
    'plot.stack <- ', stack,"\n",
    'log <- ',log,"\n",
    "barchart_sums(w.use, data.elements, area.column = area.column,\n",
    "areas = areas,log=log, plot.stack=plot.stack)"
    
  )
  
  HTML(outText)
  
})