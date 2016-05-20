output$plotBarSums <- renderPlot({
  plotBarSums()
})

plotBarSums <- reactive({
  
  w.use <- w.use_full()
  
  data.elements <- c(df[["data.element"]],df[["data.element.y"]])
  
  areas.p2e <- df[["area"]]
  
  if(all(df[["areas"]] %in% areas.p2e)){
    areas.p2e <- NA
  }
  legend <- input$legendOn
  area.column <- df[["area.column"]]
  year <- input$year_x
  
  plotBarSums <- barchart_sums(w.use, data.elements, year, 
                               area.column, areas.p2e, legend=legend)
  
  write.csv(x = plotBarSums$data, file="plotBarSums.csv", row.names = FALSE)
  
  plotBarSums
  
})