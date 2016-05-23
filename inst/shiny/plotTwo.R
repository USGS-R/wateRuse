output$plotTwo <- renderPlot({
  plotTwo()
})

plotTwo <- reactive({
  
  w.use <- w.use()
  
  data.elements <- df[["data.element"]]
  areas.p2 <- df[["area"]]#input$area
  
  if(all(df[["areas"]] %in% areas.p2)){
    areas.p2 <- NA
  }
  legend <- input$legendOn
  
  area.column <- df[["area.column"]]
  year.x.y <- c(input$year_x,input$year_y)
  plotTwo <- compare_two_years(w.use, data.elements, year.x.y, area.column, areas = areas.p2, legend=legend)
  
  write.csv(x = plotTwo$data, file = "plotTwoYears.csv", row.names = FALSE)
  
  plotTwo
})

output$hover_plotTwo <- renderPrint({
  txt <- ""
  
  if(!is.null(input$hover_plotTwo)){
    hover=input$hover_plotTwo
    plotTwo <- plotTwo()
    data <- plotTwo$data
    dist=sqrt((hover$x-data$x)^2+(hover$y-data$y)^2)
    if(min(dist, rm.na=TRUE) < 5){
      txt <- data$site[which.min(dist)]
    }
  }
  
  cat("Site:",txt)
  
})

output$downloadPlotTwo <- downloadHandler(
  filename = function() { "plotTwoYears.png" },
  content = function(file) {
    ggsave(file, plot = plotTwo(), device = "png")
  }
)

output$downloadPlotTwoPDF <- downloadHandler(
  filename = function() { "plotTwoYears.pdf" },
  content = function(file) {
    ggsave(file, plot = plotTwo(), device = "pdf")
  }
)

output$downloadPlotTwoData <- downloadHandler(
  filename = function() { "plotTwoYears.csv" },
  content = function(file) {
    file.copy("plotTwoYears.csv", file)
  }
)

output$plotTwoCode <- renderPrint({
  
  data.elements <- input$data.elements
  areas.ptC <- df[["area"]]
  legend <- input$legendOn
  
  areasOptions <- df[["areas"]]
  
  if(all(areasOptions %in% areas.ptC)){
    areas.ptC <- NA
  } else {
    areas.ptC <- paste0('c("',paste(areas.ptC, collapse = '","'),'")')
  }
  
  area.column <- df[["area.column"]]
  year.x.y <- c(input$year_x,input$year_y)
  
  outText <- paste0(
    'data.elements <- "',data.elements, '"\n',
    "areas <- ", areas.ptC, "\n",
    'area.column <- "', area.column, '"\n',
    "year.x.y <- c(",paste0(year.x.y,collapse = ","),")\n",
    "legend <- ", legend, "\n",
    "compare_two_years(w.use, data.elements, year.x.y, area.column, areas, legend)"
    
  )
  
  HTML(outText)
  
})