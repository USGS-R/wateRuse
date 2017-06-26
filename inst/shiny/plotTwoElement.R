output$plotTwoElement <- renderPlotly({
  plotTwoElement <- plotTwoElement()
  
  plotTwoElement <- plotTwoElement +
    theme(plot.margin=unit(c(10,10,50, 50), "points")) +
    theme(axis.title.y=element_text(vjust= -0.1))
  
  ggplotly(plotTwoElement, width = "100%", height = "1000px")
})

plotTwoElement <- reactive({
  
  validate(
    need(input$state, 'Choose a State'),
    need(input$area, 'Choose an Area')
  )
  
  w.use <- w.use()
  
  data.elements <- c(df[["data.element"]],df[["data.element.y"]])
  
  areas.p2e <- df[["area"]]
  
  if(all(df[["areas"]] %in% areas.p2e)){
    areas.p2e <- NA
  }
  legend <- input$legendOn
  area.column <- df[["area.column"]]
  year <- input$year_x
  
  plotTwoElement <- compare_two_elements(w.use, data.elements, year, 
                                         area.column, areas.p2e, legend=legend)
  
  write.csv(x = plotTwoElement$data, file="plotTwoElement.csv", row.names = FALSE)
  
  return(plotTwoElement)
  
})


output$downloadPlotTwoElem <- downloadHandler(
  filename = function() { "plotTwoElement.png" },
  content = function(file) {
    ggsave(file, plot = plotTwoElement(), device = "png")
  }
)

output$downloadPlotTwoElemPDF <- downloadHandler(
  filename = function() { "plotTwoElement.pdf" },
  content = function(file) {
    ggsave(file, plot = plotTwoElement(), device = "pdf")
  }
)

output$downloadPlotTwoElemData <- downloadHandler(
  filename = function() { "plotTwoElement.csv" },
  content = function(file) {
    file.copy("plotTwoElement.csv", file)
  }
)

output$plotTwoElementCode <- renderPrint({
  
  areas.ptC <- df[["area"]]
  legend <- input$legendOn
  year <- input$year_x
  
  areasOptions <- df[["areas"]]
  
  if(all(areasOptions %in% areas.ptC)){
    areas.ptC <- NA
  } else {
    areas.ptC <- paste0('c("',paste(areas.ptC, collapse = '","'),'")')
  }
  
  area.column <- df[["area.column"]]
  data.elements.x.y <- c(df[["data.element"]],df[["data.element.y"]])
  
  outText <- paste0(
    "areas <- ", areas.ptC, "\n",
    'area.column <- "', area.column, '"\n',
    "year <- ", year, "\n",
    'data.elements.x.y <- c("',paste0(data.elements.x.y,collapse = '","'),'")\n',
    "legend <- ", legend, "\n",
    "compare_two_elements(w.use, data.elements.x.y, year, area.column, areas, legend)"
    
  )
  
  HTML(outText)
  
})