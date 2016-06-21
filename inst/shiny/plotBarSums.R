output$plotBarSums <- renderPlot({
  plotBarSums()
})

plotBarSums <- reactive({
  
  w.use <- w.use_barSum()
  
  validate(
    need(input$state, 'Choose a State'),
    need(input$area, 'Choose an Area')
  )
  
  states <- df[["state"]]
  
  if(!is.null(w.use) && (states != "All Available")){
    w.use <- filter(w.use, USSTATEALPHACODE %in% states)
  }
  
  data.elements <- input$data.total.elements

  plot.stack <- input$plot.stack
  
  plotBarSums <- barchart_sums(w.use, data.elements, 
                               "USSTATEALPHACODE", areas = NA, plot.stack=plot.stack)
  
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
  
  w.use <- w.use_start()
  
  data.elements <- input$data.total.elements
  
  stack <- input$plot.stack
  log <- input$log
  
  if("USSTATEALPHACODE" %in% names(w.use)){
    txt <- paste0('w.use_state <- w.use[,c("USSTATEALPHACODE", "YEAR",totals)] %>%\n',
      '     group_by(USSTATEALPHACODE, YEAR) %>%\n',
      '     summarise_each(funs(sum))')
  } else {
    txt <- paste0('w.use_state <- w.use[,c("YEAR",totals)] %>%\n',
      '     mutate(USSTATEALPHACODE = "01") %>%\n',
      '     group_by(USSTATEALPHACODE, YEAR) %>%\n',
      '     summarise_each(funs(sum))')
  }
  
  outText <- paste0(
    "library(dplyr)\n",
    'plot.stack <- ', stack,"\n",
    'log <- ',log,"\n",
    'w.use[is.na(w.use)] <- 0\n',
    'w.use <- calculate_values(w.use)\n',
    'totals <- c("',paste0(data.elements,collapse = '","'),'")\n',
    txt,"\n",
    "barchart_sums(w.use_state, data.elements, area.column = 'USSTATEALPHACODE',\n",
    "areas = NA,log=log, plot.stack=plot.stack)"
    
  )
  
  HTML(outText)
  
})