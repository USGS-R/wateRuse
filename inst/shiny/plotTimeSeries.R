tsPlot <- reactive({
  
  validate(
    need(input$state, 'Choose a State'),
    need(input$area, 'Choose an Area')
  )
  
  w.use <- w.use()
  
  data.elements <- df[["data.element"]]
  areas.pt <- df[["area"]]
  
  areasOptions <- df[["areas"]]
  
  if(all(areasOptions %in% areas.pt)){
    areas.pt <- NA
  }
  
  area.column <-  df[["area.column"]]
  legend <- input$legendOn
  log <- input$log
  points <- input$points
  
  w.use <- filter(w.use, YEAR %in% input$whatYears)

  tsPlot <- time_series_data(w.use, data.elements, area.column, plot.points = points,
                             areas = areas.pt, legend = legend, log = log, years= NA)
  
  write.csv(file = "tsPlot.csv", tsPlot$data, row.names = FALSE)
  
  return(tsPlot)
})

output$plotTime <- renderPlotly({
  tsPlot <- tsPlot()+
    theme(plot.margin=unit(c(10,10,90, 90), "points"))
  
  ggplotly(tsPlot, width = 700, height = 400)
})

# output$hover_info_ts <- renderPrint({
#   txt <- ""
#   
#   points <- input$points
#   hover=input$hover_info_ts
#   
#   if(!is.null(hover)){
#     tsPlot <- tsPlot()
#     data <- tsPlot$data
#     
#     if(points){
#       dist=sqrt((hover$x-as.numeric(data$YEAR))^2+(hover$y-data$value)^2)
#       if(min(dist, rm.na=TRUE) < 5){
#         txt <- data[[df[["area.column"]]]][which.min(dist)]
#       }
#       # } else {
#       #   dist=sqrt((hover$x-data$YEAR)^2)
#       #   levels()
#       #   if(min(dist, rm.na=TRUE) < 5){
#       #     txt <- data[[df[["area.column"]]]][which.min(dist)]
#       #   }
#     }
#     
#   }
#   
#   cat("Site: ", txt)
# })

output$downloadPlotTime <- downloadHandler(
  filename = function() { "tsPlot.png" },
  content = function(file) {
    ggsave(file, plot = tsPlot(), device = "png")
  }
)

output$downloadPlotTimePDF <- downloadHandler(
  filename = function() { "tsPlot.pdf" },
  content = function(file) {
    ggsave(file, plot = tsPlot(), device = "pdf")
  }
)

output$downloadPlotTimeData <- downloadHandler(
  filename = function() { "tsPlot.csv" },
  content = function(file) {
    file.copy("tsPlot.csv", file)
  }
)

output$plotTimeCode <- renderPrint({
  
  data.elements <- df[["data.element"]]
  areas.pTC <- df[["area"]]
  
  areasOptions <-  df[["areas"]]
  
  if(all(areasOptions %in% areas.pTC)){
    areas.pTC <- NA
  } else {
    areas.pTC <- paste0('c("',paste(areas.pTC, collapse = '","'),'")')
  }
  
  area.column <- df[["area.column"]]
  legend <- input$legendOn
  points <- input$points
  log <- input$log
  
  outText <- paste0(
    'data.elements <- "',data.elements, '"\n',
    "areas <- ",areas.pTC, "\n",
    'area.column <- "', area.column, '"\n',
    'legend <- ',legend,"\n",
    'points <- ', points,"\n",
    'log <- ',log,"\n",
    "time_series_data(w.use, data.elements, area.column = area.column,\n",
    "areas = areas,log=log, legend=legend, plot.points=points)"
    
  )
  
  HTML(outText)
  
})