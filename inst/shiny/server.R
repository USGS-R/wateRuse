library(leaflet)
library(dplyr)
library(DT)
library(wateRuse)

w.use.start <- wUseSample

shinyServer(function(input, output, session) {
  
  w.use <- reactive({
    w.use <- w.use.start
  })
  
  output$plotTwo <- renderPlot({
    w.use <- w.use()
    
    data.elements <- input$data.elements
    areas <- input$area
    if(areas == "All"){
      areas <- NA
    }
    
    area.column <- input$area.column
    year.x.y <- c(input$year_x,input$year_y)
    compare_two_years(w.use, data.elements, year.x.y, areas, area.column)
  })
  
  output$plotTime <- renderPlot({
    w.use <- w.use()
    
    data.elements <- input$data.elements
    areas <- input$area
    if(areas == "All"){
      areas <- NA
    }
    
    area.column <- input$area.column

    time_series_data(w.use, data.elements, area.column = area.column, areas = areas)
  })
 
  output$plotTwoCode <- renderPrint({
    
    data.elements <- input$data.elements
    areas <- input$area
    if(areas == "All"){
      areas <- NA
    }
    
    area.column <- input$area.column
    year.x.y <- c(input$year_x,input$year_y)
    
    outText <- paste0(
      'data.elements <- "',data.elements, '"\n',
      "areas <- ",areas, "\n",
      'area.column <- "', area.column, '"\n',
      "year.x.y <- ",year.x.y,"\n",
      "compare_two_years(w.use, data.elements, year.x.y, areas, area.column)"
      
    )
    
    HTML(outText)
    
  })
  
  output$plotTimeCode <- renderPrint({
    
    data.elements <- input$data.elements
    areas <- input$area
    if(areas == "All"){
      areas <- NA
    }
    
    area.column <- input$area.column
    
    outText <- paste0(
      'data.elements <- "',data.elements, '"\n',
      "areas <- ",areas, "\n",
      'area.column <- "', area.column, '"\n',
      "time_series_data(w.use, data.elements, area.column = area.column, areas = areas)"
      
    )
    
    HTML(outText)
    
  })
  
})
