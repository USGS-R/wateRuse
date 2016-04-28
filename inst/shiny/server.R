library(leaflet)
library(dplyr)
library(DT)
library(wateRuse)
library(ggplot2)

w.use.start <- wUseSample

shinyServer(function(input, output, session) {
  
  w.use <- reactive({
    w.use <- w.use.start
  })
  
  areasOptions <- reactive({
    area.column <- input$area.column
    w.use <- w.use()
    areaOptions <- unique(w.use[[area.column]])
    areaOptions
  })
  
  observe({
    area.column <- input$area.column
    w.use <- w.use()
    choices <- unique(w.use[[area.column]])
    
    updateCheckboxGroupInput(session, "area", 
                             choices = choices, 
                             selected = choices)
  })
  
  output$plotTwo <- renderPlot({
    w.use <- w.use()
    
    data.elements <- input$data.elements
    areas <- input$area
    
    areasOptions <- areasOptions()
    
    if(all(areas  %in% areasOptions)){
      areas <- NA
    }
    
    area.column <- input$area.column
    year.x.y <- c(input$year_x,input$year_y)
    plotTwo <- compare_two_years(w.use, data.elements, year.x.y, areas, area.column)
    
    # ggsave("plotTwo.png",plotTwo)
    
    print(plotTwo)
  })
  
  output$plotTime <- renderPlot({
    w.use <- w.use()
    
    data.elements <- input$data.elements
    areas <- input$area
    
    areasOptions <- areasOptions()
    
    if(all(areas  %in% areasOptions)){
      areas <- NA
    }
    
    area.column <- input$area.column

    time_series_data(w.use, data.elements, area.column = area.column, areas = areas)
  })
 
  output$downloadPlotTwo <- downloadHandler(
    
    filename = function() {
      "plotTwo.png"
    },
    content = function(file) {
      file.copy("plotTwo.png", file)
    }
  )
  
  output$plotTwoCode <- renderPrint({
    
    data.elements <- input$data.elements
    areas <- input$area
    
    areasOptions <- areasOptions()
    
    if(all(areas  %in% areasOptions)){
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
    
    areasOptions <- areasOptions()
    
    if(all(areas  %in% areasOptions)){
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
