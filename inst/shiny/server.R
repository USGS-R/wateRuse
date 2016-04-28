library(dplyr)
library(DT)
library(wateRuse)
library(ggplot2)
library(tidyr)

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
    
    if(all(areasOptions %in% areas)){
      areas <- NA
    }
    
    area.column <- input$area.column
    year.x.y <- c(input$year_x,input$year_y)
    plotTwo <- compare_two_years(w.use, data.elements, year.x.y, area.column, areas)
    
    # ggsave("plotTwo.png",plotTwo)
    
    print(plotTwo)
  })
  
  output$plotTime <- renderPlot({
    w.use <- w.use()
    
    data.elements <- input$data.elements
    areas <- input$area
    
    areasOptions <- areasOptions()
    
    if(all(areasOptions %in% areas)){
      areas <- NA
    } 
    
    area.column <- input$area.column

    time_series_data(w.use, data.elements, area.column = area.column, areas = areas)
  })
  
  output$rankData <- DT::renderDataTable({
    
    w.use <- w.use()
    data.elements <- input$data.elements
    areas <- input$area
    area.column <- input$area.column
    
    w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas)

    w.use.sub <-  w.use.sub[w.use.sub$YEAR %in% year.x.y,] 
    
    df <- spread_(w.use.sub, "YEAR", data.elements)
    
    
    # rankData <- DT::datatable(statCol, extensions = 'Buttons', 
    #                            rownames = FALSE,
    #                            options = list(#dom = 'ft',
    #                              dom = 'Bfrtip',
    #                              buttons =
    #                                list('colvis', list(
    #                                  extend = 'collection',
    #                                  buttons = list(list(extend='csv',
    #                                                      filename = 'hitStats'),
    #                                                 list(extend='excel',
    #                                                      filename = 'hitStats'),
    #                                                 list(extend='pdf',
    #                                                      filename= 'hitStats')),
    #                                  text = 'Download'
    #                                )
    #                                ),
    #                              scrollX = TRUE,
    #                              pageLength = nrow(statCol),
    #                              order=list(list(colToSort,'desc'))))
    # 
    # rankData <- formatRound(tableSumm, names(statCol)[-ignoreIndex], 2) 
    # 
    # for(i in 1:length(maxEARS)){
    #   tableSumm <- formatStyle(tableSumm, 
    #                            names(statCol)[maxEARS[i]], 
    #                            backgroundColor = colors[i])
    #   tableSumm <- formatStyle(tableSumm, 
    #                            names(statCol)[freqCol[i]], 
    #                            backgroundColor = colors[i])
    #   
    #   tableSumm <- formatStyle(tableSumm, names(statCol)[maxEARS[i]], 
    #                            background = styleColorBar(range(statCol[,names(statCol)[maxEARS[i]]],na.rm = TRUE), 'goldenrod'),
    #                            backgroundSize = '100% 90%',
    #                            backgroundRepeat = 'no-repeat',
    #                            backgroundPosition = 'center' ) 
    #   tableSumm <- formatStyle(tableSumm, names(statCol)[freqCol[i]], 
    #                            background = styleColorBar(range(statCol[,names(statCol)[freqCol[i]]],na.rm = TRUE), 'wheat'),
    #                            backgroundSize = '100% 90%',
    #                            backgroundRepeat = 'no-repeat',
    #                            backgroundPosition = 'center') 
    #   
    # }

    rankData <- DT::datatable(df, rownames = FALSE,
                              options = list(scrollX = TRUE,
                                             pageLength = nrow(w.use)))
    rankData
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
    
    if(all(areasOptions %in% areas)){
      areas <- NA
    } else {
      areas <- paste0('c("',paste(areas, collapse = '","'),'")')
    }
    
    area.column <- input$area.column
    year.x.y <- c(input$year_x,input$year_y)
    
    outText <- paste0(
      'data.elements <- "',data.elements, '"\n',
      "areas <- ", areas, "\n",
      'area.column <- "', area.column, '"\n',
      "year.x.y <- c(",paste0(year.x.y,collapse = ","),")\n",
      "compare_two_years(w.use, data.elements, year.x.y, areas, area.column)"
      
    )
    
    HTML(outText)
    
  })
  
  output$plotTimeCode <- renderPrint({
    
    data.elements <- input$data.elements
    areas <- input$area
    
    areasOptions <- areasOptions()
    
    if(all(areasOptions %in% areas)){
      areas <- NA
    } else {
      areas <- paste0('c("',paste(areas, collapse = '","'),'")')
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
