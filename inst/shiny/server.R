library(dplyr)
library(DT)
library(wateRuse)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

w.use.start <- wUseSample

shinyServer(function(input, output, session) {
  
  w.use <- reactive({
    w.use <- w.use.start
    
    if(!is.null(input$data)){
      
      path <- file.path(input$data$datapath)
      newPath <- paste0(input$data$datapath,"_",input$data$name)
      newPath <- gsub(", ","_",newPath)
      file.rename(from = path, to = newPath)
      w.use <- get_awuds_data(awuds.data.files = newPath)
    }

    w.use
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
  
  observe({

    w.use <- w.use()
    choices <- names(w.use)[names(w.use) %in% c("STATECOUNTYCODE","COUNTYNAME",
                                                "HUCCODE","Area","USSTATEHUCCODE","HUCNAME")]
    
    updateSelectInput(session, "area.column", 
                             choices = choices, 
                             selected = choices[1])
  })
  
  observe({
    
    w.use <- w.use()
    yRange <- unique(w.use$YEAR)
    
    updateSelectInput(session, "year_x", 
                      choices = yRange, 
                      selected = yRange[length(yRange)-1])
    
    updateSelectInput(session, "year_y", 
                      choices = yRange, 
                      selected = yRange[length(yRange)])
    
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
  
  output$plotTwoElement <- renderPlot({
    w.use <- w.use()
    
    data.elements <- c(input$data.elements.min,input$data.elements.max)
    
    areas <- input$area
    
    areasOptions <- areasOptions()
    
    if(all(areasOptions %in% areas)){
      areas <- NA
    }
    
    area.column <- input$area.column
    year.x.y <- c(input$year_x,input$year_y)
    plotTwoElement <- compare_two_elements(w.use, data.elements, year.x.y, area.column, areas)
    
    # ggsave("plotTwo.png",plotTwo)
    
    print(plotTwoElement)
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
    legend <- input$legendOn
    log <- input$log
    points <- input$points
    
    time_series_data(w.use, data.elements, area.column = area.column, plot.points = points,
                     areas = areas, legend = legend, log = log)
  })
  
  output$rankData <- DT::renderDataTable({
    
    w.use <- w.use()
    data.elements <- input$data.elements
    areas <- input$area
    area.column <- input$area.column
    yearRange <- unique(w.use$YEAR)
    w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas)
    
    df <- spread_(w.use.sub, "YEAR", data.elements)

    rankData <- DT::datatable(df, rownames = FALSE,
                              options = list(scrollX = TRUE,
                                             pageLength = nrow(df),
                                             order=list(list(2,'desc'))))
    
    colors <- brewer.pal(length(yearRange),"Blues")
    names(colors) <- yearRange
    for(i in yearRange){
      rankData <- formatStyle(rankData, as.character(i),
                              background = styleColorBar(range(df[[as.character(i)]],na.rm = TRUE), colors[as.character(i)]),
                              backgroundSize = '100% 90%',
                              backgroundRepeat = 'no-repeat',
                              backgroundPosition = 'center' )
    }

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
    legend <- input$legendOn
    log <- input$log
    
    outText <- paste0(
      'data.elements <- "',data.elements, '"\n',
      "areas <- ",areas, "\n",
      'area.column <- "', area.column, '"\n',
      'legend <- ',legend,"\n",
      'log <- ',log,"\n",
      "time_series_data(w.use, data.elements, area.column = area.column,\n",
      "areas = areas,log=log, legend=legend)"
      
    )
    
    HTML(outText)
    
  })
  
})
