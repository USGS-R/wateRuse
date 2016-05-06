library(dplyr)
library(DT)
library(wateRuse)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

w.use.start <- wUseSample
options(shiny.maxRequestSize=50*1024^2) 

shinyServer(function(input, output, session) {
  
  w.use_full <- reactive({
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
  
  w.use <- reactive({
    w.use <- w.use_full()
    states <- df[["state"]]
    
    if(!is.null(w.use) && "USSTATEALPHACODE" %in% names(w.use) | states != "All Available"){
      w.use <- filter(w.use, USSTATEALPHACODE %in% states)
    }
    
    w.use
    
  })
  
  df <- reactiveValues(area.column="COUNTYNAME",
                       area.columns=c("STATECOUNTYCODE","COUNTYNAME"),
                       areas = unique(w.use.start[["COUNTYNAME"]]),
                       area = unique(w.use.start[["COUNTYNAME"]]),
                       states = unique(w.use.start[["USSTATEALPHACODE"]]),
                       state = unique(w.use.start[["USSTATEALPHACODE"]])[1])
  
  observeEvent(input$data, ignoreNULL = TRUE, {
    w.use <- w.use_full()
    
    if(!is.null(w.use_full) && "USSTATEALPHACODE" %in% names(w.use_full)){
      choices <- unique(w.use_full$USSTATEALPHACODE)
    } else {
      choices <- "All Available"
    }

    choice.area <- names(w.use)[names(w.use) %in% c("STATECOUNTYCODE","COUNTYNAME",
                                                "HUCCODE","Area","USSTATEHUCCODE","HUCNAME")]
    
    choice.area <- choice.area[order(choice.area)]
    
    df[["states"]] <- choices
    df[["state"]] <- choices[1]
    df[["area.column"]] <- choice.area[1]
    df[["area.columns"]] <- choice.area
    df[["area"]] <- unique(w.use[[choice.area[1]]])
    df[["areas"]] <- unique(w.use[[choice.area[1]]])
    
  })
  
  observeEvent(input$area,  {
    
    df[["area"]] <- input$area
    
  })
  
  observeEvent(input$area.column,  {
    
    df[["area.column"]] <- input$area.column
    w.use <- w.use()
    
    df[["areas"]] <- unique(w.use[[input$area.column]])
    df[["area"]] <- unique(w.use[[input$area.column]])
    
  })

  observeEvent(input$state, ignoreNULL = TRUE, {

    w.use <- w.use_full()
    df[["state"]] <- input$state
    area.column <- df[["area.column"]]
    
    if(!is.null(w.use) && "USSTATEALPHACODE" %in% names(w.use) | df[["state"]] != "All Available"){
      w.use <- filter(w.use, USSTATEALPHACODE %in% df[["state"]])
    }
    df[["areas"]] <- unique(w.use[[area.column]])
    df[["area"]] <- unique(w.use[[area.column]])
  })
  
  observe({

    updateCheckboxGroupInput(session, "area", 
                             choices =  df[["areas"]], 
                             selected =  df[["area"]])
  })
  
  observe({

    updateSelectInput(session, "area.column", 
                             choices = df[["area.columns"]], 
                             selected = df[["area.column"]])
  })
  
  observe({

    choices <- df[["states"]]
    
    updateCheckboxGroupInput(session, "state",
                      choices = choices,
                      selected = choices[1])
  })
  
  observe({

    w.use <- w.use()
    data.elements <- input$data.elements
    areas.yr <- df[["areas"]]#input$area
    area.column <- df[["area.column"]]

    w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas.yr)

    yRange <- unique(w.use.sub$YEAR[!is.na(w.use.sub[[data.elements]])])

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
    areas.p2 <- df[["area"]]#input$area

    if(all(df[["areas"]] %in% areas.p2)){
      areas.p2 <- NA
    }

    area.column <- df[["area.column"]]
    year.x.y <- c(input$year_x,input$year_y)
    plotTwo <- compare_two_years(w.use, data.elements, year.x.y, area.column, areas.p2)

    # ggsave("plotTwo.png",plotTwo)

    print(plotTwo)
  })

  output$plotTwoElement <- renderPlot({
    w.use <- w.use()

    data.elements <- c(input$data.elements.min,input$data.elements.max)

    areas.p2e <- df[["area"]]

    if(all(df[["areas"]] %in% areas.p2e)){
      areas.p2e <- NA
    }

    area.column <- df[["area.column"]]
    year.x.y <- c(input$year_x,input$year_y)
    plotTwoElement <- compare_two_elements(w.use, data.elements, year.x.y, area.column, areas.p2e)

    print(plotTwoElement)
  })

  output$plotTime <- renderPlot({
    w.use <- w.use()

    data.elements <- input$data.elements
    areas.pt <- df[["areas"]] #input$area

    areasOptions <- df[["areas"]]

    if(all(areasOptions %in% areas.pt)){
      areas.pt <- NA
    }

    area.column <-  df[["area.column"]]
    legend <- input$legendOn
    log <- input$log
    points <- input$points

    w.use <- subset_wuse(w.use, data.elements, area.column, areas.pt)
    w.use <- w.use[!is.na(w.use[data.elements]),]
    
    time_series_data(w.use, data.elements, area.column, plot.points = points,
                     areas = areas.pt, legend = legend, log = log, years= NA)
  })

  output$rankData <- DT::renderDataTable({

    w.use <- w.use()
    data.elements <- input$data.elements
    areas.rd <- df[["areas"]]#input$area
    area.column <- df[["area.column"]]
    yearRange <- unique(w.use$YEAR)
    w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas.rd)

    df <- spread_(w.use.sub, "YEAR", data.elements)
    df <- df[,colSums(is.na(df))<nrow(df)]

    rankData <- DT::datatable(df, rownames = FALSE,
                              options = list(scrollX = TRUE,
                                             pageLength = nrow(df),
                                             order=list(list(2,'desc'))))
    yearRange <- names(df)[-1]
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
  # 
  # output$downloadPlotTwo <- downloadHandler(
  # 
  #   filename = function() {
  #     "plotTwo.png"
  #   },
  #   content = function(file) {
  #     file.copy("plotTwo.png", file)
  #   }
  # )
  # 
  # output$plotTwoCode <- renderPrint({
  # 
  #   data.elements <- input$data.elements
  #   areas.ptC <- df[["areas"]]#input$area
  # 
  #   areasOptions <- areasOptions()
  # 
  #   if(all(areasOptions %in% areas.ptC)){
  #     areas.ptC <- NA
  #   } else {
  #     areas.ptC <- paste0('c("',paste(areas.ptC, collapse = '","'),'")')
  #   }
  # 
  #   area.column <- input$area.column
  #   year.x.y <- c(input$year_x,input$year_y)
  # 
  #   outText <- paste0(
  #     'data.elements <- "',data.elements, '"\n',
  #     "areas <- ", areas.ptC, "\n",
  #     'area.column <- "', area.column, '"\n',
  #     "year.x.y <- c(",paste0(year.x.y,collapse = ","),")\n",
  #     "compare_two_years(w.use, data.elements, year.x.y, area.column, areas)"
  # 
  #   )
  # 
  #   HTML(outText)
  # 
  # })
  # 
  # output$plotTimeCode <- renderPrint({
  # 
  #   data.elements <- input$data.elements
  #   areas.pTC <- df[["areas"]]#input$area
  # 
  #   areasOptions <- areasOptions()
  # 
  #   if(all(areasOptions %in% areas.pTC)){
  #     areas.pTC <- NA
  #   } else {
  #     areas.pTC <- paste0('c("',paste(areas.pTC, collapse = '","'),'")')
  #   }
  # 
  #   area.column <- input$area.column
  #   legend <- input$legendOn
  #   log <- input$log
  # 
  #   outText <- paste0(
  #     'data.elements <- "',data.elements, '"\n',
  #     "areas <- ",areas.pTC, "\n",
  #     'area.column <- "', area.column, '"\n',
  #     'legend <- ',legend,"\n",
  #     'log <- ',log,"\n",
  #     "time_series_data(w.use, data.elements, area.column = area.column,\n",
  #     "areas = areas,log=log, legend=legend)"
  # 
  #   )
  # 
  #   HTML(outText)
  # 
  # })
  
})
