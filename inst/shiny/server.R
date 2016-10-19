library(dplyr)
library(DT)
library(wateRuse)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(plotly)

w.use.start <- wUseSample

data.elements.type <- category$CODE
names(data.elements.type) <- category$NAME

data.elements <- gsub("-", ".", dataelement$DATAELEMENT)
data.elements <- data.elements[which(dataelement$CATEGORYCODE == data.elements.type[1])]
data.elements.start <- data.elements#[data.elements %in% names(w.use.start)]

options(shiny.maxRequestSize=50*1024^2) 
area.names <- c("STATECOUNTYCODE","COUNTYNAME",
                    "HUCCODE","Area","USSTATEHUCCODE","HUCNAME")
other.names <- c("STUDY","STATECODE","COUNTYCODE",
                 "YEAR","USSTATEALPHACODE","DATASETNAME","BESTAVAILABLE")

shinyServer(function(input, output, session) {
  
  w.use_start <- reactive({
    
    if(!is.null(input$data)){
      
      path <- file.path(input$data$datapath)
      newPath <- paste0(input$data$datapath,"_",input$data$name)
      newPath <- gsub(", ","_",newPath)
      file.rename(from = path, to = newPath)
      w.use <- get_awuds_data(awuds.data.files = newPath)
    } else {
      
      w.use <- w.use.start
      w.use <- w.use[-1:-nrow(w.use.start),]
    }

    w.use
  })
  
  w.use_append <- reactive({
    
    if(!is.null(input$data.sup)){
      
      path <- file.path(input$data.sup$datapath)
      newPath <- paste0(input$data.sup$datapath,"_",input$data.sup$name)
      newPath <- gsub(", ","_",newPath)
      file.rename(from = path, to = newPath)
      w.use <- get_awuds_data(awuds.data.files = newPath)
    } else {
      
      w.use <- w.use_start()
      w.use <- w.use[-1:-nrow(w.use),]
    }
    
    w.use
  })
  
  w.use_full <- reactive({
    w.use <- w.use_start()
    w.use_append <- w.use_append()
    
    if(nrow(w.use_append) > 0){
      w.use_append$YEAR <- paste0(w.use_append$YEAR,"_append")
      w.use <- full_join(w.use, w.use_append)
    }
    
    if("DATASETNAME" %in% names(w.use) & nrow(w.use) > 0){
      w.use$YEAR <- paste0(w.use$YEAR,"_",w.use$DATASETNAME)
    }
    
    w.use <- calculate_values(w.use)
    
    w.use
  })
  
  w.use_barSum <- reactive({
    w.use <- w.use_start()
    
    w.use[is.na(w.use)] <- 0
    w.use <- calculate_values(w.use)

    totals <- c("PS.WTotl","DO.WTotl","IN.WTotl", "PT.WTotl", 
      "MI.WTotl", "LS.WTotl", "AQ.WTotl","IT.WTotl","TP.TotPop","TO.WGWTo", "TO.WSWTo")
    
    if("USSTATEALPHACODE" %in% names(w.use)){
      w.use_state <- w.use[,c("USSTATEALPHACODE", "YEAR",totals)] %>%
        group_by(USSTATEALPHACODE, YEAR) %>%
        summarise_each(funs(sum))
    } else {
      w.use_state <- w.use[,c("YEAR",totals)] %>%
        mutate(USSTATEALPHACODE = "01") %>%
        group_by(USSTATEALPHACODE, YEAR) %>%
        summarise_each(funs(sum))
    }
    
    w.use_state
  })
  
  w.use <- reactive({
    w.use <- w.use_full()
    states <- df[["state"]]
    
    if(!is.null(w.use) && ("USSTATEALPHACODE" %in% names(w.use) | states != "All Available")){
      w.use <- filter(w.use, USSTATEALPHACODE %in% states)
    }
    
    validate(
      need(input$bestAvail, 'Choose Best Available, Work-In-Progress, or Both')
    )
    
    if("BESTAVAILABLE" %in% names(w.use) & length(input$bestAvail) == 1){
      if(input$bestAvail == "Best"){
        w.use <- filter(w.use, BESTAVAILABLE == "yes")
      } else {
        w.use <- filter(w.use, BESTAVAILABLE == "no")
      }
    }
    
    data.elements.full <- gsub("-", ".", dataelement$DATAELEMENT)
    data.elements <- data.elements.full[which(dataelement$CATEGORYCODE == input$data.elements.type)]
    data.elements.y <- data.elements.full[which(dataelement$CATEGORYCODE == input$data.elements.type.y)]
    
    df[["data.elements"]] <- data.elements[data.elements %in% names(w.use)]
    df[["data.element"]] <- data.elements[data.elements %in% names(w.use)][1]

    df[["data.elements.y"]] <- data.elements.y[data.elements.y %in% names(w.use)]
    df[["data.element.y"]] <- data.elements.y[data.elements.y %in% names(w.use)][1]

    w.use
    
  })

  df <- reactiveValues(area.column="COUNTYNAME",
                       area.columns=c("STATECOUNTYCODE","COUNTYNAME"),
                       areas = "Choose Data",
                       area = "Choose Data",
                       states = "All Available",
                       state = "All Available",
                       data.elements = data.elements.start,
                       data.element = data.elements.start[1],
                       data.elements.y = data.elements.start,
                       data.element.y = data.elements.start[1],
                       data.elements.norm = c("None",data.elements.start),
                       data.element.norm = "None")
  
  observeEvent(input$data, ignoreNULL = TRUE, {
    w.use <- w.use_full()
    
    if(!is.null(w.use) && "USSTATEALPHACODE" %in% names(w.use)){
      choices <- unique(w.use$USSTATEALPHACODE)
      choices <- choices[order(choices)]
    } else {
      choices <- "All Available"
    }
    
    choice.area <- names(w.use)[names(w.use) %in% area.names]

    df[["states"]] <- choices
    df[["state"]] <- ifelse("AK" %in% choices & length(choices) > 1, choices[2], choices[1])
    df[["area.column"]] <- ifelse("COUNTYNAME" %in% choice.area, "COUNTYNAME", choice.area[1])
    df[["area.columns"]] <- choice.area
    
    if(!is.null(w.use) && "USSTATEALPHACODE" %in% names(w.use)){
      w.use <- w.use[w.use$USSTATEALPHACODE == choices[1],]
    } 
    
    df[["area"]] <- unique(w.use[[df[["area.column"]]]] )
    df[["areas"]] <- unique(w.use[[df[["area.column"]]]] )
    
    w.use <- w.use[w.use[[df[["area.column"]]]] %in% df[["area"]],]
    
    w.use <- w.use[,colSums(is.na(w.use)) < nrow(w.use)]

    data.elements.full <- gsub("-", ".", dataelement$DATAELEMENT)
    data.elements <- data.elements.full[which(dataelement$CATEGORYCODE == input$data.elements.type)]
    
    data.elements.y <- data.elements.full[which(dataelement$CATEGORYCODE == input$data.elements.type.y)]
    
    df[["data.elements"]] <- data.elements[data.elements %in% names(w.use)]
    df[["data.element"]] <- data.elements[data.elements %in% names(w.use)][1]
    df[["data.elements.y"]] <- data.elements.y[data.elements.y %in% names(w.use)]
    df[["data.element.y"]] <- data.elements.y[data.elements.y %in% names(w.use)][1]
    df[["data.elements.norm"]] <- c("None",data.elements.y[data.elements.y %in% names(w.use)])
    df[["data.element.norm"]] <- "None"

  })
  
  observeEvent(input$changeArea,  {
    
    validate(
      need(input$state, 'Choose a State'),
      need(input$area, 'Choose an Area')
    )
    
    df[["area"]] <- input$area
  })
  
  observe({

    hucLogic <- "HUCCODE" %in% names(w.use_full)
    
    updateCheckboxInput(session, "unitTypeHUC", value = hucLogic)
  })
  
  observe({
    
    w.use <- w.use()
    years <- unique(w.use$YEAR)
    years <- years[order(years)]
    
    updateCheckboxGroupInput(session, "whatYears", choices = years, selected = years)
  })
 
  observeEvent(input$deselectArea, {
           
    data.elements.type
        
    updateCheckboxGroupInput(session, "area", 
                             choices =  df[["areas"]], 
                             selected =  df[["areas"]][1])
    
  })
   
  observeEvent(input$selectArea,  {
    validate(
      need(input$state, 'Choose a State'),
      need(input$area, 'Choose an Area')
    )
    
    updateCheckboxGroupInput(session, "area", 
                             choices =  df[["areas"]], 
                             selected =  df[["areas"]])
  })

  observeEvent(input$data.elements,  {
    df[["data.element"]] <- input$data.elements
  })

  observeEvent(input$data.elements.y,  {
    df[["data.element.y"]] <- input$data.elements.y
  })
  
  observeEvent(input$norm.element,  {
    df[["data.element.norm"]] <- input$norm.element
  })
  
  observeEvent(input$data.elements.type,  {
    
    data.elements <- gsub("-", ".", dataelement$DATAELEMENT)
    data.elements <- data.elements[which(dataelement$CATEGORYCODE == input$data.elements.type)]
      
    w.use_full <- w.use_full()
    
    data.elements <- data.elements[which(data.elements %in% names(w.use_full))]
    
    df[["data.elements"]] <- data.elements
    df[["data.element"]] <- data.elements[1]
      
  })
  
  observeEvent(input$data.elements.type.y,  {
    
    data.elements <- gsub("-", ".", dataelement$DATAELEMENT)
    data.elements <- data.elements[which(dataelement$CATEGORYCODE == input$data.elements.type.y)]
    
    w.use_full <- w.use_full()
    
    data.elements <- data.elements[which(data.elements %in% names(w.use_full))]
    
    df[["data.elements.y"]] <- data.elements
    df[["data.element.y"]] <- data.elements[1]
    
  })
  
  observeEvent(input$norm.element.type,  {
    
    data.elements <- gsub("-", ".", dataelement$DATAELEMENT)
    data.elements <- data.elements[which(dataelement$CATEGORYCODE == input$norm.element.type)]
    
    w.use_full <- w.use_full()
    
    data.elements <- data.elements[which(data.elements %in% names(w.use_full))]
    
    df[["data.elements.norm"]] <- c("None",data.elements)
    df[["data.element.norm"]] <- "None"
    
  })
  
  observeEvent(input$area.column,  {
    df[["area.column"]] <- input$area.column
    
    w.use <- w.use_full()
    states <- df[["state"]]
    
    if(!is.null(w.use) && ("USSTATEALPHACODE" %in% names(w.use) | any(states != "All Available"))){
      w.use <- filter(w.use, USSTATEALPHACODE %in% states)
    }
    
    df[["areas"]] <- unique(w.use[[input$area.column]])
    df[["area"]] <- unique(w.use[[input$area.column]])
  })

  observeEvent(input$state, ignoreNULL = TRUE, {

    w.use <- w.use_full()
    validate(
      need(input$state, 'Choose a State'),
      need(input$area, 'Choose an Area')
    )
    
    df[["state"]] <- input$state
    area.column <- df[["area.column"]]
    
    if(!is.null(w.use) && ("USSTATEALPHACODE" %in% names(w.use) | df[["state"]] != "All Available")){
      w.use <- filter(w.use, USSTATEALPHACODE %in% df[["state"]])
    }
    df[["areas"]] <- unique(w.use[[area.column]])
    df[["area"]] <- unique(w.use[[area.column]])
    
    data.elements.full <- gsub("-", ".", dataelement$DATAELEMENT)
    
    data.elements <- data.elements.full[which(dataelement$CATEGORYCODE == input$data.elements.type)]
    data.elements <- data.elements[which(data.elements %in% names(w.use))]
    
    data.elements.y <- data.elements.full[which(dataelement$CATEGORYCODE == input$data.elements.type.y)]
    data.elements.y <- data.elements.y[which(data.elements.y %in% names(w.use))]
    
    df[["data.elements"]] <- data.elements
    df[["data.element"]] <- data.elements[1]
    df[["data.elements.y"]] <- data.elements.y
    df[["data.element.y"]] <- data.elements.y[1]
    df[["data.elements.norm"]] <- c("None",data.elements.y)
    df[["data.element.norm"]] <- "None"
    
  })
  
  observe({
    updateCheckboxGroupInput(session, "area", 
                             choices =  df[["areas"]], 
                             selected =  df[["area"]])
  })

  observe({
    if(any(df[["state"]] %in% stateCd$STUSAB)){
      
      state <- stateCd$STATE_NAME[which(stateCd$STUSAB %in% df[["state"]])[1]]
      
      updateCheckboxGroupInput(session, "stateToMap", selected = state)
    }
  })
  
  observe({
    updateSelectInput(session, "area.column", 
                             choices = df[["area.columns"]], 
                             selected = df[["area.column"]])
  })
  
  observe({

    fancy.names <- df[["data.elements"]]
    names(fancy.names) <- dataelement$NAME[which(gsub("-",".",dataelement$DATAELEMENT) %in% fancy.names)]
    
    fancy.names.single <- df[["data.element"]]
    names(fancy.names.single) <- dataelement$NAME[which(gsub("-",".",dataelement$DATAELEMENT) %in% fancy.names.single)]
    
    updateSelectInput(session, "data.elements",
                      choices = fancy.names,
                      selected = fancy.names.single)

  })
  
  observe({
    
    w.use <- w.use_full()
    category <- category
    dataelement <- dataelement
    
    cateCodes <- unique(dataelement$CATEGORYCODE[gsub("-",".",dataelement$DATAELEMENT) %in% names(w.use)])
    cateCodes <- left_join(data.frame(CODE=cateCodes, stringsAsFactors = FALSE), category)
    
    updateSelectInput(session, "data.elements.type",
                      choices = setNames(cateCodes$CODE, cateCodes$NAME),
                      selected = setNames(cateCodes$CODE, cateCodes$NAME)[1])
    
  })
  
  observe({
    
    fancy.names <- df[["data.elements.y"]]
    names(fancy.names) <- dataelement$NAME[which(gsub("-",".",dataelement$DATAELEMENT) %in% fancy.names)]
    
    fancy.names.single <- df[["data.element.y"]]
    names(fancy.names.single) <- dataelement$NAME[which(gsub("-",".",dataelement$DATAELEMENT) %in% fancy.names.single)]
    
    updateSelectInput(session, "data.elements.y",
                      choices = fancy.names,
                      selected = fancy.names.single)
  })
  
  observe({
    
    fancy.names <- df[["data.elements.norm"]]
    
    fancy.names.single <- df[["data.element.norm"]]
    
    updateSelectInput(session, "norm.element",
                      choices = fancy.names,
                      selected = fancy.names.single)
  })
  
  observe({
    choices <- df[["states"]]
    
    updateCheckboxGroupInput(session, "state",
                      choices = choices,
                      selected = choices[1])
  })
  
  observe({
    w.use <- w.use()
    data.elements <- df[["data.element"]]
    areas.yr <- df[["areas"]]
    area.column <- df[["area.column"]]

    w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas.yr)

    yRange <- unique(w.use.sub$YEAR[!is.na(w.use.sub[[data.elements]])])
    yRange <- yRange[order(yRange)]
    
    updateSelectInput(session, "year_x",
                      choices = yRange,
                      selected = yRange[length(yRange)-1])

    updateSelectInput(session, "year_y",
                      choices = yRange,
                      selected = yRange[length(yRange)])
  })

##################################################################

  source("plotTwo.R",local=TRUE)$value
  
###################################################################

################################################################### 
  
  source("plotTwoElement.R",local=TRUE)$value
  
##################################################################
  
    
###################################################################

  source("plotBarSums.R",local=TRUE)$value
  
###################################################################
  
###################################################################

  source("plotMultiElem.R",local=TRUE)$value
  
###################################################################

###################################################################
  
 source("plotBoxplots.R",local=TRUE)$value
  
###################################################################
  
###################################################################  

  source("plotTimeSeries.R",local=TRUE)$value
  
###################################################################
  
###################################################################

  source("rankData.R",local=TRUE)$value

###################################################################

  source("mapData.R",local=TRUE)$value
  
###################################################################
  
  output$boxLegend <- renderImage({

    filename <- normalizePath(file.path('./images',"boxPlot.jpg" ))

    list(src = filename,
         alt = "Boxplot legend")
    
  })
  
})
