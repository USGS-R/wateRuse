library(dplyr)
library(DT)
library(wateRuse)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

w.use.start <- wUseSample

data.elements.type <- category$CODE
names(data.elements.type) <- category$NAME

data.elements <- gsub("-", ".", dataelement$DATAELEMENT)
data.elements <- data.elements[which(dataelement$CATEGORYCODE == data.elements.type[1])]
data.elements.start <- data.elements[data.elements %in% names(w.use.start)]

data.total.elements <- calculation$CALCULATION[grep(pattern = "WTotl", calculation$CALCULATION)]
names(data.total.elements) <- calculation$CATEGORYCODE[grep(pattern = "WTotl", calculation$CALCULATION)]
data.total.elements <- gsub("-", ".", data.total.elements)

options(shiny.maxRequestSize=50*1024^2) 
area.names <- c("STATECOUNTYCODE","COUNTYNAME",
                    "HUCCODE","Area","USSTATEHUCCODE","HUCNAME")
other.names <- c("STUDY","STATECODE","COUNTYCODE",
                 "YEAR","USSTATEALPHACODE","DATASETNAME","BESTAVAILABLE")

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

    w.use <- caluculate_values(w.use)
    
    w.use
  })
  
  w.use <- reactive({
    w.use <- w.use_full()
    states <- df[["state"]]
    
    if(!is.null(w.use) && ("USSTATEALPHACODE" %in% names(w.use) | states != "All Available")){
      w.use <- filter(w.use, USSTATEALPHACODE %in% states)
    }
    
    data.elements.full <- gsub("-", ".", dataelement$DATAELEMENT)
    data.elements <- data.elements.full[which(dataelement$CATEGORYCODE == input$data.elements.type)]
    data.elements.y <- data.elements.full[which(dataelement$CATEGORYCODE == input$data.elements.type.max)]

    data.total.elements <- calculation$CALCULATION[grep(pattern = "WTotl", calculation$CALCULATION)]
    data.total.elements <- gsub("-", ".", data.total.elements)
    
    
    df[["data.elements"]] <- data.elements[data.elements %in% names(w.use)]
    df[["data.element"]] <- data.elements[data.elements %in% names(w.use)][1]

    df[["data.elements.y"]] <- data.elements.y[data.elements.y %in% names(w.use)]
    df[["data.element.y"]] <- data.elements.y[data.elements.y %in% names(w.use)][1]

    df[["data.total.elements"]] <- data.total.elements[data.total.elements %in% names(w.use)]
    df[["data.total.element"]] <- data.total.elements[data.total.elements %in% names(w.use)][1]
    
    w.use
    
  })
  
  df <- reactiveValues(area.column="COUNTYNAME",
                       area.columns=c("STATECOUNTYCODE","COUNTYNAME"),
                       areas = unique(w.use.start[["COUNTYNAME"]]),
                       area = unique(w.use.start[["COUNTYNAME"]]),
                       states = unique(w.use.start[["USSTATEALPHACODE"]]),
                       state = unique(w.use.start[["USSTATEALPHACODE"]])[1],
                       data.elements = data.elements.start,
                       data.element = data.elements.start[1],
                       data.elements.y = data.elements.start,
                       data.element.y = data.elements.start[1],
                       data.total.elements = data.total.elements,
                       data.total.element = data.total.elements[1])
  
  observeEvent(input$data, ignoreNULL = TRUE, {
    w.use <- w.use_full()
    
    if(!is.null(w.use) && "USSTATEALPHACODE" %in% names(w.use)){
      choices <- unique(w.use$USSTATEALPHACODE)
    } else {
      choices <- "All Available"
    }
    
    choice.area <- names(w.use)[names(w.use) %in% area.names]

    df[["states"]] <- choices
    df[["state"]] <- choices[1]
    df[["area.column"]] <- choice.area[1]
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
    
    data.elements.y <- data.elements.full[which(dataelement$CATEGORYCODE == input$data.elements.type.max)]
    
    df[["data.elements"]] <- data.elements[data.elements %in% names(w.use)]
    df[["data.element"]] <- data.elements[data.elements %in% names(w.use)][1]
    df[["data.elements.y"]] <- data.elements.y[data.elements.y %in% names(w.use)]
    df[["data.element.y"]] <- data.elements.y[data.elements.y %in% names(w.use)][1]
    
    data.total.elements <- calculation$CALCULATION[grep(pattern = "WTotl", calculation$CALCULATION)]
    data.total.elements <- gsub("-",".", data.total.elements)
    df[["data.total.elements"]] <- data.total.elements[data.total.elements %in% names(w.use)]
    df[["data.total.element"]] <- data.total.elements[data.total.elements %in% names(w.use)][1]
    
  })
  
  observeEvent(input$changeArea,  {
    df[["area"]] <- input$area
  })
  
  observeEvent(input$deselectArea,  {
    updateCheckboxGroupInput(session, "area", 
                             choices =  df[["areas"]], 
                             selected =  df[["areas"]][1])
  })
  
  observeEvent(input$changeTotals,  {
    df[["data.total.element"]] <- input$data.total.elements
  })
  
  observeEvent(input$data.elements,  {
    df[["data.element"]] <- input$data.elements
  })

  observeEvent(input$data.elements.max,  {
    df[["data.element.y"]] <- input$data.elements.max
  })
  
  observeEvent(input$norm.element,  {
    df[["data.element.y"]] <- input$norm.element
  })
  
  observeEvent(input$data.elements.type,  {
    
    data.elements <- gsub("-", ".", dataelement$DATAELEMENT)
    data.elements <- data.elements[which(dataelement$CATEGORYCODE == input$data.elements.type)]
      
    w.use_full <- w.use_full()
    
    data.elements <- data.elements[which(data.elements %in% names(w.use_full))]
    
    df[["data.elements"]] <- data.elements
    df[["data.element"]] <- data.elements[1]
      
  })
  
  observeEvent(input$data.elements.type.max,  {
    
    data.elements <- gsub("-", ".", dataelement$DATAELEMENT)
    data.elements <- data.elements[which(dataelement$CATEGORYCODE == input$data.elements.type.max)]
    
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
    
    df[["data.elements.y"]] <- data.elements
    df[["data.element.y"]] <- data.elements[1]
    
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
    
    data.elements.y <- data.elements.full[which(dataelement$CATEGORYCODE == input$data.elements.type.max)]
    data.elements.y <- data.elements.y[which(data.elements.y %in% names(w.use))]
    
    df[["data.elements"]] <- data.elements
    df[["data.element"]] <- data.elements[1]
    df[["data.elements.y"]] <- data.elements.y
    df[["data.element.y"]] <- data.elements.y[1]
    
  })
  
  observe({
    updateCheckboxGroupInput(session, "area", 
                             choices =  df[["areas"]], 
                             selected =  df[["area"]])
  })
  
  observe({
    updateCheckboxGroupInput(session, "data.total.elements", 
                             choices =  df[["data.total.elements"]], 
                             selected =  df[["data.total.element"]])
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
    
    fancy.names <- df[["data.elements.y"]]
    names(fancy.names) <- dataelement$NAME[which(gsub("-",".",dataelement$DATAELEMENT) %in% fancy.names)]
    
    fancy.names.single <- df[["data.element.y"]]
    names(fancy.names.single) <- dataelement$NAME[which(gsub("-",".",dataelement$DATAELEMENT) %in% fancy.names.single)]
    
    updateSelectInput(session, "data.elements.max",
                      choices = fancy.names,
                      selected = fancy.names[1])
  })
  
  observe({
    
    fancy.names <- df[["data.elements.y"]]
    names(fancy.names) <- dataelement$NAME[which(gsub("-",".",dataelement$DATAELEMENT) %in% fancy.names)]
    
    fancy.names.single <- df[["data.element.y"]]
    names(fancy.names.single) <- dataelement$NAME[which(gsub("-",".",dataelement$DATAELEMENT) %in% fancy.names.single)]
    
    updateSelectInput(session, "norm.element",
                      choices = c("None",fancy.names),
                      selected = "None")
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
  
})
