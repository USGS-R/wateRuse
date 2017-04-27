visualize.portfolio_time <- function(viz = as.viz("portfolio_time")){
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  
  deps <- readDepends(viz)
  
  height = viz[["height"]]
  width = viz[["width"]]
  
  viz.data <- deps[["aggregate_ga"]]
  
  ave_time_on_page <- mean(viz.data$avgSessionDuration, na.rm = TRUE)
  
  hours <- floor(ave_time_on_page/60/60)
  minutes <- floor(ave_time_on_page/60) - hours*60
  seconds <- floor(ave_time_on_page) - hours*60 - minutes*60
  
  hours <- zeroPad(as.character(hours),2)
  minutes <- zeroPad(as.character(minutes),2)
  seconds <- zeroPad(as.character(seconds),2)
  
  df <- data.frame(hours = hours,
                   minutes = minutes,
                   seconds = seconds,
                   stringsAsFactors = FALSE,
                   row.names = NULL)
  
  png(viz[["location"]], height=height, width=width)
    p<-tableGrob(df, rows = NULL)
    grid.arrange(p)
  dev.off()
  
}

zeroPad <- function(x,padTo){
  if(padTo <= 1) return(x)
  
  numDigits <- nchar(x, keepNA = TRUE)
  padding <- padTo-numDigits
  
  if(any(is.na(padding))){
    padding[is.na(padding)] <- 0
  }
  
  paddingZeros <- vapply(
    X = padding[padding > 0], 
    FUN = function(y) paste0(rep("0",y),collapse=""),
    FUN.VALUE = ""
  )
  
  x[padding > 0] <- paste0(paddingZeros,x[padding > 0])
  return(x)
}

visualize.app_time <- function(viz = as.viz("app_time")){
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  
  deps <- readDepends(viz)
  
  height = viz[["height"]]
  width = viz[["width"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  plot_type <- viz[["plottype"]]
  range_text <- viz[["rangetext"]]
  
  range_days = seq(Sys.Date(), length = 2, by = range_text)
  
  viz.data <- deps[["aggregate_ga"]]
  
  for(i in unique(viz.data$viewID)){
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    sub_data <- filter(viz.data, viewID == i)
    
    sub_data_range <- sub_data %>%
      filter(date >= range_days[2])
    
    ave_time_on_page <- mean(sub_data_range$avgSessionDuration, na.rm = TRUE)
    hours <- floor(ave_time_on_page/60/60)
    minutes <- floor(ave_time_on_page/60) - hours*60
    seconds <- floor(ave_time_on_page) - hours*60 - minutes*60
    
    hours <- zeroPad(as.character(hours),2)
    minutes <- zeroPad(as.character(minutes),2)
    seconds <- zeroPad(as.character(seconds),2)
    
    df <- data.frame(hours = hours,
                     minutes = minutes,
                     seconds = seconds,
                     stringsAsFactors = FALSE,
                     row.names = NULL)
    
    png(location, height=height, width=width)
    p<-tableGrob(df, rows = NULL)
    grid.arrange(p)
    dev.off()
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE)) 
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}
