visualize.viz_y_sessions <- function(viz){
  library(dplyr)
  library(ggplot2)
  
  height = viz[["height"]]
  width = viz[["width"]]
  
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  range_text <- viz[["rangetext"]]
  plot_type <- viz[["plottype"]]
  
  max_date <- max(viz.data$date, na.rm = TRUE)
  
  year_days = seq(max_date, length = 2, by = range_text)
  full_dates <- seq(max_date, length = -as.numeric(diff(year_days))+1, by=-1)
  empty_df <- data.frame(date = full_dates)
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    sub_data_year <- select(sub_data, date, sessions) %>%
      filter(date >= year_days[2],
             date <= year_days[1]) %>%
      group_by(date) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
      data.frame() %>%
      right_join(empty_df, by="date") %>%
      arrange(desc(date))
    
    sub_data_year$sessions[is.na(sub_data_year$sessions)] <- 0
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    png(location, height = height, width = width, res = 150)
    
    par(oma=c(0,0,0,0),
        mar=c(1.5,2.5,1,1),
        las=1, 
        mgp = c(1,0.3,0),
        tck=0.02)
    
    if(nrow(sub_data_year) > 0){
      type <- ifelse(range_text == "-1 week", "b", "l")
      plot(x = sub_data_year$date, 
           sub_data_year$sessions, 
           type=type,xlab="",ylab="", yaxt='n', ylim = c(0, max(sub_data_year$sessions, na.rm = TRUE)),
           frame.plot = FALSE)

    } else {
      plot(1, axes=FALSE, 
           type="n",xlab="",ylab="")
    }
    
    last.tick <- tail(pretty(c(0, max(sub_data_year$sessions, na.rm = TRUE))),2)[1]  
    axis(1, at=c(par()$usr[1],par()$usr[2]), 
         labels = c("",""), lwd.tick=0)
    axis(2, at=c(-last.tick, 0, last.tick, last.tick*2), 
         labels = c("","0", pretty_num(last.tick), ""))
    par(xpd = NA)
    text(par('usr')[1], par('usr')[4]*1.04, 
         labels = paste0(range(sub_data_year$date), collapse = " to "), pos = 4)
    dev.off()
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      

  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)

}

visualize.viz_d_sessions <- function(viz){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users_daily"]]
  
  height = viz[["height"]]
  width = viz[["width"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  plot_type <- viz[["plottype"]]
  
  max_date <- max(viz.data$dateTime, na.rm = TRUE)
  full_dates <- seq(max_date, length = 24, by=-60*60)
  empty_df <- data.frame(dateTime=full_dates)
  
  for(i in unique(viz.data$viewID)){
    
    sub_data <- select(viz.data, dateTime, viewID, sessions) %>%
      filter(viewID == i) %>%
      group_by(dateTime) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
      data.frame() %>%
      right_join(empty_df, by="dateTime") %>%
      arrange(desc(dateTime))
    
    sub_data$sessions[is.na(sub_data$sessions)] <- 0
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    png(location, width = width, height = height, res = 150)
    par(oma=c(0,0,0,0),
        mar=c(1.5,2.5,1,1),
        las=1, 
        mgp = c(1,0.3,0),
        tck=0.02)
    
    if(nrow(sub_data) > 0){
      plot(x = sub_data$dateTime, 
           sub_data$sessions, 
           type="b",xlab="",ylab="",yaxt='n',
           frame.plot = FALSE)
    } else { 
      plot(1, axes=FALSE, 
           type="n",xlab="",ylab="")
    }
    last.tick <- tail(pretty(c(0, max(sub_data$sessions, na.rm = TRUE))),2)[1]  
    axis(1, at=c(par()$usr[1],par()$usr[2]), 
         labels = c("",""), lwd.tick=0)
    axis(2, at=c(-last.tick, 0, last.tick, last.tick*2), 
         labels = c("","0", pretty_num(last.tick), ""))
    par(xpd = NA)
    text(par('usr')[1], par('usr')[4]*1.04, 
         labels = paste0(range(sub_data$date), collapse = " to "), pos = 4)
    dev.off()
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}

# couldn't remember how to get it to source another script, so re-using this form portfolio sessions
pretty_num <- function(n){
  if (is.na(n)) n = 0
  if (n > 1e7){
    out <- sprintf('%1.0fM', n/1e6)
  } else if (n > 1e6){
    out <- sprintf('%1.1fM', n/1e6)
  } else if (n > 1e4){
    out <- sprintf('%1.0fK', n/1e3)
  } else if (n > 1000){
    out <- sprintf('%1.1fK', n/1e3)
  } else {
    out <- as.character(n)
  }
  return(out)
}