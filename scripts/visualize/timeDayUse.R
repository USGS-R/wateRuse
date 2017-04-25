#portfolio-wide
library(dplyr)
visualize.timeDayUse_all <- function(viz=as.viz("timeDayUse_port")) {
  
  viz.data <- readDepends(viz)[["aggregate_ga"]] #not sure which
  hourSum <- group_by(viz.data, hour) %>% summarise(n = n()) #need to set to numeric?
  height = viz[["height"]]
  width = viz[["width"]]
  
  png(viz[["location"]], height = height, width=width)
  
  par(oma = c(0,0,0,0),
      mgp = c(3,0.5,0),
      mar = c(4,6,2,0.1),
      tck = -0.01,
      las=1)

  barplot(hourSum$n, names.arg = hourSum$hour,
          xlab = "Hour of Day",ylab = "Sessions",
          main = "Sum total per hour for all apps",
          col = "steelblue")

  dev.off()
}

visualize.timeDayUse_app <- function(viz=as.viz("timeDayUse_app")) {
  
  viz.data <- readDepends(viz)[["aggregate_ga"]]
  
  height = viz[["height"]]
  width = viz[["width"]]
  plot_type <- viz[["plottype"]]
  range_text <- viz[["rangetext"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  range_days = rev(seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text))
  
  for(i in unique(viz.data$viewID)) {
    location <- paste0("cache/visualize/timeDayUse_", i, ".png")
    
    hourSum <- filter(viz.data, viewID == i) %>% 
      filter(date >= range_days[1]) %>%
      group_by(hour) %>% 
      summarise(n = n()) #need to set to numeric?

    png(location, height = height, width = width)

    par(oma = c(0,0,0,0),
        mgp = c(3,0.5,0),
        mar = c(4,6,2,0.1),
        tck = -0.01,
        las=1)
    
    barplot(hourSum$n, names.arg = hourSum$hour,
            xlab = "Hour of Day",ylab = "Sessions",
            main = paste("From",paste(range_days,collapse = " to ")),
            col = "steelblue")
    dev.off()
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))    
  }
  write.csv(x, file=viz[["location"]], row.names = FALSE)
}
