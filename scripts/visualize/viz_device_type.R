visualize.viz_device_type <- function(viz = as.viz("viz_device_type")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["device_type"]]
  height = viz[["height"]]
  width = viz[["width"]]
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  plot_type <- viz[["plottype"]]
  range_text <- viz[["rangetext"]]
  
  range_days = seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text)
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    sub_data_range <- sub_data %>%
      filter(date >= range_days[2]) %>%
      select(-date) %>%
      group_by(deviceCategory) %>%
      summarize(totals = n())
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    png(location, height = height, width = width)
    
    max_char = max(nchar(sub_data_range$deviceCategory), na.rm = TRUE)
    
    par(oma = c(0,0,0,0),
        mgp = c(3,0.5,0),
        mar = c(2,(max_char)/2,0.1,0.1),
        tck = -0.01,
        las=1)
    
    if(nrow(sub_data_range) > 0){
      barplot(rev(sub_data_range$totals), horiz=TRUE,
              names.arg=rev(sub_data_range$deviceCategory))
    } else {
      barplot(c(0,0,0), horiz=TRUE,
              names.arg=c("desktop","mobile","tablet"),
              xlim = c(0,10))    
    }
    
    dev.off()
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}
