visualize.viz_source <- function(viz = as.viz("viz_source")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["source_counts"]]
  height = viz[["height"]]
  width = viz[["width"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  plot_type <- viz[["plottype"]]

  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    source_sum <- data.frame(table(sub_data$source),
                             stringsAsFactors = FALSE) %>%
      arrange(desc(Freq))
    
    source_sum <- source_sum[1:min(c(5, nrow(source_sum))),]
    max_char = max(nchar(as.character(source_sum$Var1)), na.rm = TRUE)
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    png(location, height = height, width = width)
    
    par(oma = c(0,0,0,0),
        mgp = c(3,0.5,0),
        mar = c(2,(max_char-3)/2,0.1,0.1),
        tck = -0.01,
        las=1)
    
    if(nrow(source_sum) > 0){
      barplot(rev(source_sum$Freq), horiz=TRUE,
              names.arg=rev(source_sum$Var1))
      
    } else {
      barplot(c(0,0), horiz=TRUE,
              names.arg=c("google","(direct)"),
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
