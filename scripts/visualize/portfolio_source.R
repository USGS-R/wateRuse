visualize.portfolio_source <- function(viz = as.viz("portfolio_source")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["source_counts"]]
  height = viz[["height"]]
  width = viz[["width"]]
  
  source_sum <- viz.data %>%
    group_by(source) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  source_sum <- source_sum[1:min(c(10, nrow(source_sum))),]
  
  max_char = max(nchar(source_sum$source), na.rm = TRUE)
  
  png(viz[["location"]], height = height, width = width)  
  
  par(oma = c(0,0,0,0),
      mgp = c(3,0.5,0),
      mar = c(2,(max_char-3)/2,0.1,0.1),
      tck = -0.01,
      las=1)
  if(nrow(source_sum) > 0){
    barplot(rev(source_sum$count), horiz=TRUE,
            names.arg=rev(source_sum$source))
  } else {
    barplot(c(0,0), horiz=TRUE,
            names.arg=c("google","(direct)"),
            xlim = c(0,10))
  }
  dev.off()      

}
