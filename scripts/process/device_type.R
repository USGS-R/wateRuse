process.device_type <- function(viz = as.viz("device_type")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["aggregate_ga"]]
  
  device_users <- select(viz.data, date, viewID,deviceCategory) 
  
  saveRDS(device_users, file=viz[["location"]], compress = FALSE)
  
}
