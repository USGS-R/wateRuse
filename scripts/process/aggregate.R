process.aggregate_ga <- function(viz) {
  library(dplyr)
  library(data.table)
  library(lubridate)
  viz.data <- readDepends(viz) #list of all depends
  allDataDF <- do.call("bind_rows", viz.data)
  
  #drop data before longer than a year ago
  allDataDF <- allDataDF %>% 
    mutate(date = as.Date(date)) %>% 
    filter(date > (max(date) - duration(1, "year"))) %>%
    distinct()
  
  #add dateTime
  saveRDS(object = allDataDF, file=viz[["location"]], compress = FALSE)
}