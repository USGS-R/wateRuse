process.sessions_and_new_users <- function(viz = as.viz("sessions_and_new_users")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["aggregate_ga"]]
  
  session_users <- select(viz.data, date,viewID,sessions,newUsers)
  
  saveRDS(session_users, file=viz[["location"]], compress = FALSE)
  
}
