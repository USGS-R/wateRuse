visualize.portfolio_sessions <- function(viz){
  library(dplyr)
  
  deps <- readDepends(viz)
  height = viz[["height"]]
  width = viz[["width"]]
  viz.data <- deps[["sessions_and_new_users"]]
  ga_table <- deps[["project_table"]] 
  ga_table$viewID <- as.character(ga_table$viewID)
  
  range_text <- viz[["rangetext"]]
  
  range_days = seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text)
  
  summary_sessions <- viz.data %>%
    filter(date >= range_days[2]) %>%
    group_by(viewID) %>%
    summarize(newUsers = sum(newUsers, na.rm = TRUE),
              sessions = sum(sessions, na.rm = TRUE)) %>%
    mutate(oldUsers = sessions - newUsers) %>%
    arrange(sessions) %>%
    left_join(select(ga_table, viewID, shortName), by="viewID")
  
  dater <- t(as.matrix(summary_sessions[,c("newUsers", "oldUsers")]))
  
  png(viz[["location"]], height = height, width = width)
    par(las=1, oma=c(0,0,0,0))
    barplot(dater, horiz = TRUE,
            names.arg = summary_sessions$shortName)
  dev.off()
  
}
