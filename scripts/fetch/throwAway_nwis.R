#one-off to chunk the big NWIS call in case it fails midway

starts <- seq(ymd('2011-01-01'), ymd("2017-01-01"), by = "years")
ends <- seq(ymd('2012-01-01'), ymd("2018-01-01"), by = "years")
library(dplyr)
library(googleAnalyticsR)
library(data.table)
new_GA_DF <- data.frame()
for(i in 1:length(starts)) {
  ga_auth()
  dateRange = c(starts[i], ends[i])
  idDF <- google_analytics_4(viewId ="49785472", date_range = dateRange, 
                             metrics = c("sessions", "users", 'newUsers', 'avgSessionDuration'), 
                             dimensions = c("year","month", "day", "hour",
                                            "deviceCategory", 'region', 'source'), 
                             max = -1, anti_sample = TRUE)
  idDF <- mutate(idDF, viewID = "49785472")
  new_GA_DF <- bind_rows(new_GA_DF, idDF)
  fwrite(x = new_GA_DF, file = "nwisChunk.csv", 
         quote = TRUE, row.names = FALSE)
}