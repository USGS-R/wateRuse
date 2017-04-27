#fetch Google Analytics data for all viewIDs supplied
library(googleAnalyticsR)
library(googleAuthR)
library(dplyr)
library(data.table)
library(sbtools)
library(assertthat)
fetch.GAviews <- function(viz) {
  #first get SB data - does it have data through yesterday? if no, and 
  #update is true, run the fetcher for intervening time, and append, upload
  
  if(!viz[['useLocal']]) {
    
    #get file from sb with fetcher
    viz[['fetcher']] <- 'sciencebase'
    viz[['remoteFilename']] <- basename(viz[['location']])
    message("Downloading SB file...")
    fetch(as.fetcher(viz))
    message('Downloaded SB file')
    
    if(viz[['update']]) {
      masterTable <- readDepends(viz)[['project_table']]
      masterTable <- filter(masterTable, login == viz[['login']])
      #check if it is up to date (has yesterday's data) for each ID
      fileDF <- readRDS(viz[['location']])
      fileDF_summary <- group_by(fileDF, viewID) %>% summarise(lastDate = max(date)) 
      
      #all views should have current data right?
      needToUpdate <- filter(fileDF_summary, lastDate < Sys.Date() - 1)
      #get out of date AND new IDs
      viewID <- masterTable$viewID[!masterTable$viewID %in% fileDF_summary$viewID]
      newIDs <- data.frame(viewID = viewID,
                           lastDate = as.Date(rep("2007-01-01", length(viewID))),#arbitrary early start date
                           stringsAsFactors = FALSE)  
      needToUpdate <- bind_rows(needToUpdate, newIDs)
      
      if(nrow(needToUpdate) > 0) {
        message("Sciencebase file is out of date, updating from GA")
        new_GA_DF <- data.frame()
        gar_auth_service(file.path(Sys.getenv("HOME"), ".vizlab/VIZLAB-a48f4107248c.json"))
        for(i in 1:nrow(needToUpdate)) { 
          #NOTE: only want to pull full days, so don't pull today's data!  
          #this way we can just append the new data without having overlap
          dateRange <- c(as.character(needToUpdate$lastDate[i]), as.character(Sys.Date() - 1 ))
          
          #API is limited to 7 dimensions per call 
          idDF <- google_analytics_4(viewId =needToUpdate$viewID[i], date_range = dateRange, 
                                     metrics = c("sessions", "users", 'newUsers', 'avgSessionDuration'), 
                                     dimensions = c("year","month", "day", "hour",
                                                    "deviceCategory", 'region', 'source'), 
                                     max = -1, anti_sample = TRUE)
          idDF <- mutate(idDF, viewID = needToUpdate$viewID[i],
                         date = as.Date(paste(year, month, day, sep = "-")))
          new_GA_DF <- bind_rows(new_GA_DF, idDF)
          #fwrite(new_GA_DF, file = paste0(viz[['location']], i))
          print(paste("finished", needToUpdate$viewID[i]))
        }
        
        allDF <- bind_rows(fileDF, new_GA_DF)
        maxDate <- max(allDF$date)
        assert_that(maxDate == (Sys.Date() - 1)) #note: is allDF$date char or date?
        
        saveRDS(allDF, file = viz[['location']])
        message("Updating Sciencebase...")
        item_replace_files(viz[['remoteItemId']], viz[['location']])
      } else {
        message("Sciencebase file is up to date, using that")
      }
    } else {message("update set to false in viz.yaml, using Sciencebase file")}
  } else {message("useLocal = TRUE, not downloading anything")
      if(!file.exists(viz[['location']])) {
        stop("You don't have any local data!  Set useLocal = FALSE to download
             from Sciencebase")
      }
    }
}

