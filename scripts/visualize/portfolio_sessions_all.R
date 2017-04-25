visualize.portfolio_sessions_all <- function(viz){
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  deps <- readDepends(viz)
  
  height = viz[["height"]]
  width = viz[["width"]]
  
  viz.data <- deps[["sessions_and_new_users"]]
  
  viz.data.daily <- deps[["sessions_and_new_users_daily"]]
  
  ga_table <- deps[["project_table"]] 
  ga_table$viewID <- as.character(ga_table$viewID)
  
  range_text <- c("-1 year","-1 month","-1 week")
  names(range_text) <- c("Year:","Month:","Week:")
  
  latest_day = max(viz.data$date, na.rm = TRUE)
  
  summary_data <- data.frame()
  level_text <- c()
  
  for(i in seq_len(length(range_text))){
    range_days = seq(latest_day, length = 2, by = range_text[i])
    
    j <- paste(names(range_text)[i],paste0(range(range_days), collapse = " to "))
    level_text <- c(level_text, j)
    
    summary_sessions <- viz.data %>%
      filter(date >= range_days[2]) %>%
      group_by(viewID) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE), newUsers = sum(newUsers, na.rm = TRUE)) %>%
      arrange(sessions) %>%
      left_join(select(ga_table, viewID, shortName), by="viewID") %>%
      mutate(type = j) %>%
      select(-viewID)

    summary_data <- bind_rows(summary_data, summary_sessions)

  }
  
  summary_data <- summary_data %>% filter(!is.na(shortName)) %>% arrange(desc(sessions))
  
  time.facets <- c("Year", "Week")
  break.by <- "Year"
  
  break_data <- filter(summary_data, grepl(break.by, type)) %>% 
    mutate(bin = cut(sessions, breaks = c(-Inf, viz[['breaks']], Inf))) %>% arrange(desc(sessions))
  
  lm <- 1.7 # left margin
  tm <- 0.25 # top margin
  v.spc <- 0.05 # vertical space in between category rectangles
  h.spc <- 0.035 # horizontal space
  bm <- 0.05 # bottom margin
  rect.buffer <- 0.13 # buffer between top (and bottom) apps of each category and the rectangle border
  
  # total space for app info:
  cat.total <- height - tm - bm - v.spc*length(viz[['breaks']]) - rect.buffer*2*(length(viz[['breaks']]) + 1) 
  app.h <- cat.total / (length(unique(break_data$shortName)) - 1) # what happens when a category bin is empty?
  
  png(filename = viz[["location"]], 
      height = height, width = width, units = 'in', res = 150)
  par(mar = c(0, 0, 0, 0), omi = c(0, lm, 0, 0), xpd = NA)
  plot(0, NA, ylim = c(0, height), xlim = c(0, length(time.facets)), 
       axes = FALSE, xlab="", ylab="", xaxs = 'i', yaxs = 'i') # 10% wider for text
  # xlim is 0 to number of time facets
  
  bins <- unique(as.character(break_data$bin))
  cat.names <- c('very high traffic','high traffic','moderate traffic','low traffic')
  
  x.0 <- 0
  for (t in time.facets){
    y.0 <- height - tm
    top.text <- filter(summary_data, grepl(t, type)) %>% .$type %>% unique()
    text(x.0, y.0+tm/2, labels=top.text, pos=4, offset=0.2, cex = 1.25)
    cols <- sprintf('grey%1.0f', seq(from = 80, to = 95, length.out = length(bins)))
    # makes the maximums of the smaller categories a little smaller, larger numbers = further left:
    bump <- seq(from = 1.2, to = 1.3, length.out = length(bins)) 
    
    for (cat.bin in bins) { # are already ranked w/ bins
      # make the rectangle, calculate which apps are included:
      bin.names <- break_data %>% filter(bin %in% cat.bin) %>% .$shortName %>% as.character()
      rect(x.0, y.0-(length(bin.names) - 1)*app.h - 2*rect.buffer, xright = x.0 + 1.0-h.spc, ytop = y.0, col = cols[1], border = NA)
      
      cat.data <- filter(summary_data, shortName %in% bin.names, grepl(t, type))
      cat.max <- cat.data %>% .$sessions %>% max() %>% "*"(bump[1])
      if (t == time.facets[1]){
        text(x.0+1.0-h.spc, y.0-(nrow(cat.data) - 1)*app.h - 2 * rect.buffer, labels = cat.names[1], adj = c(1.05,-0.5), cex = 0.75)  
      }
      y.0 <- y.0 - rect.buffer
      for (app in bin.names){
        app.data <- filter(cat.data, shortName == app)
        if (nrow(app.data) == 0) {
          sess.len <- 0
          user.len <- 0
          short.name <- ""
          app.num <- 0
        } else {
          app.num <- app.data$sessions
          sess.len <- app.num/cat.max
          user.len <- app.data$newUsers/cat.max
          short.name <- app.data$shortName
        }
        
        # plot lollipops and label: 
        segments(c(x.0,x.0), c(y.0,y.0), c(x.0+sess.len,x.0+user.len), c(y.0,y.0),
                 lend = 1, lwd = c(1,3))
        points(x = x.0+sess.len, y = y.0, pch = 20, col='black')
        text(x = x.0+sess.len, y = y.0, labels = pretty_num(app.num), pos = 4)
        
        if (t == time.facets[1]){ # label the y-axis
          text(x.0, y = y.0, labels = short.name, pos=2)
        }
        
        y.0 <- y.0 - app.h
      }
      y.0 <- y.0 - (app.h - rect.buffer)
      cols <- tail(cols, -1L)
      bump <- tail(bump, -1L)
      cat.names <- tail(cat.names, -1L)
    }
    x.0 <- x.0 + 1
  }
  
  
  
  dev.off()
}


fancyNumbers <- function(n){
  nNoNA <- n[!is.na(n)]
  x <-gsub(pattern = "1e",replacement = "10^",x = format(nNoNA, scientific = TRUE))
  exponents <- as.numeric(sapply(strsplit(x, "\\^"), function(j) j[2]))
  base <- ifelse(exponents == 0, "1", ifelse(exponents == 1, "10","10^"))
  exponents[base == "1" | base == "10"] <- ""
  textNums <- rep(NA, length(n))  
  textNums[!is.na(n)] <- paste0(base,exponents)
  
  textReturn <- parse(text=textNums)
  return(textReturn)
}
pretty_num <- function(n){
  if (is.na(n)) n = 0
  if (n > 1e7){
    out <- sprintf('%1.0fM', n/1e6)
  } else if (n > 1e6){
    out <- sprintf('%1.1fM', n/1e6)
  } else if (n > 1e4){
    out <- sprintf('%1.0fK', n/1e3)
  } else if (n > 1000){
    out <- sprintf('%1.1fK', n/1e3)
  } else {
    out <- as.character(n)
  }
  return(out)
}
