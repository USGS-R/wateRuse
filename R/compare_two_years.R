#' compare_two_years
#'
#' Returns scatter plot for 2 years for data elements of interest.
#' Option to narrow down data geographically
#' 
#' @param w.use dataframe, the water use data 
#' @param data.elements chr, vector of data elements to be plotted (1 plot per element) 
#' @param areas chr, codes indicating HUCs, counties, states, aquifers, etc. 
#' @param year.x.y int, 2-element vector specifying the two years to be plotted
#' @param area.column character that defines which column to use to specify area
#' @param legend is a logical function to include list of counties in a legend if manageable, default is FALSE
#' @param c.palette color palette to use for points
#' 
#' @export
#' 
#' @import ggplot2 
#' @importFrom tidyr gather_
#' @importFrom grDevices colorRampPalette
#' 
#' @examples 
#' w.use <- wUseSample
#' data.elements <- c("PS.TOPop", "PS.SWPop")
#' areas <- "10" # NA uses all areas
#' area.column <- "STATECODE"
#' year.x.y <- c(2005,2010)
#' compare_two_years(w.use, data.elements, year.x.y, area.column, areas)
#' compare_two_years(w.use, data.elements, year.x.y, area.column)
#' compare_two_years(w.use, "PS.TOPop", year.x.y, area.column)
compare_two_years <- function(w.use, data.elements, year.x.y, area.column, areas=NA, 
                              legend = FALSE,
                              c.palette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")){ 

  w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas)
  
  w.use.sub <-  w.use.sub[w.use.sub$YEAR %in% year.x.y,] 
  
  x <- gather_(w.use.sub, "Key", "Value", data.elements)
  
  for(i in data.elements){
    
    df_x<-x[x$YEAR == year.x.y[1] & x$Key == i,][["Value"]]
    df_y<-x[x$YEAR == year.x.y[2] & x$Key == i,][["Value"]]
    df_site <- x[x$YEAR == year.x.y[2] & x$Key == i,][[area.column]]
    
    if(!length(df_x)==length(df_y)) { # I found this issue with Alaska between 2005 and 2010.
      stop('Different number of counties from one compilation year to the other not supported yet.')}
    
    if(all(is.na(df_x))) stop('No Data Available for First Year Selected')
    
    if(all(is.na(df_y))) stop('No Data Available for Second Year Selected')
    
    df <- data.frame(
      x = df_x,
      y = df_y,
      site = df_site,
      stringsAsFactors = FALSE)
    
    df$Key <- i
    
    if(i == data.elements[1]){
      df_full <- df
    } else {
      df_full <- rbind(df_full, df)
    }
  }
  
  if(length(unique(df_full$site)) > length(c.palette)){
    c.palette.ramp <- colorRampPalette(c.palette)
    c.palette <- c.palette.ramp(length(unique(df_full$site)))
  }
  
  compare.plot <- ggplot(data = df_full) +
   geom_point(aes_string(x = "x", y = "y", color = "site"), 
              show.legend = legend, size = 3) +
    geom_line(aes_string(x = "x", y = "x"),col="red") +
    facet_wrap(~ Key, ncol = 1) +
    xlab(year.x.y[1]) +
    ylab(year.x.y[2]) +
    scale_colour_manual(values=c.palette)
  
  compare.plot
  
  return(compare.plot)
  
}# compare_two_years