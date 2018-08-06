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

  data.elements <- data.elements[which(!is.na(data.elements))]
  
  w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas)
  
  w.use.sub <-  w.use.sub[w.use.sub$YEAR %in% year.x.y,] 
  
  x <- gather_(w.use.sub, "Key", "Value", data.elements)
  
  x <- x[which(!is.na(x$Value)),]
  
  for(i in data.elements){
    
    df_site_x <- x[x$YEAR == year.x.y[1] & x$Key == i,][[area.column]]
    df_site_y <- x[x$YEAR == year.x.y[2] & x$Key == i,][[area.column]]
    df_site <- data.frame(unique(c(df_site_x, df_site_y)), stringsAsFactors = FALSE)
    colnames(df_site) <- c(area.column)
    
    df_x <- x[x$YEAR == year.x.y[1] & x$Key == i,c(area.column, "Value")]
    colnames(df_x) <- c(area.column, "x")
    df_y <- x[x$YEAR == year.x.y[2] & x$Key == i,c(area.column, "Value")]
    colnames(df_y) <- c(area.column, "y")

    df <- left_join(df_site, df_x)
    df <- left_join(df, df_y)
    colnames(df) <- c("site", "x", "y")
    
    if(all(is.na(df_x)) | nrow(df_x) == 0) stop('No Data Available for First Year Selected')
    
    if(all(is.na(df_y)) | nrow(df_y) == 0) stop('No Data Available for Second Year Selected')
    
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
   geom_point(aes_string(x = "x", y = "y", color = "site"), size = 3) +
    geom_line(aes_string(x = "x", y = "x"),col="red") +
    facet_wrap(~ Key, ncol = 1) +
    xlab(year.x.y[1]) +
    ylab(year.x.y[2]) +
    scale_colour_manual(values=c.palette)
  
  if(!legend){
    compare.plot <- compare.plot + theme(legend.position = "none")
  }
  
  compare.plot
  
  return(compare.plot)
  
}# compare_two_years