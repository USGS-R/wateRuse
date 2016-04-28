#' compare_two_elements
#'
#' Returns scatter plot for 2 data elements for years of interest.
#' Option to narrow down data geographically
#' 
#' @param w.use dataframe, the water use data 
#' @param data.elements.x.y chr, 2-element vector of data elements to be plotted 
#' @param areas chr, codes indicating HUCs, counties, states, aquifers, etc. 
#' @param years int, vector specifying the years to be plotted (1 plot per year)
#' @param area.column chr, defines which column to use to specify area
#' 
#' @export
#' 
#' @import ggplot2 
#' @importFrom tidyr gather_
#' 
#' @examples 
#' w.use <- wUseSample
#' data.elements.x.y <- c("TP.TotPop", "PS.WSWFr")
#' areas <- "10" # NA uses all areas
#' area.column <- "STATECODE"
#' years <- c(2000, 2005, 2010)
#' compare_two_elements(w.use, data.elements.x.y, years, area.column, areas)
#' compare_two_elements(w.use, data.elements.x.y, years, area.column)
#' compare_two_elements(w.use, data.elements.x.y, "2010", area.column)
compare_two_elements <- function(w.use, data.elements.x.y, years, area.column, areas=NA){ 
  
  w.use.sub <- subset_wuse(w.use, data.elements.x.y, area.column, areas)
  
  w.use.sub <-  w.use.sub[w.use.sub$YEAR %in% years,] 
  
  x <- gather_(w.use.sub, "Element", "Value", data.elements.x.y)
  
  for(i in years){
    df <- data.frame(
      x = x[x$Element == data.elements.x.y[1] & x$YEAR == i,][["Value"]],
      y = x[x$Element == data.elements.x.y[2] & x$YEAR == i,][["Value"]])
    
    df$YEAR <- i
    
    if(i == years[1]){
      df_full <- df
    } else {
      df_full <- rbind(df_full, df)
    }
  }# i
  
  compare.plot <- ggplot(data = df_full) +
    geom_point(aes_string(x = "x", y = "y")) +
    facet_wrap(~ YEAR, ncol = 1) +
    xlab(data.elements.x.y[1]) +
    ylab(data.elements.x.y[2])
  
  compare.plot
  
  return(compare.plot)
  
}# compare_two_elements