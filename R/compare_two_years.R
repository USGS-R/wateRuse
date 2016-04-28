#' xyPlot
#'
#' returns scatter plot for user-defined params and subset criteria
#' 
#' @param w.use dataframe, the water use data 
#' @param data.elements chr, vector of data elements to be plotted (1 plot per element) 
#' @param areas chr, codes indicating HUCs, counties, states, aquifers, etc. 
#' @param year.x.y int, 2-element vector specifying the two years to be plotted
#' @param area.column character that defines which column to use to specify area
#' 
#' 
#' 
#' @export
#' 
#' @import ggplot2 
#' @importFrom tidyr gather_
#' 
#' 
#' @examples 
#' w.use <- wUseSample
#' data.elements <- c("PS.TOPop", "PS.SWPop")
#' areas <- "10" # NA uses all areas
#' area.column <- "STATECODE"
#' year.x.y <- c(2005,2010)
#' compare_two_years(w.use, data.elements, year.x.y, areas, area.column)
#' compare_two_years(w.use, data.elements, year.x.y)
#' compare_two_years(w.use, "PS.TOPop", year.x.y)
compare_two_years <- function(w.use, data.elements, year.x.y, areas=NA, area.column=NA){ 

  # subset w.use on basis of areas and select the data element of interest
  if (all(is.na(areas))){
    w.use.sub <- w.use[,c("YEAR",data.elements)]
  }else{
    w.use.sub <-  w.use[w.use[,area.column] %in% areas, c("YEAR",data.elements)]
  }
  
  w.use.sub <-  w.use.sub[w.use.sub$YEAR %in% year.x.y,] 
  
  x <- gather_(w.use.sub, "Key", "Value", data.elements)
  
  for(i in data.elements){
    df <- data.frame(
      x = x[x$YEAR == year.x.y[1] & x$Key == i,][["Value"]],
      y = x[x$YEAR == year.x.y[2] & x$Key == i,][["Value"]])
    
    df$Key <- i
    
    if(i == data.elements[1]){
      df_full <- df
    } else {
      df_full <- rbind(df_full, df)
    }
  }
  
  compare.plot <- ggplot(data = df_full) +
   geom_point(aes_string(x = "x", y = "y")) +
    geom_line(aes_string(x = "x", y = "x"),col="red") +
    facet_grid(Key ~ .) +
    xlab("") +
    ylab("")
  
  compare.plot
  
  return(compare.plot)
  
}