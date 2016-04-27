#' xyPlot
#'
#' returns scatter plot for user-defined params and subset criteria
#' 
#' @param w.use dataframe, the water use data 
#' @param data.elements chr, vector of data elements to be plotted (1 plot per element) 
#' @param areas chr, codes indicating HUCs, counties, states, aquifers, etc. 
#' @param year.x.y int, 2-element vector specifying the two years to be plotted
#' 
#' 
#' 
#' @export
#' 
#' @examples 
#' w.use <- wUseSample
#' data.elements <- "PS_Popsvr"
#' areas <- NA # NA uses all areas
#' year.x.y <- c(2005,2010)
#' compare_two_years(w.use, data.elements, areas, year.x.y)
compare_two_years <- function(w.use, data.elements, areas, year.x.y){ 

  # subset w.use based on areas and select the data element of interest
  # w.use.sub <- subset(w.use, w.use$area==areas, select=data.elements)
  
  # x.arg <- subset(w.use.sub, w.use.sub$year==year.x.y[1])
  # y.arg <- subset(w.use.sub, w.use.sub$year==year.x.y[2])
  
  # xy_Plot <- ggplot(data = w.use.sub) +
  # geom_point(aes(x = x.arg, y = y.arg))
  
  return(NA)
  
}