#' time_series_data
#'
#' time_series_data
#' 
#' @param data.elements character name of data element within available categories by year for state
#' @param years vector of integers specifying all years available for state. Defaults to NA which shows all years in dataset.
#' @param w.use is the datafile that includes all areas in all data elements for state
#' @param areas is a geographical area as defined in your datafile such as county, HUC, or aquifer
#' @param area.column character that defines which column to use to specify area
#' @param y.scale allows R to set the y-axis scale given available data range. Defaults to NA which lets R set the scale based on dataset values.
#' @param log = TRUE or FALSE allows user to set log scale, default is FALSE
#'
#' 
#' @export
#' @import ggplot2
#' 
#' @examples 
#' w.use <- wUseSample
#' year1 <- 2005
#' year2 <- 2010
#' years <- c(year1, year2)
#' areas <- c("Kent County","Sussex County")
#' data.elements <- "PS-GWPop"
#' plotObject <- time_series_data(w.use, data.elements, area.column = "COUNTYNAME",areas = areas)
#' plotObject <- time_series_data(w.use, data.elements)
#' plotObject <- time_series_data(w.use, data.elements, y.scale = c(0,100))
#' plotObject <- time_series_data(w.use, data.elements, y.scale = c(0,100), years = c(1990,2005))
time_series_data <- function(w.use, data.elements, years= NA, area.column = NA, areas= NA, y.scale=NA, log= FALSE){
  
  data.elements <- paste0("`",data.elements,"`")
  
  if(!all(is.na(areas))){
    w.use <- w.use[w.use[,area.column] %in% areas,]
  }
  
  ts.object <- ggplot(data = w.use) + 
    geom_line(aes_string(x = "YEAR", y = data.elements))
  
  if(!all(is.na(y.scale))){
    ts.object <- ts.object + ylim(y.scale)
  }
  
  if(!all(is.na(years))){
    ts.object <- ts.object + xlim(years)
  }
  print(ts.object)
  
  return(ts.object)
}
  