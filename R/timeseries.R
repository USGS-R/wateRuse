#' time_series_data
#'
#' time_series_data
#' 
#' @param data.elements character name of data element within available categories by year for state
#' @param years vector of integers specifying all years available for state. Defaults to NA which shows all years in dataset.
#' @param w.use is the datafile that includes all areas in all data elements for state
#' @param areas is a geographical area as defined in your datafile such as county, HUC, or aquifer
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
#' areas <- c("Dane","Milwaukee")
#' data.element <- "PS-GWPop"
#' time_series_data(w.use, data.element)
time_series_data <- function(w.use, data.elements, years= NA, areas= NA, y.scale=NA, log= FALSE){
  
  return(NA)
}
  