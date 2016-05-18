#' multi_element_data
#'
#' multi_element_data
#' 
#' @param data.elements character name of data element within available categories by year for state
#' @param years vector of integers specifying all years available for state. Defaults to NA which shows all years in dataset.
#' @param w.use is a subset of the datafile wUseSample that includes all areas in all data elements for state
#' @param areas is a geographical area as defined in your datafile such as county, HUC, or aquifer
#' @param area.column character that defines which column to use to specify area
#' @param y.scale allows R to set the y-axis scale given available data range. Defaults to NA which lets R set the scale based on dataset values.
#' @param log = TRUE or FALSE allows user to set log scale, default is FALSE
#' @param plot.points is a logical function to show counties as points or clustered bar graph
#' @param legend is a logical function to include list of counties in a legend if manageable, default is TRUE
#'
#' 
#' @export
#' @import ggplot2
#' @importFrom tidyr gather_
#' 
#' @examples 
#' w.use <- wUseSample
#' areas <- c("Kent County","Sussex County")
#' area.column = "COUNTYNAME"
#' data.elements <- c("PS.GWPop","TP.TotPop")
#' multi_element_data(w.use, data.elements, area.column = area.column,areas = areas)
#' multi_element_data(w.use, data.elements, plot.points = FALSE,
#'        area.column = area.column,areas = areas)
#' multi_element_data(w.use, data.elements, plot.points = FALSE,
#'        area.column = area.column,areas = "Sussex County")
#' multi_element_data(w.use, data.elements, area.column)
#' multi_element_data(w.use, data.elements, area.column, y.scale = c(0,1000))
#' multi_element_data(w.use, data.elements, area.column, 
#'        y.scale = c(0,100), years = c(1990,2005))
multi_element_data <- function(w.use, data.elements, area.column, plot.points = TRUE,
                               years= NA, areas= NA, y.scale=NA, log= FALSE, legend= TRUE){
  
  w.use <- subset_wuse(w.use, data.elements, area.column, areas)
  
  df <- w.use[,c("YEAR",area.column,data.elements)]
  
  df <- gather_(df, "dataElement", "value", c(data.elements))
  # df$area.column <- df[[area.column]]
  
  me.object <- ggplot(data = df, aes_string(x = "YEAR", y = "value"))
  
  if(plot.points){
    me.object <- me.object + 
      geom_point(aes_string(color = "dataElement"), show.legend = legend) +
      geom_line(aes_string(color = "dataElement"), show.legend = legend)
  } else {
    me.object <- me.object + geom_bar(aes_string(fill = "dataElement"), 
                                      position = "dodge",stat="identity",show.legend = legend)
  }
  
  me.object <- me.object + facet_grid(as.formula(paste0(area.column," ~ ."))) +
    ylab("") 
  
  if(!all(is.na(y.scale))){
    me.object <- me.object + ylim(y.scale)
  }
  
  if(!all(is.na(years))){
    me.object <- me.object + xlim(years)
  }
  
  if(log){
    me.object <- me.object + scale_y_log10()
  }
  
  me.object
  
  return(me.object)
}

