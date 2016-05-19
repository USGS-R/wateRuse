#' barchart_sums
#'
#' barchart_sums
#' 
#' @param data.elements character name of data element within available categories by year for state
#' @param years vector of integers specifying range of years to graph. Defaults to NA which shows all years in dataset.
#' @param w.use is a subset of the datafile wUseSample that includes all areas in all data elements for state
#' @param areas is a geographical area as defined in your datafile such as county, HUC, or aquifer
#' @param area.column character that defines which column to use to specify area
#' @param y.scale allows R to set the y-axis scale given available data range. Defaults to NA which lets R set the scale based on dataset values.
#' @param plot.stack is a logical function to show graph as optionally stacked or clustered bar graph

#'
#' 
#' @export
#' @import ggplot2
#' @importFrom tidyr gather_
#' 
#' @examples 
#' df <- wUseSample
#' areas <- c("New Castle County", "Kent County")
#' area.column = "COUNTYNAME"
#' data.elements <- c("PS.WTotl","CO.WTotl","DO.WTotl","IN.WTotl","PF.WTotl")
#' w.use <- subset_wuse(df, data.elements,area.column,areas)
#' year1 <- 2005
#' year2 <- 2010
#' years <- c(year1, year2)
#' barchart_sums(w.use, data.elements, area.column = area.column,areas = areas)
#' barchart_sums(w.use, data.elements, plot.stack = FALSE,
#'        area.column = area.column,areas = areas)
#' barchart_sums(w.use, data.elements, area.column)
#' barchart_sums(w.use, data.elements, area.column, y.scale = c(0,500))
#' barchart_sums(w.use, data.elements, area.column, 
#'        y.scale = c(0,100), years = c(1990,2005))
barchart_sums <- function(w.use, data.elements, area.column, plot.stack=TRUE,
                             years=NA, areas=NA, y.scale=NA){
  
  if(!all(is.na(areas))){
    w.use <- w.use[w.use[[area.column]] %in% areas,]
  }
  
  df2 <- w.use[,c("YEAR",area.column,data.elements)]
  
  df3 <- gather_(df2, "dataElement", "value", c(data.elements))
  
  bc.object <- ggplot(data = df3) 
  
  if(plot.stack){
    bc.object <- bc.object + geom_bar(aes_string(x = "YEAR", y = "value", fill = "dataElement"), 
                                      position = "stack",stat="identity")
  } else {
    bc.object <- bc.object + geom_bar(aes_string(x = "YEAR", y = "value", fill = "dataElement"), 
                                      position = "dodge",stat="identity")
  }
  #facet if totals available for multiple areas (counties, etc)
  bc.object <- bc.object + facet_grid(COUNTYNAME ~ .) +
    ylab("") 
  
  if(!all(is.na(y.scale))){
    bc.object <- bc.object + ylim(y.scale)
  }
  
  if(!all(is.na(years))){
    bc.object <- bc.object + xlim(years)
  }
  
  
  bc.object
  
  return(bc.object)
}
