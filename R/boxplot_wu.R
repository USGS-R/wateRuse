#' boxplot_wu
#'
#' boxplot_wu
#' 
#' @param data.elements character name of data element within available categories by year for state
#' @param years vector of integers specifying range of years to graph. Defaults to NA which shows all years in dataset.
#' @param w.use is a subset of the datafile wUseSample that includes all areas in all data elements for state
#' @param areas is a geographical area as defined in your datafile such as county, HUC, or aquifer
#' @param area.column character that defines which column to use to specify area
#' @param log = TRUE or FALSE allows user to set log scale, default is FALSE
#' @param y.scale allows R to set the y-axis scale given available data range. Defaults to NA which lets R set the scale based on dataset values.
#' @param plot.notch is a logical function to show boxplot as optionally notched or not

#'
#' 
#' @export
#' @import ggplot2
#' @importFrom tidyr gather_
#' 
#' @examples 
#' df <- wUseSample
#' areas <- "15"
#' area.column = "STATECODE"
#' data.elements <- c("PS.GWPop","TP.TotPop")
#' w.use <- subset_wuse(df, data.elements,area.column,areas)
#' year1 <- 2005
#' year2 <- 2010
#' years <- c(year1, year2)
#' boxplot_wu(w.use, data.elements, area.column = area.column,areas = areas)
#' boxplot_wu(w.use, data.elements, area.column, log=TRUE)
#' boxplot_wu(w.use, data.elements, area.column, y.scale = c(0,500))
#' boxplot_wu(w.use, data.elements, area.column, 
#'        y.scale = c(0,100), years = c("1995", "2005"))
#'        
boxplot_wu <- function(w.use, data.elements, area.column, areas=NA, plot.notch=TRUE, years=NA, log=FALSE, y.scale=NA){
  
  if(!all(is.na(areas))){
    w.use <- w.use[w.use[[area.column]] %in% areas,]
  }
  
  df2 <- w.use[,c("YEAR",area.column,data.elements)]
  
  df3 <- gather_(df2, "dataElement", "value", c(data.elements))
 
  # this works as factor or unmeric, but cannot figure out how to limit range on xaxis below (years)
  
  bp.object <- ggplot(df3, aes(x = YEAR, group=YEAR, y = value) )  
  
  # bp.object <- ggplot(df3, aes(x = as.numeric(YEAR), group=YEAR, y = value) )  
  # bp.object <- ggplot(df3, aes(x = factor(YEAR), y = value) )  
  
  
  if(plot.notch){
    bp.object <- bp.object + geom_boxplot(notch=TRUE)
  } else {
    bp.object <- bp.object + geom_boxplot()
  }

  bp.object <- bp.object + facet_grid(dataElement ~ .)  
  
  if(!all(is.na(y.scale))){
    bp.object <- bp.object + ylim(y.scale)
  }
  # error here attempting limits on xaxis-
  if(!all(is.na(years))){
    bp.object<-bp.object + xlim(years)
  }
  #Error: Discrete value supplied to continuous scale
  #bp.object<-bp.object + scale_x_continuous(breaks=NULL) + xlim(years)#xlim(years)
  #bp.object<-bp.object + scale_x_continuous(limits=c(1995, 2000)) #xlim(years)
  #Scale for 'x' is already present. Adding another scale for 'x', which will replace the existing scale. 
  
  
  # caution, zeros dropped for log scale like NAs
  if(log){
    bp.object <- bp.object + scale_y_log10()
  }
  
  bp.object
  
  return(bp.object)
}
