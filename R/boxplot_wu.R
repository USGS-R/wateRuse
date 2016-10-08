#' boxplot_wu
#'
#' boxplot_wu
#' 
#' @param data.elements character name of data element within available categories by year for state
#' @param years vector of integers specifying range of years to graph. Defaults to NA which shows all years in dataset.
#' @param w.use is a subset of the datafile wUseSample that includes all areas in all data elements for state
#' @param areas is a geographical area as defined in your datafile such as county, HUC, or aquifer
#' @param sub.area is the area (county, huc) column name. This is most useful for the app
#' @param area.column character that defines which column to use to specify area
#' @param log = TRUE or FALSE allows user to set log scale, default is FALSE
#' @param plot.notch is a logical function to show boxplot as optionally notched or not
#' 
#' @export
#' @import ggplot2
#' @importFrom tidyr gather_
#' 
#' @examples 
#' w.use <- wUseSample
#' areas <- "15"
#' area.column = "STATECODE"
#' data.elements <- c("PS.GWPop","TP.TotPop")
#' boxplot_wu(w.use, data.elements, area.column = area.column,areas = areas)
#' boxplot_wu(w.use, data.elements, area.column, log=TRUE)
#' boxplot_wu(w.use, data.elements, area.column, years = c(1995, 2005))
boxplot_wu <- function(w.use, data.elements, area.column, sub.area = "COUNTYNAME",
                       areas=NA, plot.notch=FALSE, years=NA, log=FALSE){
  
  if (all(is.na(areas))){
    w.use.sub <- w.use[,c("YEAR",area.column,sub.area,data.elements)]
  }else{
    w.use.sub <-  w.use[w.use[[area.column]] %in% areas, c("YEAR",area.column,data.elements,sub.area)]
  }
  
  if(!any(is.na(years))){
    w.use.sub <-  w.use.sub[w.use.sub$YEAR %in% years,]
    w.use.sub$YEAR <- as.factor(w.use.sub$YEAR)
    levels(w.use.sub$YEAR) <- as.character(years)
  }
  
  df2 <- w.use.sub[,c("YEAR",area.column,sub.area,data.elements)]
  
  df3 <- gather_(df2, "dataElement", "value", data.elements)
  
  bp.object <- ggplot(df3, aes_string(x = "YEAR", group="YEAR", y = "value") ) + 
    geom_boxplot(notch=plot.notch,fill = "darkorange", outlier.colour = NA) +
    facet_grid(dataElement ~ ., scales="free")  +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    geom_point(aes_string(color = sub.area), 
               position = position_jitter(width=0.9), alpha = 0.4) +
    theme(legend.position = "none")

  # caution, zeros dropped for log scale like NAs
  if(log){
    bp.object <- bp.object + scale_y_log10()
  }
  
  bp.object
  
  return(bp.object)
}
