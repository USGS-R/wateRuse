#' subset_wuse
#'
#' returns subset of WUSE data elements for plotting based on county state codes
#' 
#' @param w.use dataframe, the water use data 
#' @param data.elements chr, vector of data elements to be plotted (1 plot per element) 
#' @param areas chr, codes for state-county ('NA' uses all areas)
#' @param area.column character that defines which column to use to specify area
#' 
#' @export 
#' 
#' @return w.use.sub dataframe, a subset of water use data from user specifications
#' 
#' @examples 
#' w.use <- wUseSample
#' data.elements <- c("PS.TOPop", "PS.SWPop")
#' areas <- "10" # NA uses all areas
#' area.column <- "STATECODE"
#' 
#' w.use.sub <- subset_wuse(w.use, data.elements,area.column,areas)
subset_wuse <- function(w.use, data.elements, area.column, areas=NA) {
  # subset w.use on basis of areas and select the data elements of interest
  # if (all(is.na(data.elements))){
  #   stop("All data.elements cannot be NA.")
  # }
  data.elements <- data.elements[which(!is.na(data.elements))]
  if (all(is.na(areas))){
    w.use.sub <- w.use[,c("YEAR",area.column,data.elements)]
  }else{
    w.use.sub <-  w.use[w.use[[area.column]] %in% areas, c("YEAR",area.column,data.elements)]
  }
  return(w.use.sub)
} 

