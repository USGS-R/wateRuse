#' xyPlot
#'
#' returns scatter plot for user-defined params and subset criteria
#' 
#' @param df data 
#' @param x_arg chr, the x-axis parameter to be plotted 
#' @param y_arg chr, the y-axis parameter to be plotted 
#' @param z_subset chr, subsetting criteria
#' 
#' 
#' 
#' @export
#' 
#' @examples 
#' x_arg <- "IR_Totwdl"
#' y_arg <- "IR_Totacr"
#' z_subset <- c("county code", "state code", "2000")
#' plot <- xyPlot(x_arg,y_arg,z_subset)
#' plot
xyPlot <- function(w.use, x_arg, y_arg, z_subset){ 

  w.use.sub <- subset(w.use, z_subset)
  
  xy_Plot <- ggplot(data = w.use.sub) +
    geom_point(aes(x = x_arg, y = y_arg))
  
  return(xy_Plot)
  
}