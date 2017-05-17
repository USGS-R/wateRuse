#' Run water use application
#' 
#' Run water use application
#' 
#' @param browse use browser for map rendering
#' @export
#' @importFrom shiny runApp
#' @import DT
#' @import ggplot2
#' @import shinydashboard
#' @importFrom plotly plotlyOutput
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tidyr gather_
#' @examples 
#' \dontrun{
#' explore_wateRuse()
#' }
explore_wateRuse <- function(browse=TRUE){
  runApp(system.file('shiny', package='wateRuse'), launch.browser = browse)
}