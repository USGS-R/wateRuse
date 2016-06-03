#' subset_county_polygons
#'
#' Returns county polygons from: Siczewicz, Peter. U.S. Historical Counties 
#' (Generalized .001 deg) on the basis of year of interest
#' Dataset is subset on basis of area of interest
#' 
#' @param areas chr, vector of codes indicating HUCs, counties, states, regions, aquifers, etc. 
#' @param area.column chr, defines which column to use to specify area
#' @param year int, the year of interest to be mapped (defines historical basis for counties)
#' 
#' @export
#' 
#' @return hc.sub dataframe, a subset of county polygon data 
#' 
#' @examples 
#' areas <- "Delaware" # 3 counties present day
#' area.column <- "STATE_TERR"
#' year <- 2010 
#' hc.sub <- subset_county_polygons(area.column, year, areas)
#' hc.sub$NAME
#' 
#' areas <- "Maine" # 16 counties present day
#' area.column <- "STATE_TERR"
#' year <- 2010 
#' hc.sub <- subset_county_polygons(area.column, year, areas)
#' hc.sub$NAME
#' year <- 1850 # Maine had 13 counties in 1850; Delaware 3
#' hc.sub <- subset_county_polygons(area.column, year, areas)
#' hc.sub$NAME
subset_county_polygons <- function(year, area.column=NA, areas=NA){
  
  test.date <- as.Date(paste0(year,"-12-31"))# year of interest converted to date
  now.date <-format(Sys.Date(), "%Y-%m-%d")
  # select area of interest
  hc.sub <- histCounties[which(histCounties@data[,area.column] == areas),]
  
  # substitute todays date for the last date of the histCounties dataset (2000-12-31)
  hc.sub$END_DATE[hc.sub$END_DATE==max(hc.sub$END_DATE)] <- now.date
  # assuming the counties defined in 2000-12-31 are the same now, use todays 
  # date to make sure the selection for contemporaneous counties works  
  hc.sub <- hc.sub[hc.sub$END_DATE >= test.date & hc.sub$START_DATE <= test.date,]# for year of interest, get the counties 

  return(hc.sub)
  
}# subset_county_polygons