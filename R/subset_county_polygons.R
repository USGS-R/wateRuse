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
#' @importFrom tidyr gather_
#' 
#' @examples 
#' areas <- "Delaware" # should yield 3 counties
#' areas <- "Maine"# should yield 16 counties
#' areas <- "New Hampshire"# should yield 10 counties
#' area.column <- "STATE_TERR"
#' year <- 2010
#' subset_county_polygons(areas, area.column, year)


subset_county_polygons <- function(areas=NA, area.column, year){
  
  test.date <- as.Date(paste0(year,"-12-31"))# year of interest converted to date
  now.date <-format(Sys.Date(), "%Y-%m-%d")
  # select area of interest
  hc_sub <- histCounties[which(histCounties@data[,area.column] == areas),]
  
  # substitute todays date for the last date of the histCounties dataset (2000-12-31)
  hc_sub$END_DATE[hc_sub$END_DATE==max(hc_sub$END_DATE)] <- now.date
  # assuming the counties defined in 2000-12-31 are the same now, use todays 
  # date to make sure the selection for contemporaneous counties works  
  hc_sub <- hc_sub[hc_sub$END_DATE >= test.date & hc_sub$START_DATE <= test.date,]# for year of interest, get the counties 

  return(hc_sub)
  
}# subset_county_polygons