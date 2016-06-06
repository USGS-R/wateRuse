#' subset_huc_polygons
#'
#' Returns HUC polygons 
#' Dataset is subset on basis of area of interest
#' 
#' @param areas chr, vector of codes indicating area of interest to subset on
#' @param area.column chr, defines which column the areas refer to for subsetting
#' @param year int, the year of interest to be mapped (defines historical basis for HUCs)
#' 
#' @export
#' 
#' @return hc.sub dataframe, a subset of HUC polygon data 
#' 
#' @examples 
#' areas <- "ME" # 21 HUC8s in Maine 
#' area.column <- "STATES"
#' year <- 1995 
#' hc.sub <- subset_huc_polygons(year, area.column, areas)
#' hc.sub$HUCNAME
subset_huc_polygons <- function(year, area.column, areas){

  #hc.sub <- huc08Poly[which(huc08Poly@data[,area.column] == areas),]
  
  hc.sub <- huc08Poly[which(grepl(areas,huc08Poly@data[,area.column])),]
  
  return(hc.sub)
  
}# subset_huc_polygons