#' choropleth_plot
#'
#' Returns choropleth plot of data element of interest for year of interest
#' Currently written to work with counties only
#' Only works with one state, data element, and year as currently written
#'
#' 
#' @param data.elements chr, data element to be plotted
#' @param norm.element chr, data element to be used for normalizing data.elements
#' @param year int, the year of interest to be mapped (defines historical basis for counties)
#' @param areas chr, vector of codes indicating HUCs, counties, states, regions, aquifers, etc. 
#' @param area.column chr, defines which column to use to specify area
#' 
#' @export
#' 
#' @import ggplot2
#' @import rgeos
#' @import maptools
#' @import scales
#' 
#' @examples 
#' w.use <- wUseSample
#' data.elements <- "PS.WFrTo"
#' norm.element <- "PS.TOPop"
#' year <- 2010 
#' areas <- "Delaware" 
#' area.column <- "STATE_TERR"
#' ch.plot <- choropleth_plot(data.elements, norm.element, year, areas, area.column)
#' ch.plot
#' norm.element <- NA
#' ch.plot <- choropleth_plot(data.elements, norm.element, year, areas, area.column)
#' ch.plot
choropleth_plot <- function(data.elements, norm.element=NA, year, areas, area.column){
  
  # get counties
  hc.sub <- subset_county_polygons(areas, area.column, year)
  hc.subf<-fortify(hc.sub,region = "FIPS")
  hc.sub@data$id<-hc.sub@data$FIPS
  hc.subf<-merge(hc.subf,hc.sub@data, by="id", all.x=TRUE)
  hc.subf<-hc.subf[order(hc.subf$order), ] 
  
  # get water use data
  wu.areas <- hc.sub$FIPS
  wu.area.column <- "STATECOUNTYCODE"
  if (!is.na(norm.element)){
    data.elements.all <- cbind(data.elements, norm.element)
  }else{
    data.elements.all <- data.elements
  }
  w.use.sub <- subset_wuse(w.use, data.elements.all, wu.area.column, wu.areas)
  w.use.sub <- w.use.sub[which(w.use.sub$YEAR == year),]# for year of interest

  # normalize
  if (!is.na(norm.element)){
    for (i in data.elements){
      w.use.sub[,paste0(i,"_norm")] <- w.use.sub[,i]/w.use.sub[,norm.element]
    }# i
  }# if norm.element
  
  # set common key name "id" to merge water use data with polygon data
  names(w.use.sub)[names(w.use.sub)=="STATECOUNTYCODE"] <- "id"
  
  # merge polygons and water use data to build choropleth dataset
  hc.subf <- merge(hc.subf,w.use.sub, by="id", all.x=TRUE)
  
  # plot element
  p.elem <- data.elements
  if (!is.na(norm.element)){p.elem <- paste0(data.elements,"_norm")}
  
  
  ch.plot <- ggplot() + geom_polygon(data = hc.subf, 
                 aes(x = long, y = lat, group=group, fill= hc.subf[,p.elem]), 
                 color="black", size=0.25) + 
                 coord_map() + 
                 scale_fill_distiller(name=p.elem, palette = "YlGn", breaks = pretty_breaks(n = 5))
  
  ch.plot

  return(ch.plot)
  
}# choropleth_plot