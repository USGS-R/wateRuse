#' choropleth_plot
#'
#' Returns choropleth plot of data element of interest for year of interest
#' Currently written to work with counties and hucs only
#' Only works with one data element and year as currently written
#'
#' 
#' @param w.use dataframe, the water use data 
#' @param data.elements chr, data element to be plotted
#' @param year int, the year of interest to be mapped (defines historical basis for counties)
#' @param state character name of state
#' @param norm.element chr, data element to be used for normalizing data.elements
#' @param unit.type chr, type of unit to be mapped; acceptable options are "county", "huc", or "aquifer"
#'  
#' @export
#' 
#' @import ggplot2
#' @import maptools
#' @import mapproj
#' @import rgeos
#' @import scales
#' @importFrom ggthemes theme_map
#' @importFrom dplyr left_join
#' @importFrom scales pretty_breaks
#' 
#' @examples 
#' w.use <- wUseSample
#' data.elements <- "PS.WFrTo"
#' norm.element <- "PS.TOPop"
#' year <- 2010 
#' state <- "Delaware" 
#' ch.plot <- choropleth_plot(w.use, data.elements, year, state, norm.element)
#' ch.plot
#' norm.element <- NA
#' ch.plot <- choropleth_plot(w.use, data.elements, year, state, norm.element)
#' ch.plot
#' 
choropleth_plot <- function(w.use, data.elements, year, state, norm.element=NA, unit.type="county"){
  
  if (!is.na(norm.element)){
    data.elements.all <- cbind(data.elements, norm.element)
  } else {
    data.elements.all <- data.elements
  }
  
  if(unit.type=="county"){
    # get county polygons
    hc.sub <- subset_county_polygons(year, "STATE_TERR", state)
    hc.subf<-fortify(hc.sub,region = "FIPS")
    hc.sub@data$id<-hc.sub@data$FIPS
    # wu params for retrieving wu data
    wu.areas <- hc.sub$FIPS
    wu.area.column <- "STATECOUNTYCODE"
  } else if (unit.type=="huc"){
    #get huc polygons
    state <- stateCd$STUSAB[which(stateCd$STATE_NAME == state)]
    hc.sub <- subset_huc_polygons(year, "STATES", state)
    hc.subf<-fortify(hc.sub,region = "HUC8")
    hc.sub@data$id<-hc.sub@data$HUC8
    # wu params for retrieving wu data
    wu.areas <- hc.sub$HUC8
    wu.area.column <- "HUCCODE"
  } else if (unit.type=="aquifer"){
    #get aquifer polygons
  }

  hc.subf <- left_join(hc.subf,hc.sub@data, by="id")
  
  # get water use data
  w.use.sub <- subset_wuse(w.use, data.elements.all, wu.area.column, wu.areas)
  w.use.sub <- w.use.sub[which(w.use.sub$YEAR == year),]# for year of interest

  # normalize
  if (!is.na(norm.element)){
    for (i in data.elements){
      w.use.sub[,paste0(i,"_norm")] <- w.use.sub[,i]/w.use.sub[,norm.element]
    }
  }
  
  # set common key name "id" to merge water use data with polygon data
  if (unit.type=="county") {names(w.use.sub)[names(w.use.sub)=="STATECOUNTYCODE"] <- "id"}
  if (unit.type=="huc") {names(w.use.sub)[names(w.use.sub)=="HUCCODE"] <- "id"}
  
  # merge polygons and water use data to build choropleth dataset
  hc.subf <- left_join(hc.subf,w.use.sub, by="id")

  # plot element
  p.elem <- data.elements
  
  if(all(is.na(w.use.sub[p.elem]))) stop('No data available.')
  
  if (!is.na(norm.element)){p.elem <- paste0(data.elements,"_norm")}
  
  if("COUNTYNAME" %in% names(w.use) & !("COUNTYNAME" %in% names(hc.subf))){
      hc.subf <- left_join(hc.subf, w.use[,c("STATECOUNTYCODE", "COUNTYNAME")], by=c("id"="STATECOUNTYCODE"))
      hc.subf$labels <- paste("Area:",hc.subf$COUNTYNAME, "\n",p.elem,":",hc.subf[[p.elem]])
  } else if(unit.type=="county" & "Area.Name" %in% names(w.use) & !("Area.Name" %in% names(hc.subf))) {
      hc.subf <- left_join(hc.subf, w.use[,c("STATECOUNTYCODE", "Area.Name")], by=c("id"="STATECOUNTYCODE"))
      hc.subf$labels <- paste("Area:",hc.subf$Area.Name, "\n",p.elem,":",hc.subf[[p.elem]])
  } else if(unit.type=="huc" & "Area.Name" %in% names(w.use) & !("Area.Name" %in% names(hc.subf))) {
      hc.subf <- left_join(hc.subf, w.use[,c("HUCCODE", "Area.Name")], by=c("id"="HUCCODE"))
      hc.subf$labels <- paste("Area:",hc.subf$Area.Name, "\n",p.elem,":",hc.subf[[p.elem]])
  } else {
      hc.subf$labels <- paste("Area:",hc.subf$group, "\n",p.elem,":",hc.subf[[p.elem]])    
  }
  
  hc.subf <- hc.subf[order(hc.subf$order), ]
  hc.subf <- unique(hc.subf[,c("long","lat","group","labels",p.elem)])
  hc.subf <- hc.subf[!is.na(hc.subf[,p.elem]),]
  


 
  ch.plot <- ggplot() + geom_polygon(data = hc.subf, 
                 aes_string(x = "long", y = "lat", 
                            group="group", fill= p.elem, label = "labels"),#hc.subf[,p.elem]), 
                 color="black", size=0.25) + 
                 coord_quickmap() + 
                 theme_map() +
                 theme(legend.position=c(.8, .2)) +
                 scale_fill_distiller(name=p.elem, palette = "YlGn", 
                                      breaks = pretty_breaks(n = 5), trans = "reverse")
  
  print(ch.plot)

  return(ch.plot)
  
}