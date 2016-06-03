## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
data.elements <- c("PS.TOPop", "PS.SWPop")
areas <- "15" # NA uses all areas
area.column <- "STATECODE"
year.x.y <- c(2005,2010)
compare_two_years(w.use, "PS.TOPop", year.x.y, area.column, areas)


## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
data.elements <- c("PS.TOPop", "PS.SWPop")
areas <- "10" # NA uses all areas
area.column <- "STATECODE"
year.x.y <- c(2005,2010)
compare_two_years(w.use, data.elements, year.x.y, area.column, areas)


## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
data.elements <- "PS.TOPop"
areas <- c("10","15") # NA uses all areas
area.column <- "STATECODE"
year.x.y <- c(2005,2010)
compare_two_years(w.use, data.elements, year.x.y, area.column, areas)


## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
data.elements.x.y <- c("TP.TotPop", "PS.WSWFr")
areas <- "15" # NA uses all areas
area.column <- "STATECODE"
years <- c(2000, 2005, 2010)
compare_two_elements(w.use, data.elements.x.y, years, area.column, areas)


## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
compare_two_elements(w.use, c("TP.TotPop","PS.WSWFr"), "2010", "STATECODE", c("10","15"))

## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
areas <- c("Kent County","Sussex County")
area.column = "COUNTYNAME"
data.elements <- c("PS.GWPop","TP.TotPop")
multi_element_data(w.use, data.elements, area.column = area.column,areas = areas)


## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
areas <- c("Kent County","Sussex County")
area.column = "COUNTYNAME"
data.elements <- c("PS.GWPop","TP.TotPop")
multi_element_data(w.use, data.elements, plot.points = FALSE,
      area.column = area.column, areas = areas)


## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
areas <- "15"
area.column = "STATECODE"
data.elements <- c("PS.GWPop","TP.TotPop")
boxplot_wu(w.use, data.elements, area.column = area.column,areas = areas)

## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
areas <- "15"
area.column = "STATECODE"
data.elements <- c("PS.GWPop","TP.TotPop")
boxplot_wu(w.use, data.elements, area.column, areas=NA, log=TRUE)


## ----message=FALSE-------------------------------------------------------
df <- wUseSample
areas <- c("Kent County","Sussex County")
area.column = "COUNTYNAME"
data.elements <- c("PS.GWPop","TP.TotPop")
time_series_data(w.use, data.elements, area.column = area.column,areas = areas)

## ----message=FALSE-------------------------------------------------------
df <- wUseSample
areas <- c("Kent County","Sussex County")
area.column = "COUNTYNAME"
data.elements <- c("PS.GWPop","TP.TotPop")
time_series_data(w.use, data.elements, plot.points = FALSE, area.column = area.column,areas = areas)


## ----message=FALSE-------------------------------------------------------
df <- wUseSample
areas <- c("Hawaii County","Honolulu County","Kauai County")
area.column = "COUNTYNAME"
data.elements <- c("PS.GWPop","TP.TotPop")
time_series_data(w.use, data.elements, plot.points = FALSE,
       area.column = area.column, areas = areas, legend=TRUE, years = c("1995", "2000", "2005"))


## ----message=FALSE-------------------------------------------------------
df <- wUseSample
areas <- c("Hawaii County","Honolulu County","Kauai County")
area.column = "COUNTYNAME"
data.elements <- c("PS.GWPop","TP.TotPop")
time_series_data(w.use, data.elements, area.column, areas = areas, plot.points = FALSE, y.scale = c(0,1000))


## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
areas <- c("New Castle County", "Kent County")
area.column = "COUNTYNAME"
data.elements <- c("PS.WTotl","CO.WTotl","DO.WTotl","IN.WTotl","PF.WTotl")
barchart_sums(w.use, data.elements, area.column = area.column,areas = areas)


## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
areas <- c("New Castle County", "Kent County")
area.column = "COUNTYNAME"
data.elements <- c("PS.WTotl","CO.WTotl","DO.WTotl","IN.WTotl","PF.WTotl")
barchart_sums(w.use, data.elements, plot.stack = FALSE, area.column = area.column,areas = areas)

## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
areas <- c("New Castle County", "Kent County")
area.column = "COUNTYNAME"
data.elements <- c("PS.WTotl","CO.WTotl","DO.WTotl","IN.WTotl","PF.WTotl")
barchart_sums(w.use, data.elements, area.column, areas=areas, y.scale = c(0,500))

## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
data.elements <- "PS.WFrTo"
year <- 2010 
areas <- "Hawaii" 
area.column <- "STATE_TERR"
ch.plot <- choropleth_plot(w.use, data.elements, year, areas, area.column)

## ----message=FALSE-------------------------------------------------------
w.use <- wUseSample
data.elements <- "PS.WFrTo"
norm.element <- "PS.TOPop"
year <- 2010 
areas <- "Hawaii" 
area.column <- "STATE_TERR"
ch.plot <- choropleth_plot(w.use, data.elements, year, areas, area.column, norm.element)

