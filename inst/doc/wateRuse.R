## ----message=FALSE-------------------------------------------------------
library(wateRuse)
w.use <- wUseSample
areas <- "10" # NA uses all areas
area.column <- "STATECODE"
year.x.y <- c(2005,2010)
compare_two_years(w.use, "PS.TOPop", year.x.y)



## ----message=FALSE-------------------------------------------------------
areas <- c("Kent County","Sussex County")
area.column = "COUNTYNAME"
data.elements <- c("PS.GWPop","PS.SWPop")
time_series_data(w.use, data.elements, area.column = area.column,areas = areas)


