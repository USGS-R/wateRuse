## ----message=FALSE-------------------------------------------------------
library(wateRuse)
w.use <- wUseSample
areas <- "10" # NA uses all areas
area.column <- "STATECODE"
year.x.y <- c(2005,2010)
plot.two <- compare_two_years(w.use, "PS.TOPop", year.x.y)
plot.two


