context("Subsetting Tests")

test_that("Entered Data", {
  w.use <- wUseSample
  data.elements <- c("PS.TOPop", "PS.SWPop")
  areas <- "10" # NA uses all areas
  area.column <- "STATECODE"
  w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas)
  
  expect_equal(ncol(w.use.sub), 5)
  expect_equal(nrow(w.use.sub), 18)
  cklist <- c("YEAR","STATECOUNTYCODE",area.column,"PS.TOPop","PS.SWPop")
  expect_equal(names(w.use.sub),cklist)
})

test_that("Subset polygon data", {
  areas <- "Maine"
  area.column <- "STATE_TERR"
  year <- 2010 # 16 counties in Maine present day
  hc.sub <- subset_county_polygons(areas, area.column, year)

  expect_equal(ncol(hc.sub), 17)
  expect_equal(nrow(hc.sub), 16)
  unq.names <- unique(hc.sub$NAME)
  expect_equal(length(unq.names), 16)

  year <- 1850 # 13 counties in Maine in 1850
  hc.sub <- subset_county_polygons(areas, area.column, year)
  
  expect_equal(ncol(hc.sub), 17)
  expect_equal(nrow(hc.sub), 13)
  unq.names <- unique(hc.sub$NAME)
  expect_equal(length(unq.names), 13)
})
