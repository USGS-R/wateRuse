context("Subsetting Tests")

test_that("Entered Data", {
  w.use <- wUseSample
  data.elements <- c("PS.TOPop", "PS.SWPop")
  areas <- "10" # NA uses all areas
  area.column <- "STATECODE"
  w.use.sub <- subset_wuse(w.use, data.elements,areas, area.column)
  
  expect_equal(ncol(w.use.sub), 5)
  expect_equal(nrow(w.use.sub), 18)
  cklist <- c("YEAR","PS.TOPop","PS.SWPop","STATECOUNTYCODE","COUNTYNAME")
  labs <- labels(w.use.sub)
  expect_equal(labs[[2]],cklist)
})
