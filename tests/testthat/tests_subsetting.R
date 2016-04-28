context("Subsetting Tests")

test_that("Entered Data", {
  w.use <- wUseSample
  data.elements <- c("PS.TOPop", "PS.SWPop")
  areas <- "10" # NA uses all areas
  area.column <- "STATECODE"
  w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas)
  
  expect_equal(ncol(w.use.sub), 4)
  expect_equal(nrow(w.use.sub), 18)
  cklist <- c("YEAR",area.column,"PS.TOPop","PS.SWPop")
  expect_equal(names(w.use.sub),cklist)
})
