context("Parsing Tests")

test_that("Entered Data", {
  path <- system.file("extdata", package="wateRuse")
  exportData <- parseExport(file.path(path,"Export_2010_County.xlsx"),citation=TRUE)
  
  expect_is(exportData, 'list')
  expect_equal(length(exportData), 13)
  
})

test_that("Compare Data", {

  
})

test_that("Excel Data Gets Normalized", {
  path <- system.file("extdata", package="wateRuse")
  awuds <- get_awuds_data(path)
  expect_equal(nrow(awuds),21)
  expect_equal(ncol(awuds),98)
})
