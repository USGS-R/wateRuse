context("Parsing Tests")

test_that("Entered Data", {
  path <- system.file("extdata", package="AWUDS")
  exportData <- parseExport(file.path(path,"Export_2010_County.xlsx"),citation=TRUE)
  
  expect_is(exportData, 'list')
  expect_equal(length(exportData), 13)
  
})

test_that("Compare Data", {

  
})
