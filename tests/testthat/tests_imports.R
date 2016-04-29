context("Parsing Tests")

test_that("Entered Data", {
  path <- system.file("extdata/excel", package="wateRuse")
  exportData <- parseExport(file.path(path,"Export_2010_County.xlsx"),citation=TRUE)
  
  expect_is(exportData, 'list')
  expect_equal(length(exportData), 13)
  
})

test_that("Compare Data", {

  
})

test_that("Excel Data Gets Normalized", {
  path <- system.file("extdata/excel", package="wateRuse")
  awuds <- get_awuds_data(path)
  expect_equal(nrow(awuds),63)
  expect_equal(ncol(awuds),98)
})

test_that("Dump Data Gets Normalized", {
  path <- system.file("extdata/dump", package="wateRuse")
  awudsdump <- get_awuds_data(path)
  expect_equal(nrow(awudsdump),48)
  expect_equal(ncol(awudsdump),286)
})

test_that("Excel Data Gets Normalized When Given List of Files", {
  fileList <- c(system.file("extdata/excel/Export_2005_County.xlsx", package="wateRuse"),
             system.file("extdata/excel/Export_2010_County.xlsx", package="wateRuse"),
             system.file("extdata/excel/Export_2015_County.xlsx", package="wateRuse"))
  awuds <- get_awuds_data(awuds.data.files=fileList)
  expect_equal(nrow(awuds),21)
  expect_equal(ncol(awuds),98)
})

test_that("Dump Data gets read in when given as a direct file", {
  file <- c(system.file("extdata/dump/exampleAWUDSdump.txt", package="wateRuse"))
  awuds <- get_awuds_data(awuds.data.files=file)
  expect_equal(nrow(awuds),48)
  expect_equal(ncol(awuds),286)
})

