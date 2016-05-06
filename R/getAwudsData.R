#' get_awuds_data
#'
#' Get data from local excel export or dump file.
#' 
#' @param awuds.data.path character, path to files to try to load.
#' @param awuds.data.files character, a vector of files to try to load.
#' @return \code{data.frame} containing AWUDS data elements
#' 
#' @export
#' @importFrom tidyr gather_
#' @importFrom tidyr spread_
#' @importFrom data.table rbindlist
#' @importFrom data.table fread
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#' @importFrom data.table dcast
#' 
#' @examples 
#' awuds.data.path <- system.file("extdata/dump", package="wateRuse")
#' awuds.data <- get_awuds_data(awuds.data.path)
#' 
#' awuds.data.path <- system.file("extdata/excel", package="wateRuse")
#' awuds.data <- get_awuds_data(awuds.data.path)
#' 
#' fileList <- c(system.file("extdata/excel/Export_2005_County.xlsx", package="wateRuse"),
#' system.file("extdata/excel/Export_2010_County.xlsx", package="wateRuse"),
#' system.file("extdata/excel/Export_2015_County.xlsx", package="wateRuse"))
#' awuds.data <- get_awuds_data(awuds.data.files=fileList)
#' 
#' file <- c(system.file("extdata/dump/exampleAWUDSdump.txt", package="wateRuse"))
#' awuds.data <- get_awuds_data(awuds.data.files=file)
#' 
get_awuds_data <- function(awuds.data.path = NA, awuds.data.files = NA) {

  if ( !is.na(awuds.data.path) ) {
    if ( !dir.exists(awuds.data.path) ) stop('Did not recieve a valid path.')
    files_to_scan <- list.files(path=awuds.data.path,full.names = TRUE)
  } else if ( is.vector(awuds.data.files) ) {
    awuds.data.files.new <- gsub(", ","_", awuds.data.files)
    file.rename(awuds.data.files,awuds.data.files.new)
    
    if ( !file.exists(awuds.data.files.new[1]) || !is.vector(awuds.data.files.new) ) {
      stop('Did not get a valid file.')
    }
    
    files_to_scan <- awuds.data.files.new
  } else {
    stop('Must provide the folder where AWUDS Excel export files or dump file(s) are stored.')
  }
  
  for ( check_file in files_to_scan ) {
    if ( grepl('Export.*[1,2][0,9][0-9][0-5].*.xlsx', check_file) ) {
      if ( !exists('files_to_open')) files_to_open <- c()
      files_to_open <- c(files_to_open, file.path(check_file))
    } else if ( grepl('.*.txt',check_file) ) {
      if ( exists('dump_file_to_open') ) {
        stop('Found more than one dump file at the path given, only one is supported.')
      }
      dump_file_to_open <- check_file
    } else if ( grepl('.*zip',check_file) ) {
      tempFolder <- tempdir()
      
      fileNames <- unzip(check_file, exdir = tempFolder, list = TRUE)
      unzip(check_file, exdir = tempFolder)
      dump_file_to_open <- fileNames$Name
    }
  }
  
  if( !exists('dump_file_to_open') && !exists('files_to_open') ) {
    stop('No excel or dump files found.')
  }
  
  if ( exists('files_to_open') ) {
    for ( file_open in files_to_open ) {
      new_awuds_data <- parseExport(file_open, citations = TRUE)
      year<-regmatches(file_open,regexpr('[1,2][0,9][0-9][0-5]',file_open))
      if ( !exists('awuds_data') ) {
        awuds_data<-normalize_awuds_excel( new_awuds_data )
        awuds_data$YEAR <- year
        awuds_data <- gather_(awuds_data, "data.element","value", names(awuds_data)[!(names(awuds_data) %in% c("YEAR","Area"))])
      } else {
        next_awuds_data<-normalize_awuds_excel( new_awuds_data )
        next_awuds_data$YEAR<-year
        next_awuds_data <- gather_(next_awuds_data, "data.element","value", names(next_awuds_data)[!(names(next_awuds_data) %in% c("YEAR","Area"))])
        awuds_data<-rbind(awuds_data,next_awuds_data)
      }
    }
    
    awuds_data <- dcast(setDT(awuds_data), as.formula(paste(paste(names(awuds_data)[!(names(awuds_data) %in% c("data.element","value"))],collapse = "+"),"~ data.element")), value.var = "value")
    awuds_data <- setDF(awuds_data)
    # awuds_data <- spread_(awuds_data, "data.element","value")

  } else {
    if(length(dump_file_to_open) > 1){
      idCols <- c("STUDY","DATASETNAME","BESTAVAILABLE","USSTATEALPHACODE",
                  "STATECODE","COUNTYCODE","STATECOUNTYCODE","COUNTYNAME","YEAR",
                  "HUCCODE","Area","USSTATEHUCCODE","HUCNAME")

      totalRows <- 0
      rowVector <- rep(NA, length(dump_file_to_open))
      
      awuds_data <- list()
      colNames <- c()
      
      for(i in dump_file_to_open){
        subData <- fread(file.path(tempFolder,i), na.strings="--", colClasses="character")
        subData <- as.data.frame(lapply(subData, function(x) {gsub("na", "NaN", x)}), stringsAsFactors=FALSE)
        subData[!(names(subData) %in% idCols)] <- lapply(subData[!(names(subData) %in% idCols)], function(x) as.numeric(x))
        subData <- gather_(subData, "data.element","value", names(subData)[!(names(subData) %in% idCols)])
        
        colNames <- c(colNames, names(subData)[!(names(subData) %in% colNames)])

        awuds_data <- rbindlist(list(awuds_data, subData), fill=TRUE)

      }
      idColNames <- names(awuds_data)[(names(awuds_data) %in% idCols)]
      
      awuds_data <- dcast(setDT(awuds_data), as.formula(paste(paste(idColNames,collapse = "+"),"~ data.element")), value.var = "value")
      awuds_data <- setDF(awuds_data)
      # awuds_data <- spread_(awuds_data, "data.element","value")
      
    } else {
      awuds_data<-fread(dump_file_to_open, na.strings="--", colClasses="character")
      awuds_data <- as.data.frame(lapply(awuds_data, function(x) {gsub("na", "NaN", x)}), stringsAsFactors=FALSE)
      for ( dataCol in names(awuds_data)[9:length(names(awuds_data))]) { # Convert all data elements to numeric
        awuds_data[[dataCol]]<-as.numeric(awuds_data[[dataCol]])
      }
    }
  }
  

  return(awuds_data)
}

normalize_awuds_excel<-function(parseExport_out) {
  for ( awuds_category in names(parseExport_out) ) {
    if ( !exists('normalized_awuds') ) {
      normalized_awuds <- parseExport_out[[awuds_category]]
    } else {
      normalized_awuds <- merge(normalized_awuds, parseExport_out[[awuds_category]],by='Area')
      # This works, but requires Area to be in the right order.
      # normalized_awuds <- cbind(normalized_awuds, parseExport_out[[awuds_category]][-1])
    }
  }
  names(normalized_awuds) <- make.names(names(normalized_awuds))
  return(normalized_awuds)
}