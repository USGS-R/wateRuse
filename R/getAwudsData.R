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
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows
#' @importFrom data.table rbindlist
#' @importFrom data.table fread
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#' @importFrom data.table dcast
#' @importFrom utils unzip
#' @importFrom stats as.formula
#' @importFrom stats na.omit
#' @importFrom utils read.table
#' 
#' @examples 
#' awuds.data.path <- system.file("extdata/dump", package="wateRuse")
#' awuds.data <- get_awuds_data(awuds.data.path)
#' 
#' awuds.data.path <- system.file("extdata/excel_test", package="wateRuse")
#' awuds.data <- get_awuds_data(awuds.data.path)
#' 
#' fileList <- c(system.file("extdata/excel_test/Export_2005_County.xlsx", package="wateRuse"),
#' system.file("extdata/excel_test_test/Export_2010_County.xlsx", package="wateRuse"),
#' system.file("extdata/excel_test/Export_2015_County.xlsx", package="wateRuse"))
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
  
  area.names <- c("STATECOUNTYCODE","COUNTYNAME",
    "HUCCODE","Area","Area.Name","USSTATEHUCCODE","HUCNAME",
    "AQUIFERCODE", "USSTATEAQUIFERCODE", "AQUIFERNAME",
    "USSTATECOUNTYAQUIFERCODE", "COUNTYAQUIFERNAME")
  other.names <- c("STUDY","STATECODE","COUNTYCODE",
                   "YEAR","USSTATEALPHACODE","DATASETNAME","BESTAVAILABLE")
  
  for ( check_file in files_to_scan ) {
    if ( grepl('Export.*[1,2][0,9][0-9][0-9].*.xlsx', check_file) ) {
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
      year<-regmatches(file_open,regexpr('[1,2][0,9][0-9][0-9]',file_open))
      if ( !exists('awuds_data') ) {
        awuds_data<-normalize_awuds_excel( new_awuds_data )
        awuds_data$YEAR <- year
        if (any(grepl("Area.Name",names(awuds_data)))){
          awuds_data <- gather_(awuds_data, "data.element","value", names(awuds_data)[!(names(awuds_data) %in% c("YEAR","Area","Area.Name"))])
        }else{
          awuds_data <- gather_(awuds_data, "data.element","value", names(awuds_data)[!(names(awuds_data) %in% c("YEAR","Area"))])
        }
      } else {
        next_awuds_data<-normalize_awuds_excel( new_awuds_data )
        next_awuds_data$YEAR<-year
        if (any(grepl("Area.Name",names(next_awuds_data)))){
          next_awuds_data <- gather_(next_awuds_data, "data.element","value", names(next_awuds_data)[!(names(next_awuds_data) %in% c("YEAR","Area","Area.Name"))])
        }else{
          next_awuds_data <- gather_(next_awuds_data, "data.element","value", names(next_awuds_data)[!(names(next_awuds_data) %in% c("YEAR","Area"))])
        }
        awuds_data <- bind_rows(awuds_data,next_awuds_data)
      }
      # Handle new county-aquifer export files by aggregating (sum) data by aquifer making awuds_data look like an aquifer export file
      # This is a placeholder for now. Perhaps the ideal way to handle this in the future would be to load in as is and have some means of selecting aggregation options (county or aquifer)
      if (grepl(pattern = "CountyAquifer", x = file_open)){
        
        aquiferCodes <- strsplit(awuds_data$Area, split = c("-"), fixed = TRUE)
        awuds_data$Area <- unlist(lapply(aquiferCodes, "[", c(3)))
        
        aquiferNames <- strsplit(awuds_data$Area.Name, split = c("County-"), fixed = TRUE)
        awuds_data$Area.Name <- unlist(lapply(aquiferNames, "[", c(2)))
        
        rm(aquiferCodes, aquiferNames)
        
        awuds_data <- aggregate(value ~ Area + Area.Name + data.element + YEAR, 
          awuds_data, sum, na.action = na.pass)
        
      }
    }

    awuds_data <- unique(awuds_data)

    awuds_data <- spread(awuds_data,key = data.element, value = value)


  } else {
    if(length(dump_file_to_open) > 1){
      idCols <- c(area.names,other.names)

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
      for ( dataCol in names(awuds_data)[!(names(awuds_data) %in% c(area.names,other.names))]) { # Convert all data elements to numeric
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
      normalized_awuds_append <-parseExport_out[[awuds_category]]
      if (any(grepl("Area Name",names(parseExport_out[[awuds_category]])))){normalized_awuds_append <-normalized_awuds_append[,-(which(names(normalized_awuds_append)=="Area Name"))]}
      normalized_awuds <- merge(normalized_awuds, normalized_awuds_append,by='Area')
      # This works, but requires Area to be in the right order.
      # normalized_awuds <- cbind(normalized_awuds, parseExport_out[[awuds_category]][-1])
    }
  }
  names(normalized_awuds) <- make.names(names(normalized_awuds))
  return(normalized_awuds)
}