#' get_awuds_data
#'
#' Get data from local excel export or dump file.
#' 
#' @param awuds.data.path character, path to files
#' @return \code{data.frame} containing AWUDS data elements
#' 
#' @export
#' 
#' @examples 
#' awuds.data.path <- system.file("extdata", package="wateRuse")
#' awuds.data <- get_awuds_data(awuds.data.path)
get_awuds_data <- function(awuds.data.path = NA) {
  if ( !is.na(awuds.data.path) ) {
    files_to_scan <- list.files(path=awuds.data.path)
    files_to_open <- c()
    for ( check_file in files_to_scan ) {
      if ( grepl('Export.*[1,2][0,9][0-9][0-5].*.xlsx', check_file) ) {
        files_to_open <- c(files_to_open, check_file)
      }
    }
    for ( file_open in files_to_open ) {
      new_awuds_data <- parseExport(file.path(awuds.data.path, file_open), citations = TRUE)
      year<-regmatches(file_open,regexpr('[1,2][0,9][0-9][0-5]',file_open))
      if ( !exists('awuds_data') ) {
        awuds_data<-normalize_awuds_excel( new_awuds_data )
        awuds_data['Year'] <- rep(x = year, times = nrow(awuds_data))
      } else {
        next_awuds_data<-normalize_awuds_excel( new_awuds_data )
        next_awuds_data['Year']<-rep(x = year, times = nrow(next_awuds_data))
        awuds_data<-merge(awuds_data,next_awuds_data, all.y=TRUE)
      }
    }
  } else {
    stop('Must provide the folder where AWUDS Excel export files or dump file(s) are stored.')
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
  return(normalized_awuds)
}