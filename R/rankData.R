#' compare.data.elements
#'
#' Compare multiple files
#' 
#' @param file_path chr, path to file.1 and file.2 
#' @param file.1 character name of first file
#' @param file.2 character name of second file
#' @param data.element character name of data element
#' 
#' 
#' @export
#' 
#' @examples 
#' folderPath <- system.file("extdata", package="wateRuse")
#' file.1 <- "Export_2010_County.xlsx"
#' file.2 <- "Export_2015_County.xlsx"
#' data.element <- "TP"
#' df <- compare.data.elements(folderPath, file.1, file.2, data.element)
compare.data.elements <- function(file_path, file.1, file.2, data.element){
  
  data.1 <- parseExport(file.path(file_path, file.1), citations = TRUE)
  data.2 <- parseExport(file.path(file_path, file.2), citations = TRUE)
  
  data.1.df <- data.1[[data.element]]
  data.2.df <- data.2[[data.element]]
  
  return(data.1.df)
  
}