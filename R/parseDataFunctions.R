
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
}
 
#' AWUDS
#'
#' \tabular{ll}{
#' Package: \tab AWUDS\cr
#' Type: \tab Package\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' http://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Collection of functions to do USGS graphics.
#'
#' @name AWUDS-package
#' @docType package
NULL


# parseExport <- function(file_path, citations = FALSE){
#   sheet_names <- excel_sheets(file_path)
#   
#   user <- "Dataset list" %in% sheet_names #is it user-specified
#   
#   parsed_data <- lapply(sheet_names, function(sheet, path){
#     
# 
#     
#     all_df <- read_excel(path, sheet)
#     which_rows_notes <- grep("[:digit:\\)]", all_df[[1]])
#     metadata <- list(Description = strsplit(names(all_df)[1], ','),
#                      Notes = as.list(unname(all_df[which_rows_notes,1])))
#     
#     df <- all_df[-1,]
#     names(df) <- all_df[1,]
#     df <- df %>% 
#       filter(!is.na(Area)) %>% 
#       filter(!grepl("[:digit:\\)]", Area))
#     
#     return(df)
#   }, path = file_path)
#   names(parsed_data) <- sheet_names
#   
#   return(parsed_data)
# }


# parseExportUser <- function(file_path){
#   sheet_names <- excel_sheets(file_path)
#   sheet_info <- which(sheet_names == "Dataset list")
#   parsed_data <- lapply(sheet_names[-sheet_info], function(sheet, path){
#     all_df <- read_excel(path, sheet, na = "--")
#     metadata <- list(Description = strsplit(names(all_df)[1], ','))
#     
#     df <- all_df[-1,]
#     names(df) <- all_df[1,]
#     duplicate_columns <- which(duplicated(names(df)))
#     if(length(duplicate_columns) > 0){
#       df <- df[, -duplicate_columns]
#     }
#     
#     df <- df %>% 
#       filter(!is.na(STATECODE)) 
#     
#     return(df)
#   }, path = file_path)
#   
#   info_data <- read_excel(file_path, sheet = sheet_info) %>% na.omit() %>% as.list
#   attributes(info_data) <- NULL
#   names(info_data) <- "Dataset list"
#   
#   names(parsed_data) <- sheet_names[-sheet_info]
#   
#   parsed_all <- append(parsed_data, info_data)
#   return(parsed_all)
# }

# parseEnteredElements <- function(file_path){
#   all_data <- read_excel(path = file_path, sheet = 1)
#   
#   # format metadata from top of excel file
#   population_info <- as.character(as.vector(all_data[2,1:2]))
#   metadata_description <- all_data[1:5, 1]
#   metadata_description[2,] <- paste(population_info, collapse = " ")
#   
#   metadata_aging_counts <- all_data[1:5, c(15,16)]
#   names(metadata_aging_counts) <- c('Data Aging', 'Counts')
#   
#   metadata <- list(Descriptive = metadata_description,
#                    Aging_counts = metadata_aging_counts)
#   
#   # format actual data
#   df <- read_excel(path = file_path, sheet = 1, skip = 7)
#   df <- df[, -c(15,16)]
#   df <- df %>% 
#     filter(!is.na(`Data Element`)) %>% 
#     rename(`Thermoelectric: Once-Through Cooling` = `Once-Through Cooling`,
#            `Thermoelectric: Closed-Loop Cooling` = `Closed-Loop Cooling`,
#            `Hydroelectric: Instream` = Instream, 
#            `Hydroelectric: Offstream` = Offstream)
# 
#   attributes(df) <- append(attributes(df), metadata)
#   return(df)
# }

# parseCompareData <- function(file_path){
#   
#   sheet_names <- excel_sheets(file_path)
#   parsed_data <- lapply(sheet_names, function(sheet, path, skip){
#     all_df <- read_excel(path, sheet)
#     metadata <- na.omit(names(all_df))
#     col_names_location <- which(complete.cases(all_df))[1] #grab first occurrence of completely filled row, these are real column headers
#     names(all_df) <- all_df[col_names_location, ]
#     df <- all_df %>% 
#       filter(!is.na(select(., 1))) %>% #first column should never be empty (it's county or state)
#       filter(select(., 1) != names(all_df)[1]) #if the first col == column name, get rid of it
#     
#     return(df)
#   }, path = file_path)
#   names(parsed_data) <- sheet_names
#   return(parsed_data)
# }
