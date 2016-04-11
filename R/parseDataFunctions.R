
#' data parse functions
#'
#' Parses AWUDS excel files into R data frames.
#' 
#' @param file_path chr, path to the excel file (including file extension)
#' @param citations logical, citations were included as part of the output for Export data
#' 
#' @importFrom readxl read_excel
#' @importFrom readxl excel_sheets
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' 
#' @export
#' 
parseExport <- function(file_path, citations = FALSE){
 sheet_names <- excel_sheets(file_path)
 
 #user-specified = don't parse the metadata sheet
 user <- "Dataset list" %in% sheet_names 
 if(user){
   sheets_to_parse <- sheet_names[-which(sheet_names == "Dataset list")]
 } else {
   sheets_to_parse <- sheet_names
 }

 parsed_data <- lapply(sheets_to_parse, function(sheet, path, citations){
   
   if(citations){
     all_df <- read_excel(path, sheet, skip = 1)
   } else {
     all_df <- read_excel(path, sheet)
   }
   
   # remove notes that appear at bottom of reports 
   notes_pattern <- "[:digit:\\)]"
   which_rows_notes <- grep(notes_pattern, all_df[[1]])
   if(length(which_rows_notes) != 0) {
     df <- all_df[-which_rows_notes,]
     metadata <- list(Notes = as.list(unname(all_df[which_rows_notes,1])))
     attr(df, 'Notes') <- metadata
   } else {
     df <- all_df
   }

   df <-  all_df %>%
     removeDuplicateColumns %>% 
     filter(!is.na(select(., 1))) # remove any rows of all NA values

   return(df)
 }, path = file_path, citations = citations)
 names(parsed_data) <- sheets_to_parse
 
 if(user){
   metadata <- read_excel(path, sheet = which(sheet_names == "Dataset list"))
   attr(parsed_data, 'Datasets') <- na.omit(metadata)
 }
 
 return(parsed_data)
}

#' @export
#' 
parseEnteredElements <- function(file_path){
 all_data <- read_excel(path = file_path, sheet = 1)
 
 # format metadata from top of excel file
 population_info <- as.character(as.vector(all_data[2,1:2]))
 metadata_description <- all_data[1:5, 1]
 metadata_description[2,] <- paste(population_info, collapse = " ")
 metadata_aging_counts <- all_data[1:5, c(15,16)]
 names(metadata_aging_counts) <- c('Data Aging', 'Counts')
 metadata <- list(Descriptive = metadata_description,
                  Aging_counts = metadata_aging_counts)
 
 # format actual data
 df <- read_excel(path = file_path, sheet = 1, skip = 7)
 df <- df[, -c(15,16)]
 df <- df %>%
   filter(!is.na(`Data Element`)) %>%
   rename(`Thermoelectric: Once-Through Cooling` = `Once-Through Cooling`,
          `Thermoelectric: Closed-Loop Cooling` = `Closed-Loop Cooling`,
          `Hydroelectric: Instream` = Instream,
          `Hydroelectric: Offstream` = Offstream)
 attributes(df) <- append(attributes(df), metadata)
 return(df)
}

#' @export
#' 
parseCompareData <- function(file_path){
 sheet_names <- excel_sheets(file_path)
 parsed_data <- lapply(sheet_names, function(sheet, path, skip){
   all_df <- read_excel(path, sheet)
   metadata <- na.omit(names(all_df))
   
   #grab first occurrence of completely filled row, these are real column headers
   col_names_location <- which(complete.cases(all_df))[1] 
   names(all_df) <- all_df[col_names_location, ]
   
   df <- all_df %>%
     filter(!is.na(select(., 1))) %>% #first column should never be empty (it's county or state)
     filter(select(., 1) != names(all_df)[1]) #if the first col == column name, get rid of it
   
   return(df)
 }, path = file_path)
 names(parsed_data) <- sheet_names
 return(parsed_data)
}

parseDumpFiles <- function(file_path){
  read.table(file_path, header = TRUE, sep = '\t',
             quote = NULL, comment = "")
}

removeDuplicateColumns <- function(df){
  duplicate_columns <- which(duplicated(names(df)))
  if(length(duplicate_columns) > 0){
    df <- df[, -duplicate_columns]
  }
  return(df)
}
