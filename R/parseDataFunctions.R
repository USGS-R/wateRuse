#' data parse functions
#'
#' Parses AWUDS excel files into R data frames.
#' 
#' @param file_path chr, path to the excel file (including file extension)
#' @param citations logical, citations were included as part of the output for Export data
#' 
#' @importFrom readxl read_excel
#' @importFrom readxl excel_sheets
#' @rdname parser
#' 
#' @export
#' 
#' @examples 
#' folderPath <- system.file("extdata", package="AWUDS")
#' exportData <- parseExport(file.path(folderPath,"Export_2010_County.xlsx"),citation=TRUE)
#' TP <- exportData[["TP"]]
#' PO <- exportData[["PO"]]
#' 
#' exportData2010 <- parseExport(file.path(folderPath,"Import_2010_County-3_0805A.xlsx"),citation=TRUE)
#' LI <- exportData2010[["LI"]]
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

   df <- removeDuplicateColumns(df)
   df <- removeAllNARows(df)

   return(df)
 }, path = file_path, citations = citations)
 names(parsed_data) <- sheets_to_parse
 
 if(user){
   metadata <- read_excel(file_path, sheet = which(sheet_names == "Dataset list"))
   attr(parsed_data, 'Datasets') <- na.omit(metadata)
 }
 
 return(parsed_data)
}

#' @export
#' @rdname parser
#' 
#' @examples 
#' path <- system.file("extdata", package="AWUDS")
#' enteredData <- parseEnteredElements(file.path(path,"Entered-Data_2005.xlsx"))
parseEnteredElements <- function(file_path){
 all_data <- read_excel(path = file_path, sheet = 1)
 
 # format metadata from top of excel file
 population_info <- as.character(as.vector(all_data[2,1:2]))
 metadata_description <- all_data[1:5, 1]
 metadata_description[2] <- paste(population_info, collapse = " ")
 metadata_aging_counts <- all_data[1:5, c(15,16)]
 names(metadata_aging_counts) <- c('Data Aging', 'Counts')
 metadata <- list(Descriptive = metadata_description,
                  Aging_counts = metadata_aging_counts)
 
 # format actual data
 df <- read_excel(path = file_path, sheet = 1, skip = 7)
 df <- df[, which(!is.na(names(df)))] #removing columns that are all NA
 df <- removeAllNARows(df)
 
 #rename columns that have an upstream name
 names(df) <- unlist(lapply(names(df), function(orig_col_name) {
   renamed_col <- switch(orig_col_name, 
                         `Once-Through Cooling` = 'Thermoelectric: Once-Through Cooling', 
                         `Closed-Loop Cooling` = 'Thermoelectric: Closed-Loop Cooling', 
                         Instream = 'Hydroelectric: Instream', 
                         Offstream = 'Hydroelectric: Offstream')
   col_name <- ifelse(!is.null(renamed_col), renamed_col, orig_col_name)
   return(col_name)
 }))

 attributes(df) <- append(attributes(df), metadata)
 return(df)
}

#' @export
#' @rdname parser
#' 
#' @examples 
#' path <- system.file("extdata", package="AWUDS")
#' compareData <- parseCompareData(file.path(path, "CompareData.xlsx"))
parseCompareData <- function(file_path){
 sheet_names <- excel_sheets(file_path)
 parsed_data <- lapply(sheet_names, function(sheet, path, skip){
   
   all_df <- read_excel(path, sheet)
   metadata <- na.omit(names(all_df))
   
   #grab first occurrence of completely filled row, these are real column headers
   col_names_location <- which(complete.cases(all_df))[1] 
   names(all_df) <- all_df[col_names_location, ]
   
   df <- removeAllNARows(all_df)
   df <- df[which(df[,1] != names(df)[1]),] # remove duplicated column names that appear throughout data

   return(df)
 }, path = file_path)
 names(parsed_data) <- sheet_names
 return(parsed_data)
}

parseDumpFiles <- function(file_path){
  read.table(file_path, header = TRUE, sep = '\t',
             quote = NULL, comment.char = "")
}

removeDuplicateColumns <- function(df){
  duplicate_columns <- which(duplicated(names(df)))
  if(length(duplicate_columns) > 0){
    df <- df[, -duplicate_columns]
  }
  return(df)
}

removeAllNARows <- function(df){
  col1 <- df[[1]]
  noNA_df <- df[!is.na(col1),]
  return(noNA_df)
}
