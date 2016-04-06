# data parsing functions

library(readxl)
library(dplyr)

parseExport <- function(file_path){
  sheet_names <- excel_sheets(file_path)
  parsed_data <- lapply(sheet_names, function(sheet, path){
    all_df <- read_excel(path, sheet)
    which_rows_notes <- grep("[:digit:\\)]", all_df[[1]])
    metadata <- list(Description = strsplit(names(all_df)[1], ','),
                     Notes = as.list(unname(all_df[which_rows_notes,1])))
    
    df <- all_df[-1,]
    names(df) <- all_df[1,]
    df <- df %>% 
      filter(!is.na(Area)) %>% 
      filter(!grepl("[:digit:\\)]", Area))
    
    return(df)
  }, path = file_path)
  names(parsed_data) <- sheet_names
  
  return(parsed_data)
}

parseExportUser <- function(file_path){
  sheet_names <- excel_sheets(file_path)
  sheet_info <- which(sheet_names == "Dataset list")
  parsed_data <- lapply(sheet_names[-sheet_info], function(sheet, path){
    all_df <- read_excel(path, sheet, na = "--")
    metadata <- list(Description = strsplit(names(all_df)[1], ','))
    
    df <- all_df[-1,]
    names(df) <- all_df[1,]
    duplicate_columns <- which(duplicated(names(df)))
    if(length(duplicate_columns) > 0){
      df <- df[, -duplicate_columns]
    }
    
    df <- df %>% 
      filter(!is.na(STATECODE)) 
    
    return(df)
  }, path = file_path)
  
  info_data <- read_excel(file_path, sheet = sheet_info) %>% na.omit() %>% as.list
  attributes(info_data) <- NULL
  names(info_data) <- "Dataset list"
  
  names(parsed_data) <- sheet_names[-sheet_info]
  
  parsed_all <- append(parsed_data, info_data)
  return(parsed_all)
}

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

parseCompareData <- function(file_path){
  
  sheet_names <- excel_sheets(file_path)
  parsed_data <- lapply(sheet_names, function(sheet, path, skip){
    all_df <- read_excel(path, sheet)
    metadata <- na.omit(names(all_df))
    col_names_location <- which(complete.cases(all_df))[1] #grab first occurrence of completely filled row, these are real column headers
    names(all_df) <- all_df[col_names_location, ]
    df <- all_df %>% 
      filter(!is.na(select(., 1))) %>% #first column should never be empty (it's county or state)
      filter(select(., 1) != names(all_df)[1]) #if the first col == column name, get rid of it
    
    return(df)
  }, path = file_path)
  names(parsed_data) <- sheet_names
  return(parsed_data)
}

parseBasicTables <- function(file_path){
  
  sheet_names <- excel_sheets(file_path)
  parsedExcelSheets <- lapply(sheet_names, read_excel, path = file_path)
  names(parsedExcelSheets) <- sheet_names
  
}
