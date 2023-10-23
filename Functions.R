###-----------------------------------------------------------------------###
#-----            Reading and writing excel files                       -----         
###-----------------------------------------------------------------------###

#Reads a file a returns a List with one dataframe for each excel sheet
read_doc_and_save_df <- function(file_path) {
  sheets <- excel_sheets(file_path)
  tibble_list <- lapply(sheets, function(x) read_excel(file_path, sheet = x))
  # Convert each sheet to a data frame
  DT <- lapply(tibble_list, as.data.frame)
  # Set names for the list elements based on sheet names
  names(DT) <- sheets
  return(DT)
}

read_specific_sheets_and_save_df <- function(file_path, sheet_names) {
  tibble_list <- lapply(sheet_names, function(x) read_excel(file_path, sheet = x))
  dfs <- lapply(tibble_list, as.data.frame)
  # Set names for the data frames based on the sheet names
  names(dfs) <- sheet_names
  return(dfs)
}

###-----------------------------------------------------------------------###

#-----            Cleaning  Functions                                   -----         
###-----------------------------------------------------------------------###

clean_column_names <- function(names) {
  # Use a regular expression to find positions between consecutive uppercase and lowercase letters to solve for example BorrowerName
  pattern <- "(?<=[a-z])(?=[A-Z])"
  names <- gsub(pattern, ".", names, perl = TRUE)
  
  # Make.names handles other invalid characters and ensures uniqueness
  names <- make.names(names, unique = TRUE)
  # Use gsub to replace consecutive dots with a single dot and remove trailing dots
  names <- gsub("\\.{2,}", ".", names)
  names <- gsub("\\.$", "", names)
  
  # Convert all characters to lowercase
  names <- tolower(names)
  
  return(names)
}
# Running example:
# colnames(df) <- clean_column_names(colnames(df))


# Function to split values, and create two rows when there's one with multiple values divided by a separator
divide_column_by_character <- function(dataframe, column_name, separator) {
  dataframe %>%
    mutate(across({{column_name}}, ~ ifelse(is.na(.), "NA", .))) %>%
    rowwise() %>%
    separate_rows({{column_name}}, sep = separator, convert = TRUE) %>%
    mutate_all(~str_trim(., 'left')) %>% mutate_all(~str_trim(., 'right'))
}
# Running example:
# NDG_N <- divide_column_by_character(NDG_N, "Judicial.Procedures.CODE", "\\+ ")