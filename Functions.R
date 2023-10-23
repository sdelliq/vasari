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

###-----------------------------------------------------------------------###
#-----                Entity Table Functions                        -----         
###-----------------------------------------------------------------------###
clean_cf.piva <- function(column) {
  column <- gsub(" ", "", column)
  column <- ifelse(str_length(column) == 10, paste0(0, column), column)
  return(column)
}
# Running example:
#ENTITIES$cf.piva <- clean_cf.piva(ENTITIES$cf.piva)

# Define the age categories based on the age column
add_age_range_column <- function(data) {
  breaks <- c(0, 25, 50, 65, 75, Inf)
  labels <- c("0-25", "25-50", "50-65", "65-75", "75+")
  result <- data %>%
    mutate(
      range.age = cut(age, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}
# Running example:
#ENTITIES <- add_age_range_column(ENTITIES)

add_sex_column <- function(data) {
  result <- data %>%
    mutate(sex = case_when(
      !is.na(type.subject) & type.subject == "individual" & as.numeric(str_sub(cf.piva, start = 10L, end = 11L)) > 40 ~ "f",
      !is.na(type.subject) & type.subject == "individual" & as.numeric(str_sub(cf.piva, start = 10L, end = 11L)) <= 40 ~ "m",
      TRUE ~ NA_character_
    ))
  return(result)
}
# Running example:
# ENTITIES <-   add_sex_column (ENTITIES)

add_type.pg_column <- function(data) {
  result <- data %>%
    mutate(type.pg = case_when(
      str_detect(name, "srl|s.r.l|s.r.l.|srls")  ~ "srl",
      str_detect(name, "d.i|d.i.")  ~ "di",
      str_detect(name, " ss |s.s|s.s.|societa' semplice")  ~ "ss",
      str_detect(name, " sas |s.a.s|s.a.s.")  ~ "sas",
      str_detect(name, "snc|s.n.c|s.n.c.|sncs")  ~ "snc",
      str_detect(name, " sc |s.c|s.c.|scs")  ~ "sc",
      TRUE ~ NA_character_
    ))
} 