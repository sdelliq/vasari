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
      str_detect(name, "srl\\b|s.r.l\\b|s.r.l.\\b|srls\\b")  ~ "srl",
      str_detect(name, " d.i\\b | d.i.\\b")  ~ "di",
      str_detect(name, " ss\\b |s.s\\b|s.s.\\b|societa' semplice\\b")  ~ "ss",
      str_detect(name, " sas \\b|s.a.s\\b|s.a.s.\\b")  ~ "sas",
      str_detect(name, "snc\\b|s.n.c\\b|s.n.c.\\b|sncs\\b")  ~ "snc",
      str_detect(name, " sc\\b |s.c\\b|s.c.\\b|scs\\b")  ~ "sc",
      str_detect(name, "ass.zione") ~ "other",
      TRUE ~ NA_character_
    ))
} 

add_type_subject_column <- function(data) {
  result <- data %>%
    mutate(
      type.subject = ifelse(
        is.na(cf.piva), 
        type.subject,  # Keep old value
        ifelse(
          grepl("^[0-9]+$", cf.piva),
          "corporate",
          "individual"
          
          
        )
      )
    )
  
  return(result)
}
# Running example:
# ENTITIES <- add_type_subject_column(ENTITIES)


add_age_column <- function(data) {
  result <- data %>%
    mutate(
      is_individual = nchar(cf.piva) == 16,
      age = case_when(
        is_individual ~ {
          year_of_birth <- as.numeric(stringr::str_sub(cf.piva, start = 7L, end = 8L))
          current_year <- as.numeric(format(Sys.Date(), "%Y"))
          ifelse(
            year_of_birth >= 0 & year_of_birth <= (current_year - 2018),
            current_year - (2000 + year_of_birth),
            current_year - (1900 + year_of_birth)
          )
        },
        TRUE ~ NA_real_
      )
    ) %>%
    select(-is_individual)
  
  return(result)
}

update_type.pg_column <- function(data) {
  result <- data %>%
    mutate(type.pg = case_when(
      str_detect(name, "ass.zione") ~ "other",
      str_detect(type.pg, "societa' a responsabilita' limitata")  ~ "srl",
      str_detect(type.pg, "societa' per azioni")  ~ "spa",
      str_detect(type.pg, "societa' in nome collettivo")  ~ "snc",
      str_detect(type.pg, "societa' in accomandita semplice")  ~ "sas",
      str_detect(type.pg, "cooperativa|consortile|fidi")  ~ "sc",
      str_detect(type.pg, "ass.zione|associazione|non precisata")  ~ "other",
      TRUE ~ NA_character_
    ))
} 

update_status.pg <- function(data){
  result <- data %>%
    mutate(status.pg = case_when(
      str_detect(status.pg, "inattiva")  ~ "inactive",
      str_detect(status.pg, "attiva")  ~ "active",
      str_detect(status.pg, "fallita")  ~ "bankruptcy", 
      str_detect(status.pg, "cancellata")  ~ "canceled",
      str_detect(status.pg, "in scioglimento / liquidazione")  ~ "liquidation",
      str_detect(status.pg, "in procedura concorsuale")  ~ "insolvency",
      TRUE ~ NA_character_
    ))
}

###-----------------------------------------------------------------------###
#-----                Info providing Functions                        -----         
###-----------------------------------------------------------------------###
fct.emp <- function(x) { 
  y <- x * 12 * (1 - 0.09)  
  x.1 <- 15*10^3 
  x.2 <- 28*10^3 
  x.3 <- 50*10^3 
  p.1 <- 0.23 
  p.2 <- 0.25 
  p.3 <- 0.35 
  p.4 <- 0.43 
  t.1 <- pmin(y, x.1) %>% pmax(0) * p.1 
  t.2 <- pmin(y - x.1, x.2 - x.1) %>% pmax(0) * p.2 
  t.3 <- pmin(y - x.2, x.3 - x.2) %>% pmax(0) * p.3 
  t.4 <- pmin(y - x.3) %>% pmax(0) * p.4 
  z <- (y - (t.1+t.2+t.3+t.4))/12 
  return(z) 
} 




fct.pens <- function(x) { 
  y <- x * 12  
  x.1 <- 15*10^3 
  x.2 <- 28*10^3 
  x.3 <- 50*10^3 
  p.1 <- 0.23 
  p.2 <- 0.25 
  p.3 <- 0.35 
  p.4 <- 0.43 
  t.1 <- pmin(y, x.1) %>% pmax(0) * p.1 
  t.2 <- pmin(y - x.1, x.2 - x.1) %>% pmax(0) * p.2 
  t.3 <- pmin(y - x.2, x.3 - x.2) %>% pmax(0) * p.3 
  t.4 <- pmin(y - x.3) %>% pmax(0) * p.4 
  z <- (y - (t.1+t.2+t.3+t.4))/12 
  return(z) 
} 



