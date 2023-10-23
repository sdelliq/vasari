source("Library.R")
source("Functions.R")

#Getting the path of the file with the portfolio
file1 <- read.xlsx("data/DD_EARLY REPORT 62 POS. 13072023.xlsx")
file2 <- read.xlsx("data/DD_PDL CON RESIDENZA + IVA - 199 POS. 12072023.xlsx")
file3 <- read_doc_and_save_df("data/Vasari_LDT.xlsx")
