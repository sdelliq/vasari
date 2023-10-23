source("Library.R")
source("Functions.R")

#Getting the path of the file with the portfolio
original.borrowers.corporate <- read.xlsx("data/DD_EARLY REPORT 62 POS. 13072023.xlsx")
original.borrowers.individual <- read_excel("data/DD_PDL CON RESIDENZA + IVA - 199 POS. 12072023.xlsx")

data_frames <- read_specific_sheets_and_save_df("data/Vasari_LDT.xlsx", c("Posizioni", "Anagrafiche", "Garanzie"))
original.posizioni <- data_frames$Posizioni
original.anagrafiche <- data_frames$Anagrafiche
original.garanzie <- data_frames$Garanzie

source("Cleaning.R")
source("tables/Counterparties.R")
source("tables/Entities.R")