source("Library.R")
source("Functions.R")

#Getting the path of the file with the portfolio
original.borrowers.corporate <- read.xlsx("data/DD_EARLY REPORT 62 POS. 13072023.xlsx")
original.borrowers.individual <- read_excel("data/DD_PDL CON RESIDENZA + IVA - 199 POS. 12072023.xlsx")

data_frames <- read_specific_sheets_and_save_df("data/Vasari_LDT.xlsx", c("Posizioni", "Anagrafiche", "Garanzie",'PDR'))
original.posizioni <- data_frames$Posizioni
original.anagrafiche <- data_frames$Anagrafiche
original.garanzie <- data_frames$Garanzie
original.pdr <- data_frames$PDR

source("Cleaning.R")
source("tables/Loans.R")
source("tables/Counterparties.R")
source("tables/Entities.R")
source("tables/Guarantors.R")
source("tables/link.counterparties.entities.R")
source("tables/link.loans.counterparties.R")

source("tables/Infoprov_PG.r")
source("tables/infoprov.Individual.R")
source("tables/Entities_Infoprovided.R")
source("tables/Agreement.proj.R")

source("reports/tables.report.R")
source("reports/graphs.R")
source("reports/Report.R")
