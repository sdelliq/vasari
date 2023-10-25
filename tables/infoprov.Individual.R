infoprov.PF <- original.borrowers.individual  %>% select(cf.piva = cf_piva,
                                                         cognome, 
                                                         nome,
                                                         solvency.base = stato.occupazione,
                                                         income.gross = emolumenti.mensili.lordi,
                                                         city = comune ,
                                                         note = note.17) 
infoprov.PF <- infoprov.PF %>% mutate(date.infoprov = NA,
                                      solvency.adj = NA,
                                      income.net = NA)
infoprov.PF$name <- paste(infoprov.PF$cognome,infoprov.PF$nome,sep = ' ')

infoprov.PF <- left_join(infoprov.PF, GEO.metadata %>% select(city, province, region), by = "city", relationship = "many-to-many")
infoprov.PF <- infoprov.PF %>% distinct()


infoprov.PF <- divide_column_by_character(infoprov.PF,c(solvency.base,income.gross),'/')
infoprov.PF$income.gross <- gsub(",00 €",'',infoprov.PF$income.gross)
infoprov.PF$income.gross <- as.numeric(infoprov.PF$income.gross)

infoprov.PF <- infoprov.PF %>% group_by(cf.piva) %>%
                               summarise(name = first(name),
                                         income.gross = ifelse("pensionato" %in% solvency.base, max(income.gross[solvency.base == "pensionato"]), max(income.gross)),
                                         solvency.base =  ifelse("pensionato" %in% solvency.base, "pensionato", first(solvency.base)) ,
                                         city = first(city),
                                         note = first(note),
                                         date.infoprov = first(date.infoprov),
                                         solvency.adj = first(solvency.adj),
                                         province = first(province),
                                         region = first(region)
                                         )


infoprov.PF$income.net <- ifelse(infoprov.PF$solvency.base=='pensionato', fct.pens(infoprov.PF$income.gross),ifelse(infoprov.PF$solvency.base=='dipendente',fct.emp(infoprov.PF$income.gross),NA))


infoprov.PF <- infoprov.PF %>%
  mutate(solvency.base = case_when(
    str_detect(note, 'indeterminato') ~ 'Employee-Permanent',
    str_detect(note, 'determinato') ~ 'Employee-Temporary',
    str_detect(solvency.base,'pensionato') ~ 'Retired',
    str_detect(solvency.base,'deceduto') ~ 'Deceased',
    str_detect(solvency.base,'dipendente') ~ 'Employee-N/A',
    str_detect(solvency.base,'disoccupato') ~ 'Insolvent'
  ))
infoprov.PF <- infoprov.PF %>% select(cf.piva,date.infoprov,name,solvency.base,solvency.adj,income.gross,
                                      income.net,city,province,region)
