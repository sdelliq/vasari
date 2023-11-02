#all on tolower
original.borrowers.corporate <- original.borrowers.corporate %>% mutate_all(tolower)
original.borrowers.individual <- original.borrowers.individual %>% mutate_all(tolower)
original.posizioni <- original.posizioni %>% mutate_all(tolower)
original.anagrafiche <- original.anagrafiche %>% mutate_all(tolower)
original.garanzie <- original.garanzie %>% mutate_all(tolower)
original.pdr <- original.pdr %>% mutate_all(tolower)

#Cleaning the column names
colnames(original.borrowers.corporate) <- clean_column_names(colnames(original.borrowers.corporate))
colnames(original.borrowers.individual) <- clean_column_names(colnames(original.borrowers.individual))
colnames(original.posizioni) <- clean_column_names(colnames(original.posizioni))
colnames(original.anagrafiche) <- clean_column_names(colnames(original.anagrafiche))
colnames(original.garanzie) <- clean_column_names(colnames(original.garanzie))
colnames(original.pdr) <-  clean_column_names(colnames(original.pdr))

#Trim
original.borrowers.corporate <- original.borrowers.corporate %>% mutate_all(str_trim)
original.borrowers.individual <- original.borrowers.individual %>% mutate_all(str_trim)
original.posizioni <- original.posizioni %>% mutate_all(str_trim)
original.anagrafiche <- original.anagrafiche %>% mutate_all(str_trim)
original.pdr <- original.pdr %>%  mutate_all(str_trim)

#
anagrafiche <- original.anagrafiche
anagrafiche <- anagrafiche %>% mutate(intestaz.unica.rag.soc = gsub("\\s+", " ", intestaz.unica.rag.soc))
anagrafiche <- anagrafiche %>%
  mutate(
    cf.piva = ifelse(
      !is.na(partita.iva.aziende) & is.na(codice.fiscale.persona.fisica), partita.iva.aziende,
      ifelse(!is.na(codice.fiscale.persona.fisica) & is.na(partita.iva.aziende), codice.fiscale.persona.fisica,
             ifelse(is.na(codice.fiscale.persona.fisica) & is.na(partita.iva.aziende), NA, 
                    ifelse(codice.fiscale.persona.fisica != partita.iva.aziende, paste0(codice.fiscale.persona.fisica, ",", partita.iva.aziende), partita.iva.aziende))
      )
    )
  )
anagrafiche <- anagrafiche %>% rename(name = intestaz.unica.rag.soc, id.bor = ndg)

anagrafiche <- anagrafiche %>%
  mutate(across(c(starts_with("nome_"), "name"), ~ gsub("[,.*]+$", "", .))) #Deletes the *.* from the names
anagrafiche <- anagrafiche %>% mutate_all(str_trim)
anagrafiche$nome_1 <- gsub("\\s+", " ", anagrafiche$nome_1)
anagrafiche <- anagrafiche %>%
  mutate(
    name = ifelse(
      is.na(nome_1),
      name,
      paste0(
        ifelse(!is.na(nome_1), nome_1, ""),
        ifelse(!is.na(nome_2), paste0(", ", nome_2), ""),
        ifelse(!is.na(nome_3), paste0(", ", nome_3), ""),
        ifelse(!is.na(nome_4), paste0(", ", nome_4), ""),
        ifelse(!is.na(nome_5), paste0(", ", nome_5), "")
      )
    )
  )
anagrafiche <- anagrafiche %>% mutate(data.di.nascita = as.Date.character(data.di.nascita))


# Cleaning of garanzie
original.garanzie <- original.garanzie %>% select(-`x.immobili`)
original.garanzie$intestazione.garante <- gsub("\\s+", " ", original.garanzie$intestazione.garante)
original.garanzie$intestazione.garante <- gsub("[,.*]+$", "", original.garanzie$intestazione.garante)

# Cleaning original.pdr:
original.pdr <- original.pdr %>%
  mutate(across(c(starts_with("data"),starts_with("dt")), ~ as.Date(.)))

columns_to_convert <- c("importo.dovuto", "qta.rate", "importo.singole.rate", "pagamento", "importo.saldo", "sum.attivo", "qta.pagamenti.presenti")
original.pdr <- original.pdr %>%
  mutate_at(columns_to_convert, as.numeric)
