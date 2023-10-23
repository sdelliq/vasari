#all on tolower
original.borrowers.corporate <- original.borrowers.corporate %>% mutate_all(tolower)
original.borrowers.individual <- original.borrowers.individual %>% mutate_all(tolower)
original.posizioni <- original.posizioni %>% mutate_all(tolower)
original.anagrafiche <- original.anagrafiche %>% mutate_all(tolower)

#Cleaning the column names
colnames(original.posizioni) <- clean_column_names(colnames(original.posizioni))
colnames(original.anagrafiche) <- clean_column_names(colnames(original.anagrafiche))

#Trim
original.borrowers.corporate <- original.borrowers.corporate %>% mutate_all(str_trim)
original.borrowers.individual <- original.borrowers.individual %>% mutate_all(str_trim)
original.posizioni <- original.posizioni %>% mutate_all(str_trim)
original.anagrafiche <- original.anagrafiche %>% mutate_all(str_trim)

#
anagrafiche <- original.anagrafiche
anagrafiche <- anagrafiche %>% mutate(intestaz.unica.rag.soc = gsub("\\s+", " ", intestaz.unica.rag.soc))
anagrafiche <- anagrafiche %>%
  mutate(
    cf.piva = ifelse(
      !is.na(partita.iva.aziende) & is.na(codice.fiscale.persona.fisica), partita.iva.aziende,
      ifelse(!is.na(codice.fiscale.persona.fisica) & is.na(partita.iva.aziende), codice.fiscale.persona.fisica,
             ifelse(forma.giuridica %in% c("ind", "prv"), codice.fiscale.persona.fisica, partita.iva.aziende)
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

