#all on tolower
original.borrowers.corporate <- original.borrowers.corporate %>% mutate_all(tolower)
original.borrowers.individual <- original.borrowers.individual %>% mutate_all(tolower)
original.posizioni <- original.posizioni %>% mutate_all(tolower)
original.anagrafiche <- original.anagrafiche %>% mutate_all(tolower)

colnames(original.posizioni) <- clean_column_names(colnames(original.posizioni))
colnames(original.anagrafiche) <- clean_column_names(colnames(original.anagrafiche))
