#Counterparties

#useful columns
counterparties <- anagrafiche %>% select(id.bor, name, tipo.anagrafica.d.debitore.g.garante.dg.debitore.e.garante)
 
#creation of other columns
counterparties <- counterparties %>% mutate (id.group = NA,
                                             role= case_when(tipo.anagrafica.d.debitore.g.garante.dg.debitore.e.garante == "d" ~ "borrower",
                                                             tipo.anagrafica.d.debitore.g.garante.dg.debitore.e.garante == "g" ~  "guarantor",
                                                             tipo.anagrafica.d.debitore.g.garante.dg.debitore.e.garante == "dg" ~  "both"),
                                             flag.imputed=NA)

both <- counterparties %>% filter(role == "both") %>% select(id.bor) 
g <- original.garanzie %>% filter(ndg.garante %in% both$id.bor & ndg.garante != ndg) %>% select(ndg.garante) %>% distinct() 


real.g <- left_join(g %>% select (id.bor = ndg.garante), counterparties, by= "id.bor") %>% 
  mutate(role = 'guarantor') 
counterparties <- counterparties %>% mutate(role= case_when(role == "both"  ~ "borrower",
                                                            role == "borrower"  ~ "borrower",
                                                            role == "guarantor" ~  "guarantor"))
counterparties <- counterparties %>% rbind(real.g)
#dup.c <- counterparties[duplicated(counterparties$id.bor) | duplicated(counterparties$id.bor, fromLast = TRUE), ]

#id
counterparties$id.counterparty <- paste0("c", seq_len(nrow(counterparties)))

counterparties <- counterparties %>% select(id.counterparty, id.bor, id.group, role, name, flag.imputed)



