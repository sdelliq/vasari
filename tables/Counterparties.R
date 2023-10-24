#Counterparties

#useful columns
counterparties <- anagrafiche %>% select(id.bor, name, tipo.anagrafica.d.debitore.g.garante.dg.debitore.e.garante)
 
#creation of other columns
count_entities <- function(x) as.integer(str_count(x, ',') + 1)
counterparties <- counterparties %>% mutate (id.group = NA,
                                             role= case_when(tipo.anagrafica.d.debitore.g.garante.dg.debitore.e.garante == "d" ~ "borrower",
                                                             tipo.anagrafica.d.debitore.g.garante.dg.debitore.e.garante == "g" ~  "guarantor",
                                                             tipo.anagrafica.d.debitore.g.garante.dg.debitore.e.garante == "dg" ~  "both"),
                                             flag.imputed=NA,
                                             n.entities = count_entities(name))

g <- counterparties %>% filter(role == "both") %>% mutate(role= "guarantor")
counterparties <- counterparties %>% mutate(role= case_when(role == "both"  ~ "borrower",
                                                            role == "borrower"  ~ "borrower",
                                                            role == "guarantor" ~  "guarantor"))
counterparties <- counterparties %>% rbind(g)

#id
counterparties$id.counterparty <- paste0("c", seq_len(nrow(counterparties)))

counterparties <- counterparties %>% select(id.counterparty, id.bor, id.group, role, name, n.entities, flag.imputed)
