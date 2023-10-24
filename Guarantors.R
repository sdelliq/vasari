


original.garanzie <- original.garanzie %>% select(-`#Immobili`)
original.garanzie <- original.garanzie %>% mutate_all(tolower)
colnames(original.garanzie) <- clean_column_names(colnames(original.garanzie))
original.garanzie$intestazione.garante <- gsub("\\s+", " ", original.garanzie$intestazione.garante)
original.garanzie$intestazione.garante <- gsub("[,.*]+$", "", original.garanzie$intestazione.garante)





Guarantors <- original.garanzie %>% select(ndg.garante,intestazione.garante) %>% distinct()
Guarantors_Groups <- Guarantors %>% rename(id.bor = ndg.garante, name = intestazione.garante)
Guarantors_Groups <- Guarantors_Groups %>% mutate(id.counterparty = NA, id.group = NA,role = 'guarantor',flag.imputed = NA, n.entities = 0)
counterparties <-  rbind(counterparties,Guarantors_Groups)
counterparties <- counterparties  %>% group_by(id.bor) %>% slice(1) %>% ungroup()


counterparties <- counterparties %>% 
              mutate(id.counterparty =  case_when(is.na(id.counterparty) ~paste0('c', nrow(counterparties)),  !is.na(id.counterparty) ~id.counterparty),
                     n.entities =  case_when(n.entities==0 ~1, n.entities!=0 ~n.entities))



Link_Borrower_Guarantors <- original.garanzie %>% select(id.bor = ndg, id.guarantor = ndg.garante) %>% distinct()





ent_da_aggiungere <- data.frame(id.entity  = 'e245',name = "	
italia com-fidi societa' consortile a responsabilita'", type.subject = 'corporate',type.pg = 'sc')
entities <- entities %>% add_row(ent_da_aggiungere)
