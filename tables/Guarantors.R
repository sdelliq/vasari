
Guarantors <- original.garanzie %>% select(ndg.garante,intestazione.garante) %>% distinct()
Guarantors_Groups <- Guarantors %>% rename(id.bor = ndg.garante, name = intestazione.garante) %>% 
  filter(!(id.bor %in% counterparties$id.bor))
Guarantors_Groups <- Guarantors_Groups %>% mutate(id.counterparty = NA, id.group = NA,role = 'guarantor',flag.imputed = NA)
counterparties <-  rbind(counterparties,Guarantors_Groups) %>% distinct()
counterparties <- counterparties  %>% group_by(id.bor, role) %>% slice(1) %>% ungroup()

#We're adding the guarantors that weren't on our counterparties already
counterparties <- counterparties %>% 
              mutate(id.counterparty =  case_when(is.na(id.counterparty) ~paste0('c', nrow(counterparties)),  
                                                  !is.na(id.counterparty) ~id.counterparty))


#We're adding the new entity 
ent_da_aggiungere <- data.frame(id.entity  = paste0('e', nrow(entities)+1),
                                id.bor="703991",
                                name = "italia com-fidi societa' consortile a responsabilita'", 
                                type.subject = 'corporate',type.pg = 'sc')
entities <- entities %>% add_row(ent_da_aggiungere)
