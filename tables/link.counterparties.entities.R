link.counterparties.entities <- counterparties %>% select (id.counterparty, id.bor)  
link.counterparties.entities <- link.counterparties.entities %>% 
  left_join(entities %>% select(id.bor, id.entity) %>% divide_column_by_character(., id.bor, ","), by= "id.bor", relationship = "many-to-many")  %>% select(-id.bor)

entities <- entities %>% select(-id.bor)

counterparties <- counterparties %>%  left_join(    link.counterparties.entities %>%      
                                                      group_by(id.counterparty) %>%      
                                                      summarise(n.entities = n()),    by = "id.counterparty"  )

# entities.unica <- entities %>% select(id.bor, id.entity, name, cf.piva, type.subject)
# entities.unica <- entities.unica %>% divide_column_by_character(., id.bor, ",")
# 
# entities.unica <- entities.unica %>% group_by(id.bor, name) %>%  
#   mutate(    
#     cf.piva = ifelse(      
#       name %in% dup.bors$name, ifelse("individual" %in% type.subject, cf.piva[str_length(cf.piva)>15], cf.piva[str_length(cf.piva)<=15]), 
#       cf.piva)) %>% slice(1) %>% ungroup()
# 
# entities.unica <- entities.unica %>% select(id.bor, id.entity) %>% distinct()