link.counterparties.entities <- counterparties %>% select (id.counterparty, id.bor) 
link.counterparties.entities <- link.counterparties.entities %>% 
  left_join(entities %>% select(id.bor, id.entity) %>% divide_column_by_character(., id.bor, ","), by= "id.bor", relationship = "many-to-many")  %>% select(-id.bor)

entities <- entities %>% select(-id.bor)