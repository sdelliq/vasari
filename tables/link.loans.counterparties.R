link.loans.counterparties <- Loans %>% select (id.bor, id.loan) 
link.loans.counterparties <- link.loans.counterparties %>% 
  left_join(counterparties %>% select (id.counterparty, id.bor) , by= "id.bor", relationship = "many-to-many")  %>% select(-id.bor)