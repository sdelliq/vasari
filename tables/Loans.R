#---------------------------------#
#----  create Loans table   ------
#---------------------------------#
Loans <- original.posizioni %>% select(numero.rapporto,ndg, intestazione,forma.tecnica.rapporto,gross.book.value.totale,
                                          gross.book.value.capitale,gross.book.value.spese,gross.book.value.interessi,data.di.passaggio.a.sofferenza) %>%
                                rename(gbv.original=gross.book.value.totale,
                                       principal =gross.book.value.capitale,
                                       expenses =gross.book.value.spese,
                                       interest = gross.book.value.interessi,
                                       type = forma.tecnica.rapporto,
                                       id.loan = numero.rapporto,
                                       name = intestazione,
                                       date.status = data.di.passaggio.a.sofferenza)  %>%
                                mutate(gbv.residual = gbv.original,
                                       penalties = NA_real_)

Loans$date.status <- as.Date(Loans$date.status)
Loans <- Loans %>% mutate_at(vars(gbv.original,gbv.residual,principal,interest,penalties,expenses), ~as.numeric(.))
Loans <- Loans %>% mutate_at(vars(gbv.original,gbv.residual,principal,interest,penalties,expenses), ~replace_na(.,0))
Loans$type <- factor(Loans$type)


Loans$name <- gsub("\\s+", " ", Loans$name)
Loans$name <- gsub(" e ", ", ", Loans$name)

#---------------------------------#
#----     check gbv sum    ------
#---------------------------------#


Loans <- Loans %>% mutate(diff = gbv.original-principal-interest-penalties-expenses)%>%
                   mutate(check_gbv = ifelse(abs(diff)<0.5,TRUE,FALSE)) %>% select(-diff) 

#sum(Loans$check_gbv)==nrow(Loans)   #TRUE 

Loans <- Loans %>% select(-check_gbv)




#---------------------------------#
#----     Loans by type     ------
#---------------------------------#
total.loans <- n_distinct(Loans$id.loan)
total.gbv <- sum(Loans$gbv.original)
loans_by_type <- Loans %>% group_by(type) %>% 
                           summarise(n.loans = n(), 
                                     `%.loans` = n.loans/total.loans,
                                     sum.gbv = sum(gbv.original),
                                     `%.gbv` = sum.gbv/total.gbv)


