agreement.proj <- original.pdr %>% select(id.agreement = id.ccpdr,date.due = dt.pagamento,amount.due = importo.singole.rate,
                                          date.paid = dt.fatturazione, amount.paid = pagamento) 




agreement.summary <- original.pdr %>% select(id.agreement = id.ccpdr, id.bor=ndg, date.agreement=data.pdr,
                                             gbv.agreement=importo.dovuto, importo.singole.rate,
                                             date.start=data.inizio.pagamento, n.installment=qta.rate,
                                             status=stato,length = qta.rate, date.last.payment=dt.fatturazione, pagamento,dt.pagamento) %>%
                                mutate(id.group = NA,
                                       date.end = NA)

agreement.summary <- agreement.summary %>%
            mutate(date.start = as.Date(ifelse(is.na(date.start),dt.pagamento,date.start))) %>% 
            select(-dt.pagamento)

agreement.summary$year.agreement <- format(agreement.summary$date.agreement, '%Y')

agree <- agreement.summary %>% select(id.agreement,pagamento,gbv.agreement,date.last.payment,importo.singole.rate) %>% 
              group_by(id.agreement) %>%
              summarise(paid = sum(pagamento),
                        amount.agreement = sum(importo.singole.rate),
                        residual = amount.agreement - sum(pagamento), 
                        date.last.payment = max(date.last.payment,na.rm = TRUE)) %>% 
          distinct()
agree$date.last.payment <- gsub('-Inf',NA,agree$date.last.payment) %>% as.Date()
agreement.summary <- left_join(agree, agreement.summary %>% select(-date.last.payment,-pagamento,-importo.singole.rate), by = "id.agreement")  %>% distinct()


agreement.summary <- agreement.summary %>% left_join(counterparties %>% select(id.counterparty,id.bor), by ='id.bor',relationship = "many-to-many")

repeated_agreements <- agreement.summary %>% group_by(id.agreement) %>% filter(n()>1)%>%
    left_join(original.pdr %>% select(id.bor=ndg, codice.fiscale), by="id.bor",relationship = "many-to-many") %>% distinct()  %>%
    left_join(updated.entities %>% select(codice.fiscale=cf.piva, name), by="codice.fiscale",relationship = "many-to-many") %>% distinct() %>%
    left_join(counterparties %>% select(name, c.id.counterparty=id.counterparty), by="name",relationship = "many-to-many") %>% distinct() %>%
    filter(id.counterparty == c.id.counterparty) %>% select(-c(codice.fiscale, name, c.id.counterparty))

agreement.summary <- agreement.summary %>% group_by(id.agreement) %>% filter(n()==1)
agreement.summary <- rbind(agreement.summary,repeated_agreements)

date.cutoff <- as.Date('2023-03-30')
qt.months <- 6
agreement.summary <- agreement.summary %>% mutate(status = case_when( 
  date.start > date.cutoff ~ 'proposal',
  (paid == 0) &  as.numeric(difftime(date.cutoff, date.start, units = "days")) / 30.44 > qt.months ~ 'failed',
  (paid == 0) &  as.numeric(difftime(date.cutoff, date.start, units = "days")) / 30.44 <  qt.months ~ 'active',
  (paid != amount.agreement) &  as.numeric(difftime(date.cutoff, date.last.payment, units = "days")) / 30.44 > qt.months ~ 'failed',
  (paid != amount.agreement) &  as.numeric(difftime(date.cutoff, date.last.payment, units = "days")) / 30.44 < qt.months ~ 'active',
  paid == amount.agreement ~ 'closed'
))



