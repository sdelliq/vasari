agreement.proj <- original.pdr %>% select(id.agreement = id.ccpdr,date.due = dt.pagamento,amount.due = importo.singole.rate,
                                          date.paid = dt.fatturazione, amount.paid = pagamento) 




agreement.summary <- original.pdr %>% select(id.agreement = id.ccpdr, id.bor=ndg, date.agreement=data.pdr,
                                             gbv.agreement=importo.dovuto, importo.singole.rate,
                                             date.start=data.inizio.pagamento, n.installment=qta.rate,
                                             status=stato, date.last.payment=dt.fatturazione, pagamento) %>%
                                mutate(id.group = NA,
                                       date.end = NA,
                                       length = NA)

agreement.summary$year.agreement <- format(agreement.summary$date.agreement, '%Y')

agree <- agreement.summary %>% select(id.agreement,pagamento,gbv.agreement,date.last.payment,importo.singole.rate) %>% 
              group_by(id.agreement) %>%
              summarise(paid = sum(pagamento),
                        amount.agreement = sum(importo.singole.rate),
                        residual = amount.agreement - sum(pagamento), 
                        date.last.payment = max(date.last.payment)) %>% 
          distinct()

agreement.summary <- left_join(agree, agreement.summary %>% select(-date.last.payment,-pagamento,-importo.singole.rate), by = "id.agreement")  %>% distinct()
#paid to be calculated from pagamento
#residual to be calculated from pagamento
#gbv e amount sono lo stesso?