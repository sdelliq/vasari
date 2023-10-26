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
                                       date.status = data.di.passaggio.a.sofferenza,
                                       id.bor = ndg)  %>%
                                mutate(gbv.residual = gbv.original,
                                       penalties = NA_real_,
                                       id.group = NA,
                                       originator = NA,
                                       ptf = NA,
                                       cluster.ptf =NA,
                                       status =ifelse(is.na(date.status),'utp','bad'),
                                       date.origination = NA,
                                       flag.imputed =NA,
                                       date.last.act = NA)

Loans <- Loans %>% select(id.loan,id.bor,id.group,originator,ptf,cluster.ptf,type,status,gbv.original,
                          gbv.residual,principal,interest,penalties,expenses,date.origination,date.status,
                          date.last.act,flag.imputed)
Loans$date.status <- as.Date(Loans$date.status)
Loans <- Loans %>% mutate_at(vars(gbv.original,gbv.residual,principal,interest,penalties,expenses), ~as.numeric(.))
Loans <- Loans %>% mutate_at(vars(gbv.original,gbv.residual,principal,interest,penalties,expenses), ~replace_na(.,0))
Loans$type <- factor(Loans$type)
Loans$status <- factor(Loans$status,levels = c('utp','bad'))
