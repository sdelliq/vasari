total.loans <- n_distinct(Loans$id.loan)
total.gbv <- sum(Loans$gbv.original)
total.ndg <- n_distinct(Loans$id.bor)
#---------------------------------#
#----     Loans by type     ------
#---------------------------------#

loans.by.type <- Loans %>% group_by(type) %>% 
  summarise(ndg = n_distinct(id.bor),
            n.loans = n(), 
            `%.loans` = n.loans/total.loans,
            sum.gbv = sum(gbv.original),
            `%.gbv` = sum.gbv/total.gbv)


#---------------------------------#
#----  Loans by range.gbv  ------
#---------------------------------#


range.gbv <- c(0,25000,50000,100000,250000,500000,1000000,3000000,5000000,Inf)
range.gbv.labels <- c('0-25k','25k-50k','50k-100k','100k-250k','250k-500k','500k-1.000k','1.000k-3.000k','3.000k-5.000k','5.000k+')


loans.range <- Loans %>% group_by(id.bor) %>%
                         mutate(gbv.original = sum(gbv.original)) %>% 
                         ungroup() 
                         

loans.range$range.gbv <- cut(loans.range$gbv.original, breaks = range.gbv, labels = range.gbv.labels, include.lowest = TRUE)

loans.by.gbv.range <- loans.range %>% 
                      select(id.bor,gbv.original,range.gbv) %>% distinct() %>%
                      group_by(range.gbv) %>% 
                      summarize(ndg = n_distinct(id.bor),
                                sum_gbv = sum(gbv.original),
                                perc = sum(gbv.original)/total.gbv )
names(loans.by.gbv.range)<- c("range.gbv","ndg","sum.gbv","%.gbv")


#----------------------------------------#
#----  entities by type.subject  ------
#----------------------------------------#

ent.by.type <- updated.entities %>% select(id.entity,type.subject) %>% 
             left_join(link.counterparties.entities, by= 'id.entity') %>% 
             left_join(counterparties,by = 'id.counterparty') %>%
             filter(role=='borrower' ) %>%
             left_join(Loans,by = 'id.bor') %>% 
             select(type.subject,id.bor,gbv.original) %>% distinct()


ent.by.type <- ent.by.type %>% group_by(type.subject) %>% 
  summarise(ndg = n_distinct(id.bor),
            sum.gbv = sum(gbv.original),
            perc.ndg = sum(ndg)/total.ndg,
            perc.gbv = sum(gbv.original)/total.gbv
            )

names(ent.by.type)<- c("type.subject","ndg","sum.gbv","%.ndg","%.gbv")


