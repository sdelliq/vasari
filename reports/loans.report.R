#---------------------------------#
#----    totals by loans   ------
#---------------------------------#
total.loans <- n_distinct(Loans$id.loan)
total.gbv <- sum(Loans$gbv.original)
total.ndg <- n_distinct(Loans$id.bor)
average.loan <- total.gbv/total.loans
average.borr <- total.gbv/total.ndg
totals <- data.frame(N.Loans = total.loans,
                     Sum.GBV = total.gbv,
                     N.Borrowers = total.ndg,
                     Average.Loan.Size = average.loan,
                     Average.GBV.Borrower = average.borr)

#---------------------------------#
#----     Loans by type     ------
#---------------------------------#

loans.by.type <- Loans %>% group_by(type) %>% 
  summarise(ndg = n_distinct(id.bor),
            n.loans = n(), 
            `%.loans` = n.loans/total.loans,
            sum.gbv = sum(gbv.original),
            `%.gbv` = sum.gbv/total.gbv)
total.row <- loans.by.type %>% 
    summarise( type = 'Totals', 
               across(c(ndg,n.loans,`%.loans`,sum.gbv,`%.gbv`),sum))
loans.by.type <- rbind(total.row,loans.by.type)
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

total.row <- loans.by.gbv.range %>% 
  summarise( range.gbv = 'Totals', 
             across(c(ndg,sum.gbv,`%.gbv`),sum))
loans.by.gbv.range <- rbind(total.row,loans.by.gbv.range)

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

total.row <- ent.by.type %>% 
  summarise( type.subject = 'Totals', 
             across(c(ndg,sum.gbv,`%.ndg`,`%.gbv`),sum))
ent.by.type <- rbind(total.row,ent.by.type)

#----------------------------------------#
#----  entities by area  ------
#----------------------------------------#

ent.by.area <- updated.entities %>% select(id.entity,area) %>% 
  left_join(link.counterparties.entities, by= 'id.entity') %>% 
  left_join(counterparties,by = 'id.counterparty') %>%
  filter(role=='borrower' ) %>%
  left_join(Loans,by = 'id.bor') %>% 
  select(area,id.bor,gbv.original) %>% distinct()


ent.by.area <- ent.by.area %>% group_by(area) %>% 
  summarise(ndg = n_distinct(id.bor),
            sum.gbv = sum(gbv.original),
            perc.ndg = sum(ndg)/total.ndg,
            perc.gbv = sum(gbv.original)/total.gbv
  )

names(ent.by.area)<- c("area","ndg","sum.gbv","%.ndg","%.gbv")


total.row <- ent.by.area %>% 
  summarise( area = 'Totals', 
             across(c(ndg,sum.gbv,`%.ndg`,`%.gbv`),sum))
ent.by.area <- rbind(total.row,ent.by.area)



#----------------------------------------#
#----  entities by province - Top 5  -----
#----------------------------------------#
ent.by.province <- updated.entities %>% select(id.entity,province) %>% 
  left_join(link.counterparties.entities, by= 'id.entity') %>% 
  left_join(counterparties,by = 'id.counterparty') %>%
  filter(role=='borrower' ) %>%
  left_join(Loans,by = 'id.bor',relationship = "many-to-many") %>% 
  select(province,id.bor,gbv.original) %>% distinct()

ent.by.province <- ent.by.province %>% filter(!is.na(province)) %>%
  group_by(province) %>% 
  summarise(ndg = n_distinct(id.bor),
            sum.gbv = sum(gbv.original),
            perc.ndg = sum(ndg)/total.ndg,
            perc.gbv = sum(gbv.original)/total.gbv
            ) %>% 
  arrange(desc(sum.gbv))

Top_5_province_by_gbv <- ent.by.province[1:5, ]
names(Top_5_province_by_gbv) <- c("province", "ndg","sum.gbv","%.ndg","%.gbv")

