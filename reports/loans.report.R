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
                     ndg = total.ndg,
                     Average.Loan.Size = average.loan,
                     Average.GBV.Borrower = average.borr)

#---------------------------------#
#----     Loans by type     ------
#---------------------------------#

loans.by.type <- Loans %>% group_by(type) %>% 
  summarise(
            n.loans = n(), 
            `%.loans` = n.loans/total.loans,
            sum.gbv = sum(gbv.original),
            `%.gbv` = sum.gbv/total.gbv)
total.row <- loans.by.type %>% 
    summarise( type = 'Totals', 
               across(c(n.loans,`%.loans`,sum.gbv,`%.gbv`),sum))
loans.by.type <- rbind(total.row,loans.by.type)
#---------------------------------#
#----  Loans by range.gbv  ------
#---------------------------------#

loans.range <- Loans %>% group_by(id.bor) %>%
                         mutate(gbv.original = sum(gbv.original)) %>% 
                         ungroup() 

#quantile(loans.range$gbv.original, probs= c(0.25, 0.50, 0.75))
range.gbv <- c(0,50000,100000,200000,Inf)
range.gbv.labels <- c('0-50k','50k-100k','100k-200k','100k+')


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
#----       entities -loans      ------
#----------------------------------------#
entities.loans <- updated.entities  %>% 
  left_join(link.counterparties.entities, by= 'id.entity',relationship = "many-to-many") %>% 
  left_join(counterparties,by = 'id.counterparty') %>%
  filter(role=='borrower' ) %>%
  left_join(Loans,by = 'id.bor',relationship = "many-to-many")

#----------------------------------------#
#----  counterparties by type.subject  ------
#----------------------------------------#

ent.by.type <- entities.loans %>% 
             select(type.subject,id.bor,gbv.original) %>% distinct() %>%
  group_by(id.bor) %>% 
  summarise(gbv.original= ifelse('individual' %in% type.subject & 'corporate' %in% type.subject,sum(gbv.original[type.subject=='individual']),sum(gbv.original)),
            type.subject = ifelse('individual' %in% type.subject,'individual','corporate'))
                     


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
#----  counterparties by area  ------
#----------------------------------------#

ent.by.area <- entities.loans %>% 
  select(area,id.bor,gbv.original) %>% distinct() %>%
  group_by(id.bor,gbv.original) %>% summarise(area= first(area)) %>% distinct() %>%
  group_by(id.bor) %>% summarise(area= first(area),gbv.original = sum(gbv.original)) %>% distinct()


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
#----  counterparties by province - Top 5  -----
#----------------------------------------#
ent.by.province <- entities.loans %>% 
  select(province,id.bor,gbv.original) %>%  distinct() %>%
  group_by(id.bor,gbv.original) %>% summarise(province= first(province)) %>% distinct() %>%
  group_by(id.bor) %>% summarise(province= first(province),gbv.original = sum(gbv.original)) %>% distinct()


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


#----------------------------------------#
#----  counterparties by solvency  (brutto)-----
#----------------------------------------#



ent.by.solvency <- entities.loans %>% 
  select(solvency.pf,id.bor,gbv.original) %>% distinct() %>%
  group_by(id.bor,gbv.original) %>% summarise(solvency.pf= first(solvency.pf)) %>% distinct() %>%
  group_by(id.bor) %>% summarise(solvency.pf= first(solvency.pf),gbv.original = sum(gbv.original)) %>% distinct()


ent.by.solvency <- ent.by.solvency %>%
  group_by(solvency.pf) %>% 
  summarise(ndg = n_distinct(id.bor),
            sum.gbv = sum(gbv.original),
            perc.ndg = sum(ndg)/total.ndg,
            perc.gbv = sum(gbv.original)/total.gbv
  ) %>% 
  arrange(desc(sum.gbv))


names(ent.by.solvency) <- c("solvency.pf", "ndg","sum.gbv","%.ndg","%.gbv")

total.row <- ent.by.solvency %>% 
  summarise( solvency.pf = 'Totals', 
             across(c(ndg,sum.gbv,`%.ndg`,`%.gbv`),sum))
ent.by.solvency <- rbind(total.row,ent.by.solvency)

ent.by.solvency <- ent.by.solvency[c(1,3:7,2),] 
ent.by.solvency[is.na(ent.by.solvency)] <- "N/A *"


#----------------------------------------#
#----  counterparties by solvency  -----
#----------------------------------------#
solvency_order <- c("Pensioner", "Employee-Permanent","Employee-N/A","Employee-Temporary", "Real Estate","Self-Employed", "Insolvent","Deceased")
solvency_order <- rev(solvency_order)
counter.by.solvency <- entities.loans %>% select(id.counterparty,gbv.original,solvency.pf)%>% distinct() %>% 
  group_by(id.counterparty,gbv.original) %>% slice(1) %>% ungroup() %>%
  group_by(id.counterparty) %>%
  summarise(solvency.pf =  min(solvency.pf, order = match(solvency.pf, solvency_order)),
            gbv.original = sum(gbv.original))

counter.by.solvency <- counter.by.solvency %>% mutate(solvency.pf = case_when(
    solvency.pf == '1' ~ "Deceased",
    solvency.pf == '2' ~ "Insolvent",
    solvency.pf == '3' ~"Self-Employed",
    solvency.pf == '4' ~"Real Estate",
    solvency.pf == '5' ~"Employee-Temporary",
    solvency.pf == '6' ~"Employee-N/A",
    solvency.pf == '7' ~"Employee-Permanent",
    solvency.pf == '8' ~"Pensioner",
    TRUE ~ NA
))

counter.by.solvency <- counter.by.solvency %>%
  group_by(solvency.pf) %>% summarise(n.group = n_distinct(id.counterparty), sum.gbv = sum(gbv.original))



#----------------------------------------#
#----       entities by type      ------
#----------------------------------------#

type.entities <- updated.entities %>% select(id.entity,type.subject) %>% 
  group_by(type.subject) %>% summarise(n = n())

#----------------------------------------#
#----       entities by area      ------
#----------------------------------------#

area.entities <- updated.entities %>% select(id.entity,area) %>% 
  group_by(area) %>% summarise(n = n())

#----------------------------------------#
#----       entities by province      ------
#----------------------------------------#

province.entities <- updated.entities %>% select(id.entity,province) %>% 
  group_by(province) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(5)

#----------------------------------------#
#----       entities by solvency.pf  ------
#----------------------------------------#

solvency.pf.entities <- updated.entities %>% select(id.entity,solvency.pf) %>% 
  group_by(solvency.pf) %>% summarise(n = n())

#----------------------------------------------------#
#----       entities by solvency.pf  and area  ------
#----------------------------------------------------#

pivot_table <- updated.entities %>% group_by(solvency.pf,area) %>% summarise(values = n())
pivot_table <- as.data.frame(pivot_table)

mat <- pivot_table %>%
  pivot_wider(names_from = solvency.pf, values_from = values)

mat <- as.data.frame(mat)
mat$area <- replace_na(mat$area,'N/A')
mat[is.na(mat)] <- 0
mat$Total <- rowSums(mat[2:7])
total <-  mat %>% 
  summarise( area = 'Totals', 
             across(-area,sum))
mat <- rbind(mat,total)


