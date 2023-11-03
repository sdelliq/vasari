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
range.gbv.labels <- c('0-50k','50k-100k','100k-200k','200k+')


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


#---------------------------------#
#----  Loans by vintage and gbv range  ------
#---------------------------------#
cutoff.date <- as.Date('2023-07-12')
loans.vintage <- Loans %>% 
               mutate(vintage = round(as.numeric(cutoff.date - date.status)/365,0))

quantile(loans.vintage$vintage, c(0.33,0.66))

Range_vintage <- c(0,3,5,Inf)
Range_vintage_labels <- c('0-3','4-5','5+')
loans.vintage$range.vintage <- cut(loans.vintage$vintage, breaks = Range_vintage, labels = Range_vintage_labels, include.lowest = TRUE)
loans.vintage$range.gbv <- cut(loans.vintage$gbv.original, breaks = range.gbv, labels = range.gbv.labels, include.lowest = TRUE)

loans.vintage <- loans.vintage %>%
  select(id.bor,gbv.original,range.gbv,range.vintage) %>% distinct() %>%
  group_by(range.vintage,range.gbv) %>% 
  summarize(n.loans = n(),sum_gbv = sum(gbv.original),perc = sum(gbv.original)/total.gbv )
names(loans.vintage)<- c("Range.Vintage","Range.GBV",'N.Loans',"Sum.GBV"," % GBV")

subtotal.row <- loans.vintage %>% group_by(Range.Vintage) %>%
  summarise( Range.Vintage = 'Subtotals',
             Range.GBV = ' ',
             across(c('N.Loans',"Sum.GBV"," % GBV"),sum))
total.row <- subtotal.row %>% summarise( Range.Vintage = 'Totals',
                                         Range.GBV = ' ',
                                         across(c('N.Loans',"Sum.GBV"," % GBV"),sum))
loans.vintage <- rbind(subtotal.row,total.row,loans.vintage)
loans.vintage <- loans.vintage[c(4,5:8,1,9:12,2,13:16,3),]

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

ent.by.province <- ent.by.province[6:21,] %>% summarise(
  province= "others", 
  across(c(ndg,sum.gbv,perc.ndg,perc.gbv),sum)
)
Top_5_province_by_gbv <- rbind(Top_5_province_by_gbv, ent.by.province)
names(Top_5_province_by_gbv) <- c("province", "ndg","sum.gbv","%.ndg","%.gbv")
total.row <- Top_5_province_by_gbv %>% 
  summarise( province = 'Totals', 
             across(c(ndg,sum.gbv,`%.ndg`,`%.gbv`),sum))
Top_5_province_by_gbv <- rbind(total.row,Top_5_province_by_gbv)
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

#----------------------------------------------------#
#----       How many borrowers have guarantors
# How many guarantors are individual or corporate  ------
#----------------------------------------------------#

borrowers.with.guarantors <- counterparties %>% group_by(id.bor) %>% summarise(
  has_guarantor = ifelse(n_distinct(id.counterparty)>1, "Yes", "No"),
  id.counterparty = paste(id.counterparty, collapse = ","),
  role = paste(role, collapse = ",")
)
borrowers.with.guarantors <- divide_column_by_character(borrowers.with.guarantors, c(id.counterparty, role), ",")
borrowers.with.guarantors <- borrowers.with.guarantors %>% left_join(link.counterparties.entities, by = "id.counterparty")
borrowers.with.guarantors <- borrowers.with.guarantors %>% left_join(updated.entities %>% select (id.entity, type.subject), by = "id.entity",relationship = "many-to-many")
borrowers.with.guarantors <- borrowers.with.guarantors %>% select(-id.entity) %>% distinct()

borrowers.with.guarantors <- borrowers.with.guarantors %>%
  group_by(id.bor, role) %>% 
  summarise(type.subject = ifelse('individual' %in% type.subject,'individual','corporate'),
            has_guarantor = first(has_guarantor))

borrowers.with.guarantors <- borrowers.with.guarantors %>% mutate(type.g = ifelse(has_guarantor=="Yes", type.subject, "no"))

borrowers.with.guarantors <- borrowers.with.guarantors %>% filter(role=="borrower") %>% group_by(type.subject) %>% summarise(
  g.individual = sum(has_guarantor == "Yes" & type.g=="individual"),
  g.corporate = sum(has_guarantor == "Yes" & type.g=="corporate"),
  no.guarantor = sum(has_guarantor == "No")
)


#----------------------------------------------------#
#----       Type of corporate  ------
#----------------------------------------------------#
corporate.type <- entities.loans %>% 
  select(type.pg,id.bor,gbv.original) %>% 
  group_by(id.bor,gbv.original) %>% summarise(type.pg= first(type.pg)) %>% 
  group_by(id.bor) %>% summarise(type.pg= first(type.pg),gbv.original = sum(gbv.original)) %>% distinct()


corporate.type <- corporate.type %>%
  group_by(type.pg) %>% 
  summarise(ndg = n_distinct(id.bor),
            sum.gbv = sum(gbv.original),
            perc.ndg = sum(ndg)/total.ndg,
            perc.gbv = sum(gbv.original)/total.gbv
  ) %>% 
  arrange(desc(sum.gbv))


names(corporate.type) <- c("corporate type", "ndg","sum.gbv","%.ndg","%.gbv")

total.row <- corporate.type %>% 
  summarise( `corporate type` = 'Totals', 
             across(c(ndg,sum.gbv,`%.ndg`,`%.gbv`),sum))
corporate.type <- rbind(total.row,corporate.type)

corporate.type <- corporate.type[c(1,3:8,2),] 
corporate.type[is.na(corporate.type)] <- "N/A *"

#----------------------------------------------------#
#----       Status of corporate  ------
#----------------------------------------------------#
corporate.status <- entities.loans %>% 
  select(status.pg,id.bor,gbv.original, type.subject) %>% distinct() #%>% 
  #group_by(id.bor,gbv.original) %>% summarise(status.pg= first(status.pg)) %>% 
  #group_by(id.bor) %>% summarise(status.pg= first(status.pg),gbv.original = sum(gbv.original)) %>% distinct()

corporate.status <- corporate.status %>% group_by(id.bor,gbv.original) %>% summarise(
  status.pg= first(status.pg),
  type.subject= first(type.subject)
  )

corporate.status <- corporate.status %>%
  group_by(id.bor) %>% 
  summarise(type.subject = ifelse('individual' %in% type.subject,'individual','corporate'),
            status.pg= first(status.pg),
            gbv.original = sum(gbv.original))

corporate.status <- corporate.status %>% mutate(status.pg = ifelse(type.subject=="corporate" & is.na(status.pg), "corporate N/A", status.pg))

corporate.status <- corporate.status %>%
  group_by(status.pg) %>% 
  summarise(ndg = n_distinct(id.bor),
            sum.gbv = sum(gbv.original),
            perc.ndg = sum(ndg)/total.ndg,
            perc.gbv = sum(gbv.original)/total.gbv
  ) %>% 
  arrange(desc(sum.gbv))


names(corporate.status) <- c("corporate status", "ndg","sum.gbv","%.ndg","%.gbv")

total.row <- corporate.status %>% 
  summarise( `corporate status` = 'Totals', 
             across(c(ndg,sum.gbv,`%.ndg`,`%.gbv`),sum))
corporate.status <- rbind(total.row,corporate.status)

corporate.status <- corporate.status[c(1,3:9,2),] 
corporate.status[is.na(corporate.status)] <- "N/A *"



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


#----------------------------------------------------#
#----     total   agreement  ------
#----------------------------------------------------#

total.agreement <- n_distinct(agreement.summary$id.agreement)
total.gbv.agreement <- sum(agreement.summary$gbv.agreement)
total.amount.agreed <- sum(agreement.summary$amount.agreement)

total.pdr <- data.frame(N.Agreement = total.agreement,
                        Amount.agreed = total.amount.agreed,
                        Due.GBV = total.gbv.agreement
                        )
#----------------------------------------------------#
#----       agreement by status  ------
#----------------------------------------------------#


agreemeent.by.status <- agreement.summary %>% 
  group_by(status) %>% 
  summarise(n.agreement = n_distinct(id.agreement),'%.agreement' = n.agreement / total.agreement)
total.row <- agreemeent.by.status %>% summarise(status = 'totals', n.agreement=sum(n.agreement), `%.agreement`= sum(`%.agreement`))
agreemeent.by.status <- rbind(total.row,agreemeent.by.status)
#----------------------------------------------------#
#----       agreement by discount ------
#----------------------------------------------------#

agreement.summary.discount <- agreement.summary %>% mutate(discount = round((gbv.agreement-amount.agreement)/gbv.agreement,2))
quantile(agreement.summary.discount$discount,c(0.33,0.66))

# 0.33 , 0.44

Range_discount <- c(0,0.32,0.4,Inf)
Range_discount_labels <- c('0-32%','32-40%','40-100%')
agreement.summary.discount$discount <- cut(agreement.summary.discount$discount, breaks = Range_discount, labels = Range_discount_labels, include.lowest = TRUE)

reference_df <- expand.grid(
  status = unique(agreement.summary.discount$status),
  discount = unique(agreement.summary.discount$discount)
)

agreement.summary.discount <- agreement.summary.discount %>% 
  group_by(status,discount) %>% 
  summarise(n.agreement = n_distinct(id.agreement),paid = sum(paid),amount.agreed = sum(amount.agreement),total.debt = sum(gbv.agreement))
  
agreement.summary.discount <- merge(reference_df, agreement.summary.discount, by = c("status", "discount"), all = TRUE)
agreement.summary.discount[is.na(agreement.summary.discount)]<-0
subtotal.row <- agreement.summary.discount %>% group_by(status) %>%
  summarise( status = 'Subtotals',
             discount = ' ',
             across(c('n.agreement',"paid","amount.agreed","total.debt"),sum))
total.row <- subtotal.row %>% summarise( status = 'Totals',
                                         discount = ' ',
                                         across(c('n.agreement',"paid","amount.agreed","total.debt"),sum))
agreement.summary.discount <- rbind(total.row,agreement.summary.discount,subtotal.row)
agreement.summary.discount <- agreement.summary.discount[c(1:4,11,8:10,13,5:7,12),]
#----------------------------------------------------#
#----       agreement by role ------
#----------------------------------------------------#

agree.by.role <- agreement.summary %>% 
  left_join(counterparties %>% select(id.counterparty,role), by = 'id.counterparty') %>%
  group_by(role) %>% summarise(n.agreement = n_distinct(id.agreement),'%.agreement'= round(n.agreement/total.agreement,2))

