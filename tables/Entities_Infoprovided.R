#We take the info we want from entities, from infoproviding, and we join them.
pf <- entities %>% filter(type.subject=="individual") %>% select(id.entity, name, cf.piva, city)
pf <- pf %>% left_join(infoprov.PF %>% select(cf.piva, solvency.pf=solvency.adj, income.pf=income.net, city), 
                       by ="cf.piva")
pf <- pf %>% mutate(
  city= ifelse(!is.na(city.y), city.y, city.x),
  type.subject = "individual",
  dummy.info = ifelse(cf.piva %in% infoprov.PF$cf.piva, 1, 0),
  date.infoprov = as.Date(2023-07-12)
  )  



#We take the info we want from entities, from infoproviding, and we join them.
pg <- entities %>% filter(type.subject=="corporate" & !is.na(cf.piva)) %>% select(id.entity, name, cf.piva, city)
pg <- pg %>% left_join(Infoprov_PG %>% select(cf.piva, type.pg=type, status.pg=status, date.cessation, city), 
                       by ="cf.piva")
pg <- pg %>% mutate(
  city= ifelse(!is.na(city.y), city.y, city.x),
  type.subject = "corporate",
  dummy.info = ifelse(cf.piva %in% Infoprov_PG$cf.piva, 1, 0),
  date.infoprov = as.Date(2023-07-13)
)  

#We take the info we want from entities, from infoproviding, and we join them.
senza_cf.piva <- entities %>% filter(is.na(cf.piva)) %>% select(id.entity, name, city)
senza_cf.piva <- senza_cf.piva %>% 
  left_join(infoprov.PF %>% select(name, cf.piva, solvency.pf=solvency.adj, income.pf =income.net, city), 
            by ="name")
senza_cf.piva <- senza_cf.piva %>% mutate(
  city= ifelse(!is.na(city.y), city.y, city.x),
  type.subject = "individual",
  dummy.info = ifelse(cf.piva %in% infoprov.PF$cf.piva, 1, 0),
  date.infoprov = as.Date(2023-07-13)
)  

#We recalculate some information in case some cf.piva were wrong, and to add it in the cases we didn't have the cf.piva before.
updated.entities <- bind_rows(pf, pg, senza_cf.piva) 
updated.entities <- add_sex_column(updated.entities)
updated.entities <- add_age_column(updated.entities)
updated.entities <- add_age_range_column(updated.entities)
updated.entities <- update_type.pg_column(updated.entities)
updated.entities <- update_status.pg(updated.entities)
updated.entities <- left_join(updated.entities, GEO.metadata %>% select(city, province, region, area), by = "city", relationship = "many-to-many")
updated.entities <- updated.entities %>% distinct()
updated.entities <- updated.entities %>% mutate(flag.imputed = 0)
updated.entities <- updated.entities %>% select(id.entity, name, cf.piva, type.subject, dummy.info, sex, range.age, age, solvency.pf, income.pf, type.pg, status.pg, date.cessation, city, province, region, area, flag.imputed)
