pf <- entities %>% filter(type.subject=="individual")
pg <- entities %>% filter(type.subject=="corporate" & !is.na(cf.piva)) %>% select(-date.cessation)
senza_cf.piva <- entities %>% filter(is.na(cf.piva)) %>% select(-cf.piva)

pf <- pf %>% left_join(infoprov.PF %>% select(cf.piva, solvency.base, income.net), by ="cf.piva")
pg <- pg %>% left_join(Infoprov_PG %>% select(cf.piva, type, status, date.cessation), by ="cf.piva")
senza_cf.piva <- senza_cf.piva %>% left_join(infoprov.PF %>% select(name, cf.piva, solvency.base, income.net), by ="name")

pf <- pf %>% mutate(solvency.pf=solvency.base,
                    income.pf = income.net,
                    dummy.info = 1) %>%
  select(-income.net, -solvency.base)

pg <- pg %>% mutate(type.pg=type,
                    status.pg=status,
                    dummy.info = ifelse(!is.na(cf.piva), 1, 0)) %>%
  select(-type, -status)

senza_cf.piva <- senza_cf.piva %>% mutate(solvency.pf=solvency.base,
                    income.pf = income.net,
                    dummy.info = ifelse(is.na(cf.piva), 0, 1))  %>%
  select(-income.net, -solvency.base)

updated.entities <- rbind(pf, pg, senza_cf.piva)
updated.entities <- add_type_subject_column(updated.entities)
