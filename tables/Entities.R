colnames(anagrafiche)
entities <- anagrafiche %>% select(name, cf.piva, type.subject=forma.giuridica, city=comune, province=provincia, data.di.nascita)
#id
entities$id.entity <- paste0("e", seq_len(nrow(counterparties))) 

entities <- entities %>% mutate(dummy.info=NA,
                                solvency.pf=NA,
                                income.pf=NA,
                                date.cessation=NA,
                                status.pg=NA,
                                flag.imputed=NA,
                                region=NA,
                                area = NA)

entities <- entities %>% mutate(age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(data.di.nascita, "%Y")))
entities <- add_age_range_column(entities)

#Adds 0 in start when needed 
entities$cf.piva <- clean_cf.piva(entities$cf.piva)

#Change type.subject column
entities <- entities %>% mutate(
  type.subject = ifelse(
    type.subject == "ind" | type.subject == "prv",
    "individual",
    "corporate"
  )
)

entities <- add_sex_column(entities)

entities <- add_type.pg_column(entities)


entities <- entities %>% select(id.entity, name, cf.piva, type.subject, dummy.info, sex, range.age, age, solvency.pf, income.pf, type.pg, status.pg, date.cessation, city, province, region, area, flag.imputed)
