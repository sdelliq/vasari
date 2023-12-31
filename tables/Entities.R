
entities <- anagrafiche %>% select(id.bor, name, cf.piva, type.subject=forma.giuridica, city=comune, province=provincia, data.di.nascita)
entities <- divide_column_by_character(entities, name, ",")
entities <- entities %>% distinct()

entities <- entities %>% group_by(name) %>% 
  summarise(
    id.bor = paste(id.bor, collapse = ","),
    cf.piva = ifelse(all(is.na(cf.piva)), NA, cf.piva[!is.na(cf.piva)]),
    type.subject = ifelse("prv" %in% type.subject | "ind" %in% type.subject, "individual", "corporate"),
    city = first(city),
    province = first(province),
    data.di.nascita = first(data.di.nascita)
    )
entities <- divide_column_by_character(entities, cf.piva, ",")

#id
entities$id.entity <- paste0("e", seq_len(nrow(entities))) 

entities <- entities %>% mutate(dummy.info=NA,
                                solvency.pf=NA,
                                income.pf=NA,
                                date.cessation=NA,
                                status.pg=NA,
                                flag.imputed=NA)

entities <- add_type_subject_column(entities)
entities <- entities %>% 
  mutate(data.di.nascita = as.Date(data.di.nascita, format = "%Y-%m-%d")) %>%
  mutate(age= ifelse(type.subject=="individual", as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(data.di.nascita, "%Y")),NA))
entities <- add_age_range_column(entities)

#Adds 0 in start when needed 
entities$cf.piva <- clean_cf.piva(entities$cf.piva)

entities <- add_sex_column(entities)

entities <- add_type.pg_column(entities)

paths_content <- readLines(".paths.txt")
geoMetadataPath_line <- grep("^geoMetadataPath=", paths_content)
geoMetadataPath_value <- sub("^geoMetadataPath=\\s*", "", paths_content[geoMetadataPath_line])

GEO.metadata <- read_excel(geoMetadataPath_value, sheet = "Geo")
GEO.metadata <- GEO.metadata %>% mutate_all(tolower)

entities <- entities %>% rename("or.province" =province)

# Merge specific columns from 'city_info' into 'ENTITIES'
entities <- left_join(entities, GEO.metadata %>% select(city, province, region, area), by = "city", relationship = "many-to-many")
entities <- entities %>% distinct()


entities <- entities %>% select(id.bor, id.entity, name, cf.piva, type.subject, dummy.info, sex, range.age, age, solvency.pf, income.pf, type.pg, status.pg, date.cessation, city, province, region, area, flag.imputed)

entities$name <- gsub(' time house','',entities$name)


