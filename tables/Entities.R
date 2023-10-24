
entities <- anagrafiche %>% select(name, cf.piva, type.subject=forma.giuridica, city=comune, province=provincia, data.di.nascita)
entities <- divide_column_by_character(entities, name, ",")
entities <- entities %>% distinct()

entities <- entities %>% group_by(name) %>% 
  summarise(
    cf.piva = ifelse(all(is.na(cf.piva)), NA, cf.piva[!is.na(cf.piva)]),
    type.subject = first(type.subject),
    city = first(city),
    province = first(province),
    data.di.nascita = first(data.di.nascita)
    )

#id
entities$id.entity <- paste0("e", seq_len(nrow(entities))) 

entities <- entities %>% mutate(dummy.info=NA,
                                solvency.pf=NA,
                                income.pf=NA,
                                date.cessation=NA,
                                status.pg=NA,
                                flag.imputed=NA)

entities <- entities %>% 
  mutate(data.di.nascita = as.Date(data.di.nascita, format = "%Y-%m-%d")) %>%
  mutate(age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(data.di.nascita, "%Y")))
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

paths_content <- readLines(".paths.txt")
geoMetadataPath_line <- grep("^geoMetadataPath=", paths_content)
geoMetadataPath_value <- sub("^geoMetadataPath=\\s*", "", paths_content[geoMetadataPath_line])

GEO.metadata <- read_excel(geoMetadataPath_value, sheet = "Geo")
GEO.metadata <- GEO.metadata %>% mutate_all(tolower)

entities <- entities %>% rename("or.province" =province)

# Merge specific columns from 'city_info' into 'ENTITIES'
entities <- left_join(entities, GEO.metadata %>% select(city, province, region, area), by = "city")
entities <- entities %>% distinct()


entities <- entities %>% select(id.entity, name, cf.piva, type.subject, dummy.info, sex, range.age, age, solvency.pf, income.pf, type.pg, status.pg, date.cessation, city, province, region, area, flag.imputed)
