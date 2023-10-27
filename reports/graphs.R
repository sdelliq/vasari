#---------------------------------#
#----  graph Loans by type  ------
#---------------------------------#

#loans.by.type<- loans.by.type %>% arrange(desc(`%.gbv`))
loan.type.plot <- loans.by.type[2:4,]  %>% mutate(`type` = fct_reorder(type,`%.gbv`))%>%
  ggplot(aes(x =type , y = `%.gbv`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.2f%%", `%.gbv`*100),group = `type`, y = `%.gbv`/2 , vjust = 0.5,hjust = "center"),size = 3) +
  geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6),vjust = -0.4, hjust = 0.4),size = 3) +
  xlab("Type Of Loan") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 100,accuracy = 10))


loan.type.plot

ggsave("File/loan.type.png",plot = loan.type.plot)




#---------------------------------#
#----  graph entity by type ------
#---------------------------------#

entity.type.plot <- ent.by.type[2:3,] %>% mutate(`type.subject` = fct_reorder(`type.subject`,`%.gbv`))%>%
  ggplot(aes(x =type.subject , y = `%.gbv`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.2f%%", `%.gbv`*100),group = `type.subject`, y = `%.gbv`/2 , vjust = 0.5,hjust = "center"),size = 3) +
  geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6),vjust = -0.4, hjust = 0.4),size = 3) +
  xlab("Type Of Entity") +
  theme_bw() +
scale_y_continuous(labels = scales::percent_format(scale = 100,accuracy = 10))


entity.type.plot

ggsave("File/entity.type.png",plot = entity.type.plot)



#----------------------------------------#
#----  entities by area  ------
#----------------------------------------#


ent.by.area[is.na(ent.by.area)] <- "N/A"
ent.by.area_no_totals <- ent.by.area[2:6,]
ent.by.area_no_totals$`%.gbv` <-ent.by.area_no_totals$`%.gbv`*100
area_plot <- ent.by.area_no_totals %>% mutate(`area` = fct_reorder(`area`,`%.gbv`)) %>% 
  ggplot(aes(x = `%.gbv` , y = `area`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.1f%%", `%.gbv`)), hjust = -0.1,size = 3) +
  xlab("Sum of GBV %") +
  theme_bw() +
  #scale_x_continuous(labels = scales::percent_format(scale = 100,accuracy = 10)) +
  xlim(0,100)

area_plot

ggsave("File/Pie_Chart.png",plot = area_plot)



#----------------------------------------#
#----  Top 5 Province  ------
#----------------------------------------#

province_plot <- Top_5_province_by_gbv %>% mutate(`province` = fct_reorder(`province`,`sum.gbv`)) %>% 
  ggplot(aes(x = `sum.gbv` , y = `province`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6)), hjust = 1,size = 3) +
  xlab("Sum of GBV (Millions)") +
  theme_bw() +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))

province_plot

ggsave("File/province_plot.png",plot = province_plot)

