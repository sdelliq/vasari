library(ggplot2)
#---------------------------------#
#----  graph Loans by type  ------
#---------------------------------#

#loans.by.type<- loans.by.type %>% arrange(desc(`%.gbv`))
loan.type.plot <- loans.by.type  %>%
  ggplot(aes(x =type , y = `%.gbv`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.2f%%", `%.gbv`),group = `type`, y = `%.gbv`/2 , vjust = 0.5,hjust = "center")) +
  geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6),vjust = -0.4, hjust = 0.4)) +
  xlab("Type Of Loan") +
  theme_bw() #+
  #scale_y_continuous(labels = scales::percent_format(scale = 1,accuracy = 1))


loan.type.plot

ggsave("File/loan.type.png",plot = loan.type.plot,width = 8,height = 6,units = "in")




#---------------------------------#
#----  graph entity by type ------
#---------------------------------#

entity.type.plot <- ent.by.type %>%
  ggplot(aes(x =type.subject , y = `%.gbv`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.2f%%", `%.gbv`),group = `type.subject`, y = `%.gbv`/2 , vjust = 0.5,hjust = "center")) +
  geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6),vjust = -0.4, hjust = 0.4)) +
  xlab("Type Of Entity") +
  theme_bw() #+
#scale_y_continuous(labels = scales::percent_format(scale = 1,accuracy = 1))


entity.type.plot

ggsave("File/entity.type.png",plot = entity.type.plot,width = 8,height = 6,units = "in")



#----------------------------------------#
#----  entities by area  ------
#----------------------------------------#


myPalette <- c("#FF5733", "#33FFFF", "#33FF57", "#5733FF", "#FFFF33")

ent.by.area[is.na(ent.by.area)] <- "N/A"
ent.by.area_no_totals <- ent.by.area[2:6,]
pie_chart <- ggplot(data = ent.by.area_no_totals, aes(x = 1, y = `%.gbv`, fill = area)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = myPalette) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "Areas") +
  ggtitle("GBV Distribution by Area")

pie_chart <- pie_chart +
  geom_text(data = ent.by.area_no_totals, aes(label = scales::percent(`%.gbv` / sum(`%.gbv`)), x = 1.7), position = position_stack(vjust = 0.5))

print(pie_chart)

ggsave("File/Pie_Chart.png",plot = pie_chart)



#----------------------------------------#
#----  Top 5 Province  ------
#----------------------------------------#

province_plot <- Top_5_province_by_gbv %>% mutate(`province` = fct_reorder(`province`,`sum.gbv`)) %>% 
  ggplot(aes(x = `sum.gbv` , y = `province`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6)), hjust = 1) +
  xlab("Sum of GBV (Millions)") +
  theme_bw() +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))

province_plot

ggsave("File/province_plot.png",plot = province_plot,width = 8,height = 6,units = "in")

