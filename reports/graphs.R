#---------------------------------#
#----  graph Loans by type  ------
#---------------------------------#

#loans.by.type<- loans.by.type %>% arrange(desc(`%.gbv`))
loan.type.plot <- loans.by.type[2:4,]  %>% mutate(`type` = fct_reorder(type,`%.gbv`))%>%
  ggplot(aes(x =type , y = `%.gbv`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.1f%%", `%.gbv`*100),group = `type`, y = `%.gbv`/2 , vjust = 0.5,hjust = "center"),size = 3) +
  geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6),vjust = -0.4, hjust = 0.4),size = 3) +
  xlab("Type Of Loan") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 100,accuracy = 10)) +
  theme(
    panel.border = element_blank(),
    panel.grid.minor.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  )

loan.type.plot

ggsave("File/loan.type.png",plot = loan.type.plot)




#---------------------------------#
#----  graph entity by type ------
#---------------------------------#

entity.type.plot <- ent.by.type[2:3,] %>% mutate(`type.subject` = fct_reorder(`type.subject`,`%.gbv`))%>%
  ggplot(aes(x =type.subject , y = `%.gbv`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.1f%%", `%.gbv`*100),group = `type.subject`, y = `%.gbv`/2 , vjust = 0.5,hjust = "center"),size = 3) +
  #geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6),vjust = -0.4, hjust = 0.4),size = 3) +
  xlab("Type Of Entity") +
  theme_bw() +
scale_y_continuous(labels = scales::percent_format(scale = 100,accuracy = 10)) +
  theme(
    panel.border = element_blank(),
    panel.grid.minor.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  ) +
labs(title = " ", subtitle= " ")

entity.type.plot 
grid.text("Sum GBV", x=0.15, y=0.94, gp=gpar(fontsize=9))
grid.roundrect(width=.15, height=.1, name="rr", x=0.34, y=0.94, gp=gpar(col="#1b8bbe"))
grid.text("8.6 M", x=0.34, y=0.94, gp=gpar(fontsize=9))
grid.roundrect(width=.15, height=.1, name="rr", x=0.74, y=0.94, gp=gpar(col="#1b8bbe"))
grid.text("17.0 M", x=0.74, y=0.94, gp=gpar(fontsize=9))

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
  xlim(0,85) +
  theme(
    panel.border = element_blank(),
    panel.grid.minor.x = element_line(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  )

area_plot

ggsave("File/Pie_Chart.png",plot = area_plot)



#----------------------------------------#
#----  Top 5 Province  ------
#----------------------------------------#

province_plot <- Top_5_province_by_gbv[2:6,] %>% mutate(`province` = fct_reorder(`province`,`sum.gbv`)) %>% 
  ggplot(aes(x = `sum.gbv` , y = `province`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6)), hjust = 1.1,size = 3) +
  xlab("Sum of GBV (Millions)") +
  theme_bw() +
  #xlim(0,16*1e6) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  theme(
    panel.border = element_blank(),
    panel.grid.minor.x = element_line(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  )

province_plot

ggsave("File/province_plot.png",plot = province_plot)


#----------------------------------------#
#----  Corporate Status  ------
#----------------------------------------#

corporate.status.plot <- corporate.status[2:8,] %>% mutate(`corporate status` = fct_reorder(`corporate status`,`%.gbv`))%>%
  ggplot(aes(x =`corporate status` , y = `%.gbv`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.1f%%", `%.gbv`*100), group = `corporate status`, y = `%.gbv`/2 , vjust = 0.5,hjust = "center"),size = 3) +
  geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6),vjust = -0.7, hjust = 0.5),size = 3) +
  xlab("Corporate's Status") +
  theme_bw() + theme(axis.text.x= element_text(angle=10)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100,accuracy = 10)) +
  theme(
    panel.border = element_blank(),
    panel.grid.minor.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  )


corporate.status.plot

ggsave("File/corporate.status.png",plot = corporate.status.plot)



#
library(grid)



# }
