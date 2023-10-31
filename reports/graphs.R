add_grid_elements <- function(values, x_positions, width.rec, height.rec, ratio) {
  for (i in 1:length(values)) {
    grid.roundrect(width = width.rec, height = height.rec, name = "rr", x = x_positions[i], y = 0.94, r=unit(ratio, "snpc"), gp = gpar(col = "#1b8bbe"))
    grid.text(values[i], x = x_positions[i], y = 0.94, gp = gpar(fontsize = 9))
  }
}

#---------------------------------#
#----  graph Loans by type  ------
#---------------------------------#

#loans.by.type<- loans.by.type %>% arrange(desc(`%.gbv`))
dev.new()
loan.type.plot <- loans.by.type[2:4,]  %>% mutate(`type` = fct_reorder(type,`%.gbv`))%>%
  ggplot(aes(x =type , y = `%.gbv`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.1f%%", `%.gbv`*100),group = `type`, y = `%.gbv`/2 , vjust = 0.5,hjust = "center"),size = 3) +
  #geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6),vjust = -0.4, hjust = 0.4),size = 3) +
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
  ) +
labs(title = " ", subtitle= " ")

png("File/loan.type.png", units = "px") 
loan.type.plot 
grid.text("Sum GBV", x=0.12, y=0.94, gp=gpar(fontsize=9))
values <- c("5.0 M", "8.4 M", "12.2 M")
n_values <- length(values)
# Input the first and last rectangles
x_positions <- seq(0.26, 0.82, length.out = n_values)

add_grid_elements(values, x_positions, 0.12, 0.06, 0.2)

dev.off()


#---------------------------------#
#----  graph entity by type ------
#---------------------------------#
dev.new()
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

png("File/entity.type.png", units = "px") 
entity.type.plot
grid.text("Sum GBV", x=0.15, y=0.94, gp=gpar(fontsize=9))
values <- c("8.6 M", "17.0 M")
n_values <- length(values)
x_positions <- seq(0.34, 0.74, length.out = n_values)
add_grid_elements(values, x_positions, 0.16, 0.06, 0.2)
dev.off()


#----------------------------------------#
#----  Corporate Status  ------
#----------------------------------------#
dev.new()
corporate.status.plot <- corporate.status[2:8,] %>% mutate(`corporate status` = fct_reorder(`corporate status`,`%.gbv`))%>%
  ggplot(aes(x =`corporate status` , y = `%.gbv`)) +
  geom_col(fill = "#57c1ef", alpha = 0.6, width = 0.4) +
  geom_text(aes(label = sprintf("%.1f%%", `%.gbv`*100), group = `corporate status`, y = `%.gbv`/2 , vjust = 0.5,hjust = "center"),size = 3) +
  #geom_text(aes(label = sprintf("%.1fM", `sum.gbv` / 1e6),vjust = -0.7, hjust = 0.5),size = 3) +
  xlab("Corporate's Status") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 100,accuracy = 5)) +
  theme(
    panel.border = element_blank(),
    panel.grid.minor.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_line(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  )+
  labs(title = " ", subtitle= " ")


png("File/corporate.status.png", units = "px") 
corporate.status.plot
grid.text("Sum GBV", x=0.05, y=0.94, gp=gpar(fontsize=9))
values <- c("0.0 M", "1.2 M", "1.9 M", "2.8 M", "3.2 M", "3.8 M", "4.1 M")
n_values <- length(values)
x_positions <- seq(0.17, 0.91, length.out = n_values)
add_grid_elements(values, x_positions, 0.057, 0.047, 0.12)
dev.off()

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
  )+
  labs(title = " ", subtitle= " ")

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
  )+
labs(title = " ", subtitle= " ")

province_plot

ggsave("File/province_plot.png",plot = province_plot)




