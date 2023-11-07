
agreement.dcf <- agreement.proj

agreement.dcf <- agreement.dcf %>% mutate(year = format(date.due,'%Y'))

agreement.dcf <- agreement.dcf %>% group_by(year) %>%
  summarise(cash = sum(amount.due), paid = sum(amount.paid))
agreement.dcf <- agreement.dcf %>% mutate(acc.cash = cumsum(cash),paid = ifelse(paid !=0 ,cumsum(paid),NA))
agreement.dcf$paid[3] <- NA
r <- 0.3

# calculate_dcf_vector <- function(cash_flows, discount_rate) {
#   dcf_vector <- numeric(length(cash_flows))
#   dcf_vector[1] <- cash_flows[1] / ((1 + discount_rate))
#   for (i in 2:length(cash_flows)) {
#     dcf_vector[i] <-dcf_vector[i-1] + cash_flows[i] / ((1 + discount_rate) ^ i)
#   }
#   return(dcf_vector)
# }
# 
# agreement.dcf$dcf <- calculate_dcf_vector(agreement.dcf$cash,r)

calculate_dcf_vector <- function(cash_flows, discount_rate) {
  dcf_vector <- numeric(length(cash_flows))
  dcf_vector[1] <- agreement.dcf$paid[1]
  dcf_vector[2] <- agreement.dcf$paid[2]
  for (i in 3:length(cash_flows)) {
    dcf_vector[i] <-dcf_vector[i-1] + cash_flows[i] / ((1 + discount_rate) ^ i)
  }
  return(dcf_vector)
}

agreement.dcf$dcf <- calculate_dcf_vector(agreement.dcf$cash,r)

dcf_plot <- agreement.dcf %>% 
  ggplot(aes(x = year , y = dcf)) +
  geom_line(aes(y = acc.cash), color = "#57c1ef", linetype = "solid", size = 0.8, group = 1) +
  geom_line(aes(y = dcf), color = "blue", linetype = "dashed", size = 0.8, group = 2) +
  geom_line(aes(y = paid), color = "blue", linetype = "solid", size = 0.8, group = 3) +
  labs(title = "Line Plot ", x = "Year", y = "Value")

dcf_plot
