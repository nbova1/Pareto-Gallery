library(readr)
library(tidyverse)
library(ggplot2)
library(ggQC)

#Pareto Chart using stat_pareto.
#Bar fill is preset based on percent of observations
#Cum percent is calculated by the graph. The top 50 customers graph reports the cum percent for that subgroup, not the entire population.
sales_hist <- read_rds("rda/sales_hist.rda")
sales_hist %>% group_by(Customer) %>%
  summarize(total_quantity = sum(Quantity), cg = first(customer_group)) %>%
  arrange(desc(total_quantity)) %>%
  mutate(cum_sales = cumsum(total_quantity))%>%
  mutate(Customer = factor(Customer, levels = Customer)) %>%
  slice_head(n = 50) %>%
  ggplot(aes(x=Customer, y=total_quantity))+
  # geom_bar(aes(fill = cg), stat="identity") +
  stat_pareto(point.size = 2,
              point.color = "red",
              bars.fill = c("blue","orange"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title = "Customer Pareto",
       subtitle = ("Top 50 Customers"))

#using ggplot
#
options(scipen = 999)
sales_hist %>% group_by(Customer) %>%
  summarize(total_quantity = sum(Quantity), cg = first(customer_group)) %>%
  arrange(desc(total_quantity)) %>%
  mutate(cum_sales = cumsum(total_quantity))%>%
  mutate(Customer = factor(Customer, levels = Customer)) %>%
  slice_head(n = 50) %>%
  ggplot(aes(x=Customer))+
  geom_bar(aes(y=total_quantity, fill = cg), stat="identity") +
  geom_point(aes(y=cum_sales)) +
  geom_path(aes(y=cum_sales), group = 1) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title = "Customer Pareto",
       subtitle = ("Top 50 Customers"))

# reproduces the chart with cum probability shown for the top 50 subgroup
options(scipen = 999)
sh2 <- sales_hist %>% group_by(Customer) %>%
  summarize(total_quantity = sum(Quantity), cg = first(customer_group)) %>%
  arrange(desc(total_quantity)) %>%
  mutate(cum_sales = cumsum(total_quantity))%>%
  mutate(Customer = factor(Customer, levels = Customer)) %>%
  slice_head(n = 50)
sh2 %>%  ggplot(aes(x=Customer))+
  geom_bar(aes(y=total_quantity, fill = cg), stat="identity") +
  geom_point(aes(y=cum_sales)) +
  geom_path(aes(y=cum_sales), group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~. / max(sh2$cum_sales), labels = scales::percent))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title = "Customer Pareto",
       subtitle = ("Top 50 Customers"))
  
# graph showing the cum percent for the population
options(scipen = 999)
sh2 <- sales_hist %>% group_by(Customer) %>%
  summarize(total_quantity = sum(Quantity), cg = first(customer_group)) %>%
  arrange(desc(total_quantity)) %>%
  mutate(cum_sales = cumsum(total_quantity))%>%
  mutate(Customer = factor(Customer, levels = Customer))
  
brks <- c(seq(0, 1.0, 0.05))
sh2 %>%  slice_head(n = 50) %>%
  ggplot(aes(x=Customer))+
  geom_bar(aes(y=total_quantity, fill = cg), stat="identity") +
  geom_point(aes(y=cum_sales)) +
  geom_path(aes(y=cum_sales), group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~. / max(sh2$cum_sales), labels = scales::percent, breaks = brks))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title = "Customer Pareto",
       subtitle = ("Top 50 Customers"),
       fill = "Customer\nGroup",
       y = "Tons Shipped 2019",
       x = "Customer ID")



