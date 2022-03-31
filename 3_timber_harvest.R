####
#
# Economical analysis - timber harvest
# 2022-03-10
#
####


### load libraries
library(dplyr)
library(ggplot2)
library(tidyr)




# Let´s aim for analysis the harvests
# E.g. getting a line plot that shows the total harvests over the time under the different reference scenarios
# works with ggplot and geom line; e.g. http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
# on x axis we want to have the year and on y axis the total harvest


df.harvest <- select(df.solution_alldata, -c(Old_forests, regime_6class, regime_class))


# To have a line plot over time we need to aggregate our data first a bit, coming form stand values over time to landscape values (total values) over time.
# See example below for solution_alldata
# This would need to be done for the "new combined data set"

df.harvest <- df.harvest %>% 
  # first we need to multiply the harvest with the represented forest area of our solution
  mutate(tot_harvest = solution_area_share * Harvested_V) %>% 
  # then we group the data by year because we want to sum up over year, but also by policy and climate
  group_by(year, scenario, policy) %>% 
  # then we summarise
  summarise(sum_tot_harveset = sum(tot_harvest)) %>% 
  arrange(scenario, year)


#
# Same for pulp and roundwood
#

df.pulp_log <- df.solution_alldata

df.pulp_log <- df.pulp_log %>%
  mutate(tot_pulp_harvest = solution_area_share * Harvested_V_pulp_under_bark) %>%
  mutate(tot_log_harvest = solution_area_share * Harvested_V_log_under_bark) %>%
  group_by(year, scenario, policy) %>%
  arrange(scenario, year)

df.pulp_log <- df.pulp_log %>%
  summarise((sum_tot_pulp = sum(tot_pulp_harvest)), (sum_tot_log = sum(tot_log_harvest)))

df.pulp_log <- df.pulp_log %>%
  rename(Total_pulp = 4) %>%
  rename(Total_log = 5)


#
# Plot of timber harvests
#

plot_timber_harvest <- df.harvest %>%
  ggplot(aes(x=year, y=sum_tot_harveset, group=scenario))+
  geom_line(aes(color = scenario))+
  labs(title="Volume of timber harvests",x="Year", y = "million m3")+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(labels=function(x)x/1000000)+
  facet_grid(. ~ policy)

plot_timber_harvest
#ggsave(plot = plot_timber_harvest, paste0(path,"/outp/plot_timber_harvest_V?.tiff"), width=10, height=10)


#
# Plot of pulp and log harvests
#

plot_pulp_log_harvest <- df.pulp_log %>%
  ggplot(aes(x=year, group=scenario))+
  geom_line(aes(y = Total_pulp, color = scenario))+
  geom_line(aes(y = Total_log, color = scenario))+
  labs(title="Volume of pulp and log harvests",x="Year", y = "million m3")+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(labels=function(x)x/1000000)+
  facet_grid(. ~ policy)

plot_pulp_log_harvest
#ggsave(plot = plot_pulp_log_harvest, paste0(path,"/outp/plot__plot_and_log_V3.tiff"), width=10, height=10)







###############################################################################
# Backup
#
#df.pulp <- df.pulp_log %>%
#  summarise(sum_tot_pulp = sum(tot_pulp_harvest), )
#
#df.log <- df.pulp_log %>%
#  summarise(sum_tot_log = sum(tot_log_harvest))
#
# Plot of pulp harvests
#
#plot_pulp_harvest <- df.pulp %>%
#  ggplot(aes(x=year, y=sum_tot_pulp, group=scenario))+
#  geom_line(aes(color = scenario))+
#  labs(title="Volume of harvested pulp under bark",x="Year", y = "million 3")+
#  theme(axis.text.x = element_text(angle = 90))+
#  scale_y_continuous(labels=function(x)x/1000000)+
#  facet_grid(. ~ policy)
#
#plot_pulp_harvest
#ggsave(plot = plot_pulp_harvest, paste0(path,"/outp/plot_pulp_harvest_V2.tiff"), width=10, height=10)
#
# Plot of roundwood harvests
#
#
#plot_log_harvest <- df.log %>%
#  ggplot(aes(x=year, y=sum_tot_log, group=scenario))+
#  geom_line(aes(color = scenario))+
#  labs(title="Volume of harvested log under bark",x="Year", y = "milion m3")+
#  theme(axis.text.x = element_text(angle = 90))+
#  scale_y_continuous(labels=function(x)x/1000000)+
#  facet_grid(. ~ policy)#

#plot_log_harvest
#ggsave(plot = plot_log_harvest, paste0(path,"/outp/plot_log_harvest_V2.tiff"), width=10, height=10)
