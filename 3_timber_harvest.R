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



df.harvest <- select(df.solution_alldata, -c(regime_6class, regime_class))


# To have a line plot over time we need to aggregate our data first a bit, coming form stand values over time to landscape values (total values) over time, see example below for solution_alldata

df.harvest <- df.harvest %>% 
  # first we need to multiply the harvest with the represented forest area of our solution
  mutate(tot_harvest = solution_area_share * Harvested_V) %>% 
  # then we group the data by year because we want to sum up over year, but also by policy and climate
  group_by(year, scenario, policy) %>% 
  # then we summarise
  summarise(sum_tot_harveset = sum(tot_harvest)) %>% 
  arrange(scenario, year)

# Same for pulp and roundwood

df.pulp_log <- df.solution_alldata

df.pulp_log <- df.pulp_log %>%
  mutate(tot_pulp_harvest = solution_area_share * Harvested_V_pulp_under_bark) %>%
  mutate(tot_log_harvest = solution_area_share * Harvested_V_log_under_bark) %>%
  group_by(year, scenario, policy) %>%
  summarise(Total_pulp = sum(tot_pulp_harvest),
            Total_log = sum(tot_log_harvest)) %>% 
  arrange(scenario, year)

head(df.pulp_log)


# #
# # CB: you need to include the below summarise function directly above... see how I have included it there
# #     and separate for pulp and log, then you need not to rename the columns either...
# #
# df.pulp_log <- df.pulp_log %>%
#   summarise((sum_tot_pulp = sum(tot_pulp_harvest)), (sum_tot_log = sum(tot_log_harvest)))
# 
# df.pulp_log <- df.pulp_log %>%
#   rename(Total_pulp = 4) %>%
#   rename(Total_log = 5)




# ----------
# Plot of timber harvests
# ----------

# CB: see parallel the plot example in folder cb/plot_harvest_cb.R

# Timber harvests at landscape level

plot_timber_harvest <- df.harvest %>%
  ggplot(aes(x = year, y = sum_tot_harveset, group=scenario))+
  geom_line(aes(color = policy, linetype = scenario))+
  labs(title="Volume of timber harvests",x="Year")+
  ylab(expression(Million~m^{3}~ha^{-1}~year^{-1})) +
  theme(axis.text.x = element_text(angle = 90))+
  # Labels correspinding to the timesteps of the optimisation
  scale_x_continuous(breaks = seq(from = 2021, to = 2116, by= 10))+    
  # Values of millions of cubic meters to more readable format
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_grid(. ~ policy)

plot_timber_harvest

#ggsave(plot = plot_timber_harvest, paste0(path,"/outp/plot_timber_harvest.tiff"), width=10, height=6)


# Mean timber harvests per hectare

df.harvest <- df.solution_alldata %>% 
  group_by(year, scenario, policy) %>% 
  summarise(mean_harvest = mean(Harvested_V)) %>%
  arrange(scenario, year)

# plot of mean timber harvests

plot_mean_timber_harvest <- df.harvest %>%
  ggplot(aes(x = year, y = mean_harvest, group=scenario))+
  geom_line(aes(color = policy, linetype = scenario))+
  labs(title="Mean volume of timber harvests per hectare",x="Year")+
  ylab(expression(m^{3}~ha^{-1}~year^{-1})) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks = seq(from = 2021, to = 2116, by= 10))+    
  facet_grid(. ~ policy)

plot_mean_timber_harvest
#ggsave(plot = plot_mean_timber_harvest, paste0(path,"/outp/plot_mean_timber_harvest.tiff"), width=10, height=6)



# ----------
# Plot of pulp and log harvests
# ----------


# CB: being able to plot this in way that its around, we might need to re-arrange the data bit...
head(df.pulp_log)

# This makes the two columns total_pulp & total_log from "wide" to "long" format 
# with new column specifying the assortment and its value
df.pulp_log<- df.pulp_log %>% 
  tidyr::gather(assortment, value, 4:5) 


#
# Stuck here, not able to make the bar chart. If this is for the timely mean, is it for a single time step? The plot from row 138 onwards has year as the x axis although this does not exist in df.pulp_log. Should it be assortment instead? I tried this as well and was not able to make it work.
#

# CB: to have an more easier comparison, we could also look only at the timely mean values and 
head(df.pulp_log)

df.pulp_log<-df.pulp_log %>% 
  group_by(scenario, policy, assortment) %>% 
  summarise(timely_mean = mean(value),
            # those - either the sd, or the min/max could be use for error bars in bar plot
            sd = sd(value),
            min = min(value),
            max = max(value))

head(df.pulp_log )

plot.timely_mean <- df.pulp_log %>%
  ggplot(aes(x = assortment, y = timely_mean)) +
  geom_bar(position = "dodge") +
  facet_wrap(.~ policy) 

plot.timely_mean


# see example here http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization




# ----------
# Backup
# ----------

# this can now be plotted keeping the info about pulp and log - similar to the harvest example under "plot_harvest_cb.R"
#df.pulp_log %>% 
#  ggplot(aes(year, value)) +
#  geom_line( aes(color = assortment, linetype = scenario)) +
#  facet_wrap(.~ policy) 
# then you can add hear further specifications to modify the plot: color, axis labes, background, and so on... there are many examples if you google

#plot_pulp_log_harvest <- df.pulp_log %>%
#  ggplot(aes(x=year, group=scenario))+
#  # CB: in this way it 
#  geom_line(aes(y = Total_pulp, linetype = scenario, color = 'Pulp'))+
#  geom_line(aes(y = Total_log, linetype = scenario, color = 'Log'))+
#  labs(title="Total volume of pulp and log harvests",x="Year", y = "million m3")+
#  theme(axis.text.x = element_text(angle = 90))+
#  scale_y_continuous(labels=function(x)x/1000000)+
#  facet_grid(. ~ policy)

# plot_pulp_log_harvest

#ggsave(plot = plot_pulp_log_harvest, paste0(path,"/outp/plot__pulp_log_tot.tiff"), width=10, height=6)






