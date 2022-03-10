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


# But therefor first the "df.refall" and the "df.solution_alldata" need to be combined
# This requires again that they are having the same columns!! But this time we are additionally interested in the columns:
# "Harvested_V" and e.g.  "Harvested_V_log_under_bark"  "Harvested_V_pulp_under_bark"

names(df.solution_alldata)
names(df.refall)

# To have a line plot over time we need to aggregate our data first a bit, coming form stand values over time to landscape values (total values) over time.
# See example below for solution_alldata
# This would need to be done for the "new combined data set"

df.harvest <- df.solution_alldata %>% 
  # first we need to multiply the harvest with the represented forest area of our solution
  mutate(tot_harvest = solution_area_share * Harvested_V) %>% 
  # then we group the data by year because we want to sum up over year, but also by policy and climate
  group_by(year, scenario, policy) %>% 
  # then we summarise
  summarise(sum_tot_harveset = sum(tot_harvest)) %>% 
  arrange(scenario, year)

# This aggregated values can the be plotted using ggplot and geom_line
# I guess you will figure out how to do it :-)





