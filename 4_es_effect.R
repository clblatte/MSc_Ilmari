####
#
# Ecosystem services analysis
# 2022-05-16
#
####


### load libraries
library(dplyr)
library(ggplot2)
library(tidyr)



#
# Bioenergy (biomass)
#

# First aggregate data to see landscape values instead of stand-level values

df.biomass <- df.solution_alldata

df.biomass <- df.biomass %>% 
  mutate(tot_biomass = solution_area_share * Biomass) %>% 
  group_by(year, scenario, policy) %>% 
  summarise(sum_tot_biomass = sum(tot_biomass)) %>% 
  arrange(scenario, year)

# Plot of aggregated values
plot_biomass_harvest <- df.biomass %>%
  ggplot(aes(x=year, y=sum_tot_biomass, group=scenario))+
  geom_line(aes(linetype = scenario))+
  labs(title="Volume of biomass harvests",x="Year", y = "m3")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)

plot_biomass_harvest



#
# Biodiversity conservation (deadwood volume, decidous tree %)
#

df.bd <- df.solution_alldata

# Deadwood 
# Same aggregation as before, total amount of deadwood in the landscape
df.bd <- df.bd %>% 
  mutate(tot_deadwood = solution_area_share * V_deadwood) %>% 
  group_by(year, scenario, policy) %>% 
  summarise(sum_tot_deadwood = sum(tot_deadwood)) %>% 
  arrange(scenario, year)

plot_deadwood <- df.bd %>%
  ggplot(aes(x=year, y=sum_tot_deadwood, group=scenario))+
  geom_line(aes(color = scenario))+
  labs(title="Volume of deadwood",x="Year", y = "m3")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)
plot_deadwood

# It does not make sense to me to take the sum of the % of decidous trees at the landscape level as with previous variables
# I was thinking that the average % over the whole landscape for one year would be more suitable
# How would I begin doing this?

# Decidous tree % 
# Take average value for the whole area for each year
#df.bd <- df.bd %>% 
#  mutate(tot_deadwood = solution_area_share * V_deadwood) %>% 
#  group_by(year, scenario, policy) %>% 
#  summarise(sum_tot_deadwood = sum(tot_deadwood)) %>% 
#  arrange(scenario, year)

#plot_decidous_trees <- df.solution_alldata %>%
#  ggplot(aes(x=year, y=prc_V_deciduous, group=scenario))+
#  geom_line(aes(color = scenario))+
#  labs(title="% volume of decidous trees",x="Year", y = "%")+
#  theme(axis.text.x = element_text(angle = 90))+
#  facet_grid(. ~ policy)
#plot_decidous_trees



#
# Non-wood products (berries, mushrooms)
#

df.nwp <- df.solution_alldata

# Total amounts harvested 
df.nwp <- df.nwp %>%
  mutate(tot_mushrooms = solution_area_share * ALL_MARKETED_MUSHROOMS) %>%
  mutate(tot_bilberry = solution_area_share * BILBERRY) %>%
  mutate(tot_cowberry = solution_area_share * COWBERRY) %>%
  group_by(year, scenario, policy) %>%
  summarise(sum_tot_mushrooms = sum(tot_mushrooms),
            sum_tot_bilberry = sum(tot_bilberry), 
            sum_tot_cowberry = sum(tot_cowberry)) %>% 
  arrange(scenario, year)

# Plot total amounts
plot_nwp <- df.nwp %>%
  ggplot(aes(x=year, group=scenario))+
  geom_line(aes(y = sum_tot_mushrooms, linetype = scenario, color = 'Mushrooms'))+
  geom_line(aes(y = sum_tot_bilberry, linetype = scenario, color = 'Bilberry'))+
  geom_line(aes(y = sum_tot_cowberry, linetype = scenario, color = 'Cowberry'))+
  labs(title="Harvested non wood products",x="Year", y = "kg")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)

plot_nwp



#
# I was not able to test the next two (recreation and carbon) because I have the wrong data and cannot access WinSCP... However the following should produce the types of graphs I want when I have the correct data
#



#
# Recreation (recreation index, scenic index)
#

df.rec <- df.solution_alldata

# Sum total of recreation & scenic indices
df.rec <- df.rec %>% 
  mutate(tot_rec = solution_area_share * Recreation) %>% 
  mutate(tot_sce = solution_area_share * Scenic) %>%
  group_by(year, scenario, policy) %>% 
  summarise(sum_tot_rec = sum(tot_rec)) %>% 
  summarise(sum_tot_sce = sum(tot_sce)) %>%
  arrange(scenario, year)

# Plot of aggregated values

plot_recreation <- df.rec %>%
  ggplot(aes(x=year, group=scenario))+
  geom_line(aes(y = sum_tot_rec, linetype = scenario, color = 'Recreation index'))+
  geom_line(aes(y = sum_tot_sce, linetype = scenario, color = 'Scenic index'))+
  labs(title="Total Recreation & Scenic index ",x="Year", y = "Aggregated index")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)

plot_recreation



#
# Climate regulation (carbon sink)
#

df.carbon <- df.solution_alldata

# Aggregate at landscape level
df.carbon <- df.carbon %>% 
  mutate(tot_C_sink = solution_area_share * CARBON_SINK) %>% 
  mutate(tot_C_stored = solution_area_share * CARBON_STORAGE_Update) %>%
  group_by(year, scenario, policy) %>% 
  summarise(sum_tot_C_sink = sum(tot_C_sink)) %>% 
  summarise(sum_tot_C_storage = sum(tot_C_stored)) %>% 
  arrange(scenario, year)

# Plot of aggregated values
plot_carbon <- df.carbon %>%
  ggplot(aes(x=year, group=scenario))+
  geom_line(aes(y = sum_tot_C_sink, linetype = scenario, color = 'Carbon Sink'))+
  geom_line(aes(y = sum_tot_C_sink, linetype = scenario, color = 'Total carbon stored'))+
  labs(title="Carbon sink & storage ",x="Year", y = "kg")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)

plot_carbon


#
# Collect all ES graphs into one image
#




