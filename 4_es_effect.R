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

df.biomass <- df.solution_alldata

df.biomass <- df.biomass %>% 
  # mutate(tot_biomass = solution_area_share * Biomass) %>% 
  group_by(year, scenario, policy) %>% 
  # summarise(sum_tot_biomass = sum(tot_biomass)) %>% 
  summarise(mean_biomass = mean(Biomass))
  arrange(scenario, year)

# Plot of mean values
plot_biomass_harvest <- df.biomass %>%
  ggplot(aes(x=year, y=mean_biomass, group=scenario))+
  geom_line(aes(color = policy, linetype = scenario))+
  labs(title="Volume of biomass harvests per hectare",x="Year")+
  ylab(expression(m^{3}~ha^{-1}~year^{-1})) +
  scale_x_continuous(breaks = seq(from = 2021, to = 2116, by= 10))+    
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)
plot_biomass_harvest

#ggsave(plot = plot_biomass_harvest, paste0(path,"/outp/plot_biomass_harvest.tiff"), width=10, height=6)



#
# Biodiversity conservation (deadwood volume, decidous tree %)
#

df.bd <- df.solution_alldata

# Deadwood 
df.bd <- df.bd %>% 
  group_by(year, scenario, policy) %>% 
  summarise(mean_deadwood = mean(V_deadwood)) %>% 
  arrange(scenario, year)

plot_deadwood <- df.bd %>%
  ggplot(aes(x=year, y=mean_deadwood, group=scenario))+
  geom_line(aes(color = policy, linetype = scenario))+
  labs(title="Volume of deadwood per hectare",x="Year")+
  ylab(expression(m^{3}~ha^{-1}~year^{-1})) +
  scale_x_continuous(breaks = seq(from = 2021, to = 2116, by= 10))+    
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)
plot_deadwood

#ggsave(plot = plot_deadwood, paste0(path,"/outp/plot_deadwood.tiff"), width=10, height=6)


# Decidous tree % 

df.bd <- df.solution_alldata
df.bd <- df.bd %>% 
  group_by(year, scenario, policy) %>% 
  summarise(mean_decidous = mean(prc_V_deciduous)) %>% 
  arrange(scenario, year)

plot_decidous_trees <- df.bd %>%
  ggplot(aes(x=year, y=mean_decidous, group=scenario))+
  geom_line(aes(color = policy, linetype = scenario))+
  labs(title="Mean % of decidous trees per hectare",x="Year")+
  ylab(expression("%"~ha^{-1}~year^{-1})) +
  scale_x_continuous(breaks = seq(from = 2021, to = 2116, by= 10))+    
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)
plot_decidous_trees

#ggsave(plot = plot_decidous_trees, paste0(path,"/outp/plot_decidous_trees.tiff"), width=10, height=6)



#
# Non-wood products (berries, mushrooms)
#

df.nwp <- df.solution_alldata

df.nwp <- df.nwp %>%
  group_by(year, scenario, policy) %>%
  summarise(mean_mushrooms = mean(ALL_MARKETED_MUSHROOMS),
            mean_bilberry = mean(BILBERRY), 
            mean_cowberry = mean(COWBERRY)) %>% 
  arrange(scenario, year)

plot_nwp <- df.nwp %>%
  ggplot(aes(x=year, group=scenario))+
  geom_line(aes(y = mean_mushrooms, linetype = scenario, color = 'Mushrooms'))+
  geom_line(aes(y = mean_bilberry, linetype = scenario, color = 'Bilberry'))+
  geom_line(aes(y = mean_cowberry, linetype = scenario, color = 'Cowberry'))+
  labs(title="Harvested non wood products",x="Year")+
  ylab(expression(kg^{-1}~ha^{-1}~year^{-1})) +
  scale_x_continuous(breaks = seq(from = 2021, to = 2116, by= 10))+    
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)
plot_nwp

#ggsave(plot = plot_nwp, paste0(path,"/outp/plot_nwp.tiff"), width=10, height=6)



#
# Recreation (recreation index, scenic index)
#

df.rec <- df.solution_alldata

df.rec <- df.rec %>% 
  group_by(year, scenario, policy) %>% 
  summarise(mean_rec = mean(Recreation),
            mean_sce = mean(Scenic)) %>% 
  arrange(scenario, year)

plot_recreation <- df.rec %>%
  ggplot(aes(x=year, group=scenario))+
  geom_line(aes(y = mean_rec, linetype = scenario, color = 'Recreation index'))+
  geom_line(aes(y = mean_sce, linetype = scenario, color = 'Scenic index'))+
  labs(title="Total Recreation & Scenic index ",x="Year")+
  ylab(expression(Indices~ha^{-1}))+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)

plot_recreation
#ggsave(plot = plot_recreation, paste0(path,"/outp/plot_recreation.tiff"), width=10, height=6)


#
# Climate regulation (carbon sink)
#


# Aggregated values at landscape level

df.carbon <- df.solution_alldata

df.carbon <- df.carbon %>% 
  mutate(tot_C_sink = solution_area_share * CARBON_SINK) %>% 
  mutate(tot_C_stored = solution_area_share * CARBON_STORAGE_Update) %>%
  group_by(year, scenario, policy) %>% 
  summarise(sum_tot_C_sink = sum(tot_C_sink), 
            sum_tot_C_storage = sum(tot_C_stored)) %>% 
  arrange(scenario, year)

# Plot of aggregated values
plot_carbon_sink <- df.carbon %>%
  ggplot(aes(x=year, group=scenario))+
  geom_line(aes(y = sum_tot_C_sink, linetype = scenario))+
  labs(title="Carbon sink ",x="Year", y = "kg")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)
plot_carbon_sink
#ggsave(plot = plot_carbon_sink, paste0(path,"/outp/plot_carbon_sink.tiff"), width=10, height=6)


plot_carbon_storage <- df.carbon %>%
  ggplot(aes(x=year, group=scenario))+
  geom_line(aes(y = sum_tot_C_storage, linetype = scenario))+
  labs(title="Carbon storage ",x="Year", y = "kg")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)
plot_carbon_storage
#ggsave(plot = plot_carbon_storage, paste0(path,"/outp/plot_carbon_storage.tiff"), width=10, height=6)


# Mean values per ha

df.carbon <- df.solution_alldata

df.carbon <- df.carbon %>% 
  group_by(year, scenario, policy) %>% 
  summarise(mean_C_sink = mean(CARBON_SINK), 
            mean_C_storage = mean(CARBON_STORAGE_Update)) %>% 
  arrange(scenario, year)

plot_mean_carbon_storage <- df.carbon %>%
  ggplot(aes(x=year, group=scenario))+
  geom_line(aes(y = mean_C_storage, color = policy, linetype = scenario))+
  labs(title="Carbon storage per hectare ",x="Year")+
  ylab(expression(kg^{-1}~ha^{-1}~year^{-1})) +
  scale_x_continuous(breaks = seq(from = 2021, to = 2116, by= 10))+    
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)
plot_mean_carbon_storage
#ggsave(plot = plot_mean_carbon_storage, paste0(path,"/outp/plot_mean_carbon_storage.tiff"), width=10, height=6)


plot_mean_carbon_sink <- df.carbon %>%
  ggplot(aes(x=year, group=scenario))+
  geom_line(aes(y = mean_C_sink, linetype = scenario))+
  labs(title="Mean carbon sink ",x="Year", y = "kg per ha")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ policy)
plot_mean_carbon_sink
#ggsave(plot = plot_mean_carbon_sink, paste0(path,"/outp/plot_mean_carbon_sink.tiff"), width=10, height=6)



#################################################################################################################
#################################################################################################################
  
#
# Collect all ES graphs into one image
#


