####
#
# Graphical analysis of the optimal regime shares
# 2022-02-01
#
####


### load libraries
library(dplyr)
library(ggplot2)
library(tidyr)




# --------------------
# plot optimal solution: !! considering the represented forest area by plot !!
# --------------------

# --------------------
# Single regimes 
# --------------------


# calculate the percentage share for each regime according
single_regime_prc <- df.solution %>% 
  # group by scenario, policy, and regime
  group_by(scenario, policy, regime) %>% 
  # and summarize the area by regime
  # using here the "solution_area_share" which was created under 1_load_opt_data.R and which is simply stand_share * by represented_area_by_NFIplot
  # reason: stand_share can be < 1, which means a stand where assigned two optimal regimes, e.g. each with a share of 0.5
  summarise( tot_area = sum(solution_area_share)) %>% 
  # calculate also the total area of the region Keski Suomi
  group_by(scenario, policy) %>% 
  mutate(sum_tot_area = sum(tot_area)) %>% 
  # calculate prc share
  mutate(prc_share = round((tot_area / sum_tot_area)*100, digits = 1) ) %>% 
  ungroup()


# plot the single regimes - next by next
plot.regime <- single_regime_prc %>%   
  ggplot(aes(x=regime, y=prc_share) ) +
  geom_bar(aes(fill=policy), position="dodge", stat="identity" )+
  ylab("regime share [%]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x=element_blank()) +
  facet_wrap(. ~ scenario)
plot.regime

# you can also save the outputs to include them later in your thesis
ggsave(plot = plot.regime, paste0(path,"/outp/plot.regime.tiff"), width=6, height=4) # you can specify the size of plot that it looks nice


# plot the single regimes - on top
plot.regime_stack <- single_regime_prc %>% 
  ggplot(aes(x=scenario, y=prc_share) ) +
  geom_bar(aes(fill=regime), position="stack", stat="identity" ) +
  theme_minimal() +
  facet_grid(. ~ policy) +
  ylab("share of regime class [%]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x=element_blank(),
        legend.title = element_blank()) 
plot.regime_stack



# --------------------
# Regime 6classes
# those were added under 1_load_opt_data.R and are classified in the regimeclass.txt
# --------------------

regime_6class_prc <- df.solution %>% 
  # calculate the percentage share for each regime class according to the area
  group_by(scenario, policy, regime_6class) %>% 
  # area share by regime
  summarise( tot_area = sum(solution_area_share)) %>% 
  # add total area
  mutate(sum_tot_area = sum(tot_area)) %>% 
  # calculate prc share
  mutate(prc_share = round((tot_area / sum_tot_area)*100, digits = 2)) 


# plot the single regimes - on top
plot.regime_6class_stack <- regime_6class_prc %>% 
  ggplot(aes(x=scenario, y=prc_share) ) +
  geom_bar(aes(fill=regime_6class), position="stack", stat="identity" ) +
  theme_minimal() +
  facet_grid(. ~ policy) +
  ylab("share of regime class [%]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x=element_blank(),
        legend.title = element_blank()) 
plot.regime_6class_stack


