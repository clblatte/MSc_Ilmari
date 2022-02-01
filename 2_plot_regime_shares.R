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

single_regime_prc <- df.solution %>% 
  # calculate the percentage share for each regime according to the area
  group_by(scenario, policy, regime) %>% 
  # area share by regime
  summarise( tot_area = sum(solution_area_share)) %>% 
  # add total area
  group_by(scenario, policy) %>% 
  mutate(sum_tot_area = sum(tot_area)) %>% 
  # calculate prc share
  mutate(prc_share = round((tot_area / sum_tot_area)*100, digits = 1) ) %>% 
  ungroup()


plot.single_regime <- single_regime_prc %>%   
  ggplot(aes(x=regime, y=prc_share) ) +
  geom_bar(aes(fill=policy), position="dodge", stat="identity" )+
  ylab("regime share [%]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x=element_blank()) +
  scale_y_continuous(limits=c(0, 45)) +
  scale_fill_brewer(palette="Set3") 
plot.single_regime



# --------------------
# Regime 6classes
# --------------------

regime_6class_prc <- df.solution %>% 
  # calculate the percentage share for each regime class according to the area
  group_by(scenario, policy, regime_6class) %>% 
  # area share by regime
  summarise( tot_area = sum(solution_area_share)) %>% 
  # add total area
  mutate(sum_tot_area = sum(tot_area)) %>% 
  # calculate prc share
  mutate(prc_share = round((tot_area / sum_tot_area)*100, digits = 2)) %>% 
  ungroup()


plot.regime_6class <- regime_6class_prc %>% 
  ggplot(aes(x=regime_6class, y=prc_share) ) +
  geom_bar(aes(fill=policy), position="dodge", stat="identity" ) +
  theme_minimal() +
  ylab("percent [%]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1),
        axis.title.x=element_blank()) 
plot.regime_6class

