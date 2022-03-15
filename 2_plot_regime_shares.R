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
# plot management of the optimal solution of our policy scenario: !! considering the represented forest area by plot !!
# --------------------

# --------------------
# example for all single regimes 
# --------------------


# general, I recommend to look at the ggplot cheat sheet to get ideas how to plot
# this is a powerful package for making plots! https://www.rstudio.com/resources/cheatsheets/

# For data handling and aggregation I recommend the dyplr cheatsheet https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-transformation.pdf

# But of course google is also all the time a good help, but the sheets can give an idea what to look for


# calculate the percentage share for each regime and ...
single_regime_prc <- df.solution %>% 
  # group by scenario, policy, and regime ...
  group_by(scenario, policy, regime) %>% 
  # and summarize the area by regime
  # using here the "solution_area_share" which was created under 1_load_opt_data.R 'and which'
  # This is simply stand_share * by represented_area_by_NFIplot
  # reason: stand_share can be < 1, which means a stand where assigned two optimal regimes, e.g. each with a share of 0.5
  summarise( tot_area = sum(solution_area_share)) %>% 
  # calculate also the total area of the region Keski Suomi
  # Therefore group by scenario and policy
  group_by(scenario, policy) %>% 
  mutate(sum_tot_area = sum(tot_area)) %>% 
  # calculate percent share
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
# using ggsave and save it under folder "outp" and call it "plot.regime.tiff"
# when you open this folder you will see the tiff file
ggsave(plot = plot.regime, paste0(path,"/outp/plot.regime.tiff"), width=6, height=4) # wuith width and height you can specify the size of plot that it looks nice


# plot the single regimes - on top of each other, called stack
# see ggplot cheat sheet
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
# Plot regime grouped for some classes 
# --------------------

# Those were added to df.soluntion under "1_load_opt_data.R" and are classified in the "../params/regimeclass.txt"
# If you look at the .txt file you see that there two rows with classes defined, let´s used "regime_class", which groups all of our regimes bit together

# Having the classes might make it bit more transparent

# start again with the df.solution
# and also calculate first the percent shares like above but now for the class
regime_class_prc <- df.solution %>% 
  # calculate the percentage share for each regime, but now for the classes! 
  group_by(scenario, policy, regime_class) %>% 
  # get the area share by regime
  summarise( tot_area = sum(solution_area_share)) %>% 
  # add total area
  mutate(sum_tot_area = sum(tot_area)) %>% 
  # calculate prc share
  mutate(prc_share = round((tot_area / sum_tot_area)*100, digits = 2)) 


# plot the regime class - on top
plot.regime_class <- regime_class_prc %>% 
  ggplot(aes(x=scenario, y=prc_share) ) +
  geom_bar(aes(fill=regime_class), position="stack", stat="identity" ) +
  theme_minimal() +
  facet_grid(. ~ policy) +
  ylab("share of regime class [%]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x=element_blank(),
        legend.title = element_blank()) 
plot.regime_class

# now we can also save it
ggsave(plot = plot.regime_class, paste0(path,"/outp/plot.regime_class.tiff"), width=4, height=4)

# The current color schemes are bit strong... but there are also other colour schemes.
# If you google for "ggplot adjust color" you can find for example ways how to adapt it; 



#########################################################################################################
# NEXT step: Plot policy scenario together reference scenarios (df.refall)
#########################################################################################################

# Lets get a stacked management plot, where the four scenarios are next to each other

# There for the two data frames df.solution and df.refall need to be combine (rbind)
# But this requires that they have the same columns with identical names

names(df.solution)
names(df.refall)

unique(df.solution$scenario)
unique(df.refall$scenario)

# 1) take from df.refall only the columns that we need (id, year, regime, policy, scenario, represented_area_by_NFIplot, and so on), works with "select"
# from df.refall we also want to keep only one year, since the management is constant over time
# df.all_scenario <- df.refall %>%  select() %>% filter(year ...)

df.all_scenario <- df.refall %>% select(id, year, regime, policy, scenario, regime, represented_area_by_NFIplot)
df.all_scenario <- filter(df.all_scenario, year == "2021")


# 2) create a column called "stand_share", using "mutate" and give it the value of 1 since under the reference stands are fully managed by one regime

df.all_scenario <- mutate(df.all_scenario, stand_share = 1)


# create a column called solution area share, with "mutate", see e.g. under script 1-load_opt_data.R around line 149

df.all_scenario <- mutate(df.all_scenario, solution_area_share = round(stand_share * represented_area_by_NFIplot, digits=0))


# column year can now also be ignored since this is not in df.solution

df.all_scenario <- select(df.all_scenario,-c(year))


# relocating column before rbind

df.all_scenario <- df.all_scenario %>%
  relocate(stand_share, .after = regime) %>%
  mutate(regime_6class = regime)%>%
  relocate(regime_6class, .after = scenario) %>%
  mutate(regime_class = regime)%>%
  relocate(regime_class, .after = regime_6class)


# when you are having the same columns in both sets you can combine them with rbind

df.all_scenario <- rbind(df.all_scenario, df.solution)


# Afterwards you calculate also the percentage shares and plot them, e.g. like above form line 93 onward with the stacked version. 


# calculation of percent shares
regime_class_prc <- df.all_scenario %>% 
  # percentage share for each regime, but now for the classes! 
  group_by(scenario, policy, regime_class) %>% 
  # area share by regime
  summarise( tot_area = sum(solution_area_share)) %>% 
  # add total area
  mutate(sum_tot_area = sum(tot_area)) %>% 
  # calculate prc share
  mutate(prc_share = round((tot_area / sum_tot_area)*100, digits = 2)) 

# plot of regime class
plot.regime_class_all_scenario <- regime_class_prc %>% 
  ggplot(aes(x=scenario, y=prc_share) ) +
  geom_bar(aes(fill=regime_class), position="stack", stat="identity" ) +
  theme_minimal() +
  facet_grid(. ~ policy) +
  ylab("share of regime class [%]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x=element_blank(),
        legend.title = element_blank()) 
plot.regime_class_all_scenario

# save the plot
ggsave(plot = plot.regime_class_all_scenario, paste0(path,"/outp/plot.regime_class_all_scenario.tiff"), width=4, height=4)
