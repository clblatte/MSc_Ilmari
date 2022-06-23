#####
#
# Create reference scenario
# 2022-02-09
#
#####



# get working direction
path <- getwd()


### load libraries
library(dplyr)
library(tidyr)
library(openxlsx)



# Below is shown how to generate the data for the reference scenario.
# This could also be done for a scenario representing CCF.
# Later we might also think about to have this for different climate scenarios.




# -----------
# Data - for reference scenario 
# -----------

# I put this in a for loop since the previous was probably causing the some error.


# for those two climate scenarios
rcplist <-  c("rcp0", "rcp45")


df.refall.t <- NULL

# for each of them do the below...
for(i in rcplist){
  
  # # # to test the code within the loop the below line can be un-comment (means remove #)
  # i = rcplist[2]
  
  # read the simulated data (input also for optimization)
  df.t <- read.csv(paste0(path, "/scripts_cb/sample_central_fin_", i, ".csv"), sep = ";"  ,header = TRUE)
  names(df.t)
  
  # filter the regimes used for ref scenario
  df <- df.t %>% 
    filter(regime %in% "SA" | regime %in% "BAUwT" | regime %in% "BAU" | regime %in% "BAUwoT" | regime %in% "CCF_2") # 
  
  # how often is each regime simulated - note - regime set aside is the only one applied to all stands
  df[,c("id", "regime")]  %>%  distinct %>% count(regime)
  
  
  # -----------
  # reference scenario for Set Aside
  # -----------
  
  df.refsa <- df %>%  filter(regime %in% "SA") %>% 
    mutate(policy = "refSA") %>% 
    mutate(scenario = i) # data has already info about climate --> column scenario
  
  length(unique(df.refsa$id)) # should be 3579 
  # both data sets should have the same lenght (see right hand side - global env.)
  
  
  # -----------
  # Combine all three reference scenarios for this climate
  # -----------
  
  df.refall.t <- rbind(df.refall.t, df.refsa)
  
}


df.refall <- df.refall.t
rm(df.refall.t)


head(df.refall)
names(df.refall)


# reference scenario over time which will be combined with df.solution_alldata

# Info to group the regimes according to 6 management classes as in Blattert et al. 2022 
regimeclass <- read.csv(paste0(path, "/params/regimeclass.txt"), sep = ";"  ,header = TRUE, stringsAsFactors = TRUE)


df.refall <- df.refall %>% 
  # merge the regime classes
  left_join(regimeclass, by = "regime") %>% 
  mutate(stand_share = 1) %>% 
  mutate(solution_area_share = round(stand_share * represented_area_by_NFIplot, digits=0)) %>% 
  rename(V_deadwood = V_total_deadwood)

# reference scenario which will be combined with df.solution of the opt output

df.refallsolution <- df.refall %>% 
  filter(year %in% 2021) %>% 
  select(id, regime, stand_share, policy, scenario, regime_6class, regime_class, represented_area_by_NFIplot, solution_area_share )





# # -----------
# # Initial graphs of the data
# # -----------
# 
# library(ggplot2)
# 
# # arbitrary start and end dates for comparrison
# df.filteredforplot <- df.refall %>%  
#   filter(year %in% c("2021"))
# 
# #
# # Have I done something wrong here or when making the CCF_2 ref scenario?
# #
# 
# ## CB: now it works: with the new df.refall dataframe it works, plus plotting not scenario against a different variable on the y-axis
# 
# # change in management regimes
# df.filteredforplot %>%
#   ggplot(aes(x=scenario, y= id, fill=regime))+
#   geom_bar(stat="identity")+
#   facet_wrap(~ policy)
# 
# 
# # df.filteredforplot %>%
# #   ggplot(aes(x=scenario, y= scenario, fill=factor(regime)))+ # CB: you cannot plot scenario (x) against scenario (y), x and y need to be different
# #   geom_bar(stat="identity")+
# #   labs(fill="Year")+
# #   facet_wrap(~ policy)
# 
# 
# 
# ## CB: 
# ## This is one way to explore the effects on ecosystem service indicators 
# ## We can do this in a next step, and maybe also in new Rscript that could be called es_effect.R
# 
# # change of increment
# df.filteredforplot %>%
#   ggplot(aes(x=regime, y=V, fill=factor(year)))+
#   geom_boxplot()+
#   labs(fill="Year")+
#   facet_wrap(~ scenario)
# 
# # change of carbon sink
# df.filteredforplot %>%
#   ggplot(aes(x=regime, y=CARBON_SINK, fill=factor(year)))+
#   geom_boxplot()+
#   labs(fill="Year")+
#   facet_wrap(~ scenario)
# 
# #change in deadwood volume
# df.filteredforplot %>%
#   ggplot(aes(x=regime, y=V_total_deadwood, fill=factor(year)))+
#   geom_boxplot()+
#   labs(fill="Year")+
#   facet_wrap(~ scenario)
