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
  
  # # to test the code within the loop the below line can be un-comment (means remove #)
  i = rcplist[2]
  
  # read the simulated data (input also for optimization)
  df <- read.csv(paste0(path, "/scripts_cb/sample_central_fin_", i, ".csv"), sep = ";"  ,header = TRUE)
  names(df)
  
  # filter the regimes used for ref scenario
  df <- df %>% 
    filter(regime %in% "SA" | regime %in% "BAUwT" | regime %in% "BAU" | regime %in% "BAUwoT" | regime %in% "CCF_2")
  
  # how often is each regime simulated - note - regime set aside is the only one applied to all stands
  df[,c("id", "regime")]  %>%  distinct %>% count(regime)
  
  names(df)
  
  
  # -----------
  # reference scenario representing business as usual
  # -----------
  
  # first, use stands simulated by BAUwt; second, stands simulated with BAU; all remaining stands will be SA 
  
  # get list of stands under certain regime
  bauwt_id <- unique(df[df$regime %in% "BAUwT",]$id)
  bau_id <- unique(df[df$regime %in% "BAU",]$id)
  
  # filter stands with BAUwT
  df.bauwt <- df %>%  
    filter(regime %in% "BAUwT",
           id %in% bauwt_id)
  length(unique(df.bauwt$id))
  
  # filter stands with BAU that have not been simulated with BAUwt
  df.bau <- df %>%  
    filter(regime %in% "BAU") %>% 
    filter(! id %in% bauwt_id)
  length(unique(df.bau$id))
  
  # filter stands that have not been simulated with any BAU, BAUwt regime
  df.sa <- df %>%  
    filter(regime %in% "SA") %>% 
    filter(! id %in% bauwt_id,
           ! id %in% bau_id)
  length(unique(df.sa$id))
  
  # combine all of them to reference scenario BAU
  df.refbau <- rbind(df.bauwt, df.bau, df.sa) %>% 
    mutate(policy = "refBAU") %>% 
    mutate(scenario = i) # data has already info about climate --> column scenario
  
  length(unique(df.refbau$id)) # should be 3579
  
  # -----------
  # reference scenario for Set Aside
  # -----------
  
  df.refsa <- df %>%  filter(regime %in% "SA") %>% 
    mutate(policy = "refSA") %>% 
    mutate(scenario = i) # data has already info about climate --> column scenario
  
  length(unique(df.refsa$id)) # should be 3579 
  # both data sets should have the same lenght (see right hand side - global env.)
  
  
  
  # -----------
  # reference scenario for CCF
  # -----------
  
  # get list of stands under certain regime
  CCF_2_id <- unique(df[df$regime %in% "CCF_2",]$id)
  length(CCF_2_id)
  
  # filter stands with CCF_2
  df.CCF_2 <- df %>%  
    filter(regime %in% "CCF_2",
           id %in% CCF_2_id)
  length(unique(df.CCF_2$id))
  
  # assign stands not filtered with CCF_2 to SA
  df.sa_notCCF <- df %>%  
    filter(regime %in% "SA",
           ! id %in% CCF_2_id)
  length(unique(df.sa_notCCF$id))
  
  
  # combine all of them to reference scenario CCF_2
  df.refCCF_2 <- rbind(df.CCF_2, df.sa_notCCF) %>% 
    mutate(policy = "refCCF_2") %>% 
    mutate(scenario = i) # data has already info about climate --> column scenario
  
  length(unique(df.refCCF_2$id)) # should be 3579
  
  
  
  # -----------
  # Combine all three reference scenarios for this climate
  # -----------
  
  df.ref <- rbind(df.refbau, df.refsa, df.refCCF_2)
  
  df.refall.t <- rbind(df.refall.t, df.ref)
  
  # remove the unused stuff
  rm(bau_id, bauwt_id, CCF_2_id, df.bau, df.bauwt, df.CCF_2, df.sa, df.sa_notCCF)
  rm(df.refbau, df.refsa, df.refCCF_2, df.ref)
  
}


df.refall <- df.refall.t
rm(df.refall.t)



head(df.refall)
names(df.refall)







# -----------
# Initial graphs of the data
# -----------

library(ggplot2)

# arbitrary start and end dates for comparrison
df.filteredforplot <- df.refall %>%  
  filter(year %in% c("2021"))

#
# Have I done something wrong here or when making the CCF_2 ref scenario?
#

## CB: now it works: with the new df.refall dataframe it works, plus plotting not scenario against a different variable on the y-axis

# change in management regimes
df.filteredforplot %>%
  ggplot(aes(x=scenario, y= id, fill=regime))+
  geom_bar(stat="identity")+
  facet_wrap(~ policy)


# df.filteredforplot %>%
#   ggplot(aes(x=scenario, y= scenario, fill=factor(regime)))+ # CB: you cannot plot scenario (x) against scenario (y), x and y need to be different
#   geom_bar(stat="identity")+
#   labs(fill="Year")+
#   facet_wrap(~ policy)



## CB: 
## This is one way to explore the effects on ecosystem service indicators 
## We can do this in a next step, and maybe also in new Rscript that could be called es_effect.R

# change of increment
df.filteredforplot %>%
  ggplot(aes(x=regime, y=V, fill=factor(year)))+
  geom_boxplot()+
  labs(fill="Year")+
  facet_wrap(~ scenario)

# change of carbon sink
df.filteredforplot %>%
  ggplot(aes(x=regime, y=CARBON_SINK, fill=factor(year)))+
  geom_boxplot()+
  labs(fill="Year")+
  facet_wrap(~ scenario)

#change in deadwood volume
df.filteredforplot %>%
  ggplot(aes(x=regime, y=V_total_deadwood, fill=factor(year)))+
  geom_boxplot()+
  labs(fill="Year")+
  facet_wrap(~ scenario)
