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


# read the simulated data (input also for optimization)
df.t <- read.csv(paste0(path, "/scripts_cb/sample_central_fin_rcp0.csv"), sep = ";"  ,header = TRUE, stringsAsFactors = TRUE)
names(df.t)

# filter the regimes used for ref scenario
df <- df.t %>% 
  filter(regime %in% "SA" | regime %in% "BAUwT" | regime %in% "BAU" | regime %in% "BAUwoT" | regime %in% "CCF_2") %>%
  mutate(scenario = "noCC") # indicate climate scenario, in case we will have more then current climate

# how often is each regime simulated - note - regime set aside is the only one applied to all stands
df[,c("id", "regime")]  %>%  distinct %>% count(regime)



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
df.refbau <- rbind(df.bauwt, df.bau, df.sa)

length(unique(df.refbau$id)) # should be 3579



# -----------
# reference scenario for Set Aside
# -----------

df.refsa <- df %>%  filter(regime %in% "SA")

length(unique(df.refbau$id)) # should be 3579 
# both data sets should have the same lenght (see right hand side - global env.)


