####
#
# Plot some indicators of Central Finland
# 2022-01-17
#
####


library(dplyr)
library(ggplot2)


# get directory
path <- getwd()


# read data
df <- read.csv(paste0(path, "/scripts_cb/solutionAlldata_EUFS_rcp0_V2woF.csv"), header = TRUE, sep= ";")

head(df)
summary(df)

# add colum indicating climate
df <- df %>% mutate(climate = "RCP0")

head(df)

# which years and regimes
unique(df$year)
unique(df$regime)

# Age of stands at the beginning of simulation (might become inportant for protection target)
df %>% filter(regime %in% "initial_state") %>% 
  ggplot(aes(x=AGE_ba)) + 
  geom_density()

# calculate mean value for each year and plot it for three regime (set aside, BAU and CCF)
df %>% filter(regime %in% c("SA", "BAU", "CCF_2")) %>% 
  group_by(year, regime) %>% 
  summarise(mean_age = mean(Age)) %>% 
  ggplot(aes(year, mean_age)) +
  geom_line( aes(linetype = regime))
  
  
